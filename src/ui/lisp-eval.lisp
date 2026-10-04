;;;; lisp-eval.lisp — M2 commands that use the connected Lisp: evaluating,
;;;; compiling, finding definitions, documentation, completion, and argument
;;;; hints in the status bar

(in-package #:cadre-ui)

(defun view-package (view)
  "The package for code in VIEW at the cursor: the buffer's own (as for
*Macroexpansion*), else from its IN-PACKAGE, else the REPL's."
  (let ((syntax (buffer-syntax (view-buffer view))))
    (or (buffer-local (view-buffer view) :package)
        (and syntax (buffer-package-name syntax (cursor-line-column view)))
        (if (connected-p) (connection-package *connection*) "COMMON-LISP-USER"))))

(defun view-region-text (view line column end-line end-column)
  (let ((gtk-buffer (view-gtk-buffer view)))
    (gtk:text-buffer-get-text gtk-buffer (line-iter gtk-buffer line column)
                              (line-iter gtk-buffer end-line end-column) t)))

(defun flash-region (view line column end-line end-column)
  "Highlight a region briefly, to show what was evaluated or compiled."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (tag (face-tag gtk-buffer :search)))
    (when tag
      (gtk:text-buffer-apply-tag gtk-buffer tag (line-iter gtk-buffer line column) (line-iter gtk-buffer end-line end-column))
      (glib:timeout-add glib:+priority-default+ 300
                        (lambda ()
                          (gtk:text-buffer-remove-tag gtk-buffer tag (line-iter gtk-buffer line column)
                                                      (line-iter gtk-buffer end-line end-column))
                          nil)))))

(defun show-result (result)
  (message "~a" (string-right-trim '(#\Newline) result)))

;;; Results shown inline, after the form: "⇒ 42". One per buffer; it goes
;;; away when the buffer changes.

(defun result-text (result)
  "Swank's \"=> 42 (6 bits, …)\" as \"42 (6 bits, …)\", on one line."
  (let ((text (string-trim '(#\Newline #\Space) result)))
    (substitute #\Space #\Newline (if (and (> (length text) 3) (string= "=> " text :end2 3))
                                     (subseq text 3)
                                     text))))

(defun clear-inline-result (buffer)
  (let ((shown (buffer-local buffer :inline-result)))
    (when shown
      (setf (buffer-local buffer :inline-result) nil)
      (destructuring-bind (text-view . label) shown
        ;; GTK keeps overlay children in a container of its own; removing
        ;; one doesn't always take it off the screen, so hide it too.
        (gtk:widget-set-visible label nil)
        (when (gtk:widget-get-parent label)
          (ignore-errors (gtk:text-view-remove text-view label)))))))

(defun show-inline-result (view line result)
  "Show RESULT after the end of LINE in VIEW, until the buffer changes."
  (let* ((buffer (view-buffer view))
         (text-view (view-text-view view))
         (rect (gtk:text-view-get-iter-location text-view (line-end-iter (view-gtk-buffer view) line)))
         (text (result-text result))
         ;; Overlay children get their minimum width, so the label must not
         ;; ellipsize; long values are shortened here instead.
         (label (make-instance 'gtk:label :label (format nil "⇒ ~a" (if (> (length text) 120)
                                                                          (concatenate 'string (subseq text 0 119) "…")
                                                                          text))
                                          :tooltip-text (string-trim '(#\Newline) result)
                                          :css-classes '("cadre-inline-result"))))
    (clear-inline-result buffer)
    (gtk:text-view-add-overlay text-view label (+ (gdk:rectangle-x rect) 16) (gdk:rectangle-y rect))
    (setf (buffer-local buffer :inline-result) (cons text-view label))))

(defun eval-for-message (string package &optional view line)
  "Evaluate STRING; show the value in the status bar and, with VIEW, after LINE."
  (with-connection (connection)
    (rex connection (swank-call "swank:interactive-eval" (gtk-thread-source string) 3 120) :package package
         :on-ok (lambda (result)
                  (show-result result)
                  (image-changed)
                  (when (and view (member (view-buffer view) (buffer-list)))
                    (show-inline-result view line result))))))

;;; Evaluating

(define-command eval-last-expression ()
  "Evaluate the expression before the cursor and show its value."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc) (backward-sexp-position (current-syntax) line column)
        (unless sl (editor-error "No expression before the cursor"))
        (flash-region view sl sc line column)
        (eval-for-message (view-region-text view sl sc line column) (view-package view) view line)))))

(define-command eval-defun ()
  "Evaluate the top-level form around the cursor and show its value."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc el ec) (toplevel-form-bounds (current-syntax) line column)
        (unless sl (editor-error "Not in a top-level form"))
        (flash-region view sl sc el ec)
        (eval-for-message (view-region-text view sl sc el ec) (view-package view) view el)))))

(define-command eval-region ()
  "Evaluate the selected text."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (unless has (editor-error "Nothing is selected"))
      (let ((text (gtk:text-buffer-get-text gtk-buffer start end t))
            (package (view-package view)))
        (with-connection (connection)
          (rex connection (swank-call "swank:interactive-eval-region" (gtk-thread-source text) 3 120) :package package
               :on-ok (lambda (result) (show-result result) (image-changed))))))))

(define-command eval-expression-or-region ()
  "Evaluate the selection if there is one, else the expression before the cursor."
  (:modes lisp-mode)
  (if (gtk:text-buffer-get-has-selection (view-gtk-buffer (current-view)))
      (eval-region)
      (eval-last-expression)))

;;; Compiling

(defun compilation-message (notes successp duration what)
  (let ((errors (count 0 notes :key (lambda (n) (severity-rank (compiler-note-severity n)))))
        (warnings (count 1 notes :key (lambda (n) (severity-rank (compiler-note-severity n)))))
        (others (count-if (lambda (n) (> (severity-rank (compiler-note-severity n)) 1)) notes)))
    (message "~a~:[ failed~;~]~@[: ~a~] (~,2f s)" what successp
             (and notes (format nil "~@[~d error~:p~]~@[~*, ~]~@[~d warning~:p~]~@[~*, ~]~@[~d note~:p~]"
                                (and (plusp errors) errors) (and (plusp errors) (plusp (+ warnings others)))
                                (and (plusp warnings) warnings) (and (plusp warnings) (plusp others))
                                (and (plusp others) others)))
             (or duration 0))))

(defun compile-toplevel-form (&key policy (what "Compiled") then)
  "Compile the top-level form around the cursor, with POLICY (an alist such
as ((cl:debug . 3))) if given; show the compiler's notes, then call THEN
with whether it compiled."
  (let* ((view (current-view))
         (buffer (view-buffer view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc el ec) (toplevel-form-bounds (current-syntax) line column)
        (unless sl (editor-error "Not in a top-level form"))
        (flash-region view sl sc el ec)
        (let* ((text (view-region-text view sl sc el ec))
               (position (1+ (text-line-position (buffer-text buffer) sl sc)))
               (package (view-package view)))
          (with-connection (connection)
            (rex connection
                 (swank-call "swank:compile-string-for-emacs" text (buffer-name buffer)
                             (list (list :position position) (list :line (1+ sl) (1+ sc)))
                             (and (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)))
                             policy)
                 :package package
                 :on-ok (lambda (result)
                          (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                            (show-notes notes :buffer buffer :replace-lines (cons sl el))
                            (compilation-message notes successp duration what)
                            (image-changed)
                            (when then (funcall then successp)))))))))))

(define-command compile-defun ()
  "Compile the top-level form around the cursor; show the compiler's notes."
  (:modes lisp-mode)
  (compile-toplevel-form))

(defun definition-form-p (syntax line column)
  "True if the top-level form at (LINE, COLUMN) is a definition: its head is DEF… or DEFINE-…."
  (multiple-value-bind (sl sc) (toplevel-form-bounds syntax line column)
    (when sl
      (let ((head (symbol-at syntax sl (1+ sc))))
        (and head (cadre::definer-name-p head))))))

(define-command compile-or-eval-defun ()
  "Compile the top-level form if it is a definition (showing compiler notes);
otherwise evaluate it and show its value."
  (:modes lisp-mode)
  (multiple-value-bind (line column) (cursor-line-column (current-view))
    (if (definition-form-p (current-syntax) line column)
        (compile-defun)
        (eval-defun))))

(define-command compile-and-load-file ()
  "Save the file, compile it and load the result; show the compiler's notes."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (buffer (view-buffer view)))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (flet ((compile-it ()
             (with-connection (connection)
               (rex connection (swank-call "swank:compile-file-for-emacs"
                                           (uiop:native-namestring (buffer-file buffer)) t)
                    :on-ok (lambda (result)
                             (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                               (show-notes notes :buffer buffer :replace-lines :all)
                               (compilation-message notes successp duration
                                                    (format nil "Compiled ~a" (buffer-name buffer)))
                               ;; Compiling only compiles; load the fasl if it worked.
                               (let ((loadp (fifth result)) (fasl (sixth result)))
                                 (when (and successp loadp fasl)
                                   (rex connection (swank-call "swank:load-file" fasl)
                                        :on-ok (lambda (v) (declare (ignore v)) (image-changed)))))))))))
      (if (buffer-modified-p buffer)
          (write-buffer buffer (buffer-file buffer) (lambda (ok) (when ok (compile-it))))
          (compile-it)))))

(define-command load-file ()
  "Load the current file's source into the Lisp."
  (:modes lisp-mode)
  (let ((buffer (view-buffer (current-view))))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (with-connection (connection)
      (rex connection (swank-call "swank:load-file" (uiop:native-namestring (buffer-file buffer)))
           :on-ok (lambda (v) (declare (ignore v)) (message "Loaded ~a" (buffer-name buffer)) (image-changed))))))

;;; Definitions

(defvar *definition-stack* '() "Where M-. came from: (buffer . offset) pairs.")

(defun push-position (view)
  (push (cons (view-buffer view) (text-point (view-gtk-buffer view))) *definition-stack*))

(defun goto-location (location &key then)
  "Show LOCATION (from parse-location) in a tab; then call THEN with its view."
  (flet ((visit (view)
           (let ((gtk-buffer (view-gtk-buffer view)))
             (cond ((getf location :position)
                    (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (min (getf location :position)
                                                                                     (text-length gtk-buffer)))))
                   ((getf location :line)
                    (goto-line-column view (getf location :line) (or (getf location :column) 0) :extend nil)))
             (scroll-to-cursor view)
             (focus-view view)
             (when then (funcall then view)))))
    (cond ((getf location :error) (editor-error "~a" (getf location :error)))
          ((and (getf location :buffer) (find-buffer (getf location :buffer)))
           (visit (show-buffer *window* (find-buffer (getf location :buffer)))))
          ((getf location :file) (open-file-path (pathname (getf location :file)) :then #'visit))
          (t (editor-error "Cannot show this location")))))

(defun source-definition-choices (name buffer)
  "NAME's definitions in the source (open buffers and project files), as
(label location) like Swank's."
  (loop for (definition . place) in (find-source-definitions name buffer)
        collect (list (format nil "(~(~a~) ~a)" (kind-name (definition-kind definition)) (definition-name definition))
                      (if (pathnamep place)
                          (list :location (list :file (uiop:native-namestring place)) (list :line (definition-line definition)))
                          (list :location (list :buffer (buffer-name place)) (list :line (definition-line definition)))))))

(defun go-to-definition-choices (view name choices)
  "Go to the one definition among CHOICES ((label location) lists), or let the user pick."
  (flet ((visit (choice)
           (push-position view)
           (goto-location (parse-location (second choice)))))
    (if (null (rest choices))
        (visit (first choices))
        (open-picker (window-picker *window*)
                     :items choices :label #'first
                     :detail (lambda (d) (let* ((l (parse-location (second d)))
                                                (f (or (getf l :file) (getf l :buffer))))
                                           (if f (file-namestring f) "")))
                     :placeholder (format nil "Definitions of ~a" name)
                     :on-choose #'visit))))

(define-command edit-definition ()
  "Go to the definition of the symbol at the cursor (M-, comes back). With
no Lisp running, or if it doesn't know the symbol, the source's own
definitions are used."
  (let* ((view (current-view))
         (syntax (buffer-syntax (view-buffer view)))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c)))))
    (unless name (editor-error "No symbol at the cursor"))
    (let ((package (view-package view))
          (source (source-definition-choices name (view-buffer view))))
      (if (not (connected-p))
          (if source
              (go-to-definition-choices view name source)
              (message "No definition of ~a in the source (start a Lisp to look in loaded code)" name))
      (with-connection (connection)
        (rex connection (swank-call "swank:find-definitions-for-emacs" name) :package package
             :on-ok (lambda (definitions)
                      (let ((found (remove-if (lambda (d) (getf (parse-location (second d)) :error)) definitions)))
                        (cond ((and (null found) source) (go-to-definition-choices view name source))
                              ((null definitions) (message "No definition found for ~a" name))
                              ((null found) (message "~a" (getf (parse-location (second (first definitions))) :error)))
                              ((null (rest found))
                               (push-position view)
                               (goto-location (parse-location (second (first found)))))
                              (t (open-picker (window-picker *window*)
                                              :items found :label #'first
                                              :detail (lambda (d) (let ((f (getf (parse-location (second d)) :file)))
                                                                    (if f (file-namestring f) "")))
                                              :placeholder (format nil "Definitions of ~a" name)
                                              :on-choose (lambda (d)
                                                           (push-position view)
                                                           (goto-location (parse-location (second d)))))))))))))))

(define-command pop-definition ()
  "Go back to where the last M-. started."
  (let ((place (or (pop *definition-stack*) (editor-error "No earlier position"))))
    (destructuring-bind (buffer . offset) place
      (unless (member buffer (buffer-list)) (editor-error "That buffer was closed"))
      (let ((view (show-buffer *window* buffer)))
        (gtk:text-buffer-place-cursor (view-gtk-buffer view) (iter-at (view-gtk-buffer view) offset))
        (scroll-to-cursor view)))))

;;; Documentation

(defun show-help (title text)
  "Show TEXT in a read-only *Help* tab."
  (let ((buffer (or (find-buffer "*Help*")
                    (make-buffer :name "*Help*" :text (make-gtk-text)))))
    (text-replace-contents (buffer-text buffer) (format nil "~a~%~%~a" title text))
    (let ((view (show-buffer *window* buffer)))
      (gtk:text-view-set-editable (view-text-view view) nil)
      (gtk:text-view-set-wrap-mode (view-text-view view) :word-char))))

(define-command describe-symbol ()
  "Describe the symbol at the cursor."
  (let* ((view (current-view))
         (syntax (buffer-syntax (view-buffer view)))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c)))))
    (unless name (editor-error "No symbol at the cursor"))
    (let ((package (view-package view)))
      (with-connection (connection)
        (rex connection (swank-call "swank:describe-symbol" name) :package package
             :on-ok (lambda (text) (show-help name text)))))))

;;; Argument hints

(defvar *autodoc-timer* nil)
(defvar *autodoc-cache* (make-hash-table :test 'equal))

(defun autodoc-markup (string)
  "STRING with ===> x <=== shown in bold, as Pango markup."
  (let ((escaped (glib:markup-escape-text string -1)))
    (loop for start = (search "===&gt; " escaped)
          while start
          do (let ((end (search " &lt;===" escaped :start2 start)))
               (unless end (return))
               (setf escaped (concatenate 'string (subseq escaped 0 start) "<b>"
                                          (subseq escaped (+ start 8) end) "</b>"
                                          (subseq escaped (+ end 8))))))
    escaped))

(defun show-arglist (string)
  (let ((label (window-status-arglist *window*)))
    (if string
        (gtk:label-set-markup label (autodoc-markup (substitute #\Space #\Newline string)))
        (gtk:label-set-text label ""))))

(defun schedule-autodoc (view)
  "Ask for the arglist at VIEW's cursor shortly, if the cursor stays put."
  (when *autodoc-timer* (glib:source-remove *autodoc-timer*))
  (setf *autodoc-timer*
        (glib:timeout-add glib:+priority-default+ 250
                          (lambda () (setf *autodoc-timer* nil) (request-autodoc view) nil))))

(defun request-autodoc (view)
  "Show the arglist of the call around VIEW's cursor: from the source if it
defines the function, else from the connected Lisp, else from standard
Common Lisp."
  (let ((syntax (buffer-syntax (view-buffer view))))
    (if (not (and syntax (eq view (focused-view *window*))))
        (show-arglist nil)
        (multiple-value-bind (line column) (cursor-line-column view)
          (let* ((form (and (eq (context-at syntax line column) :code) (raw-form-at syntax line column)))
                 (key (and form (sexp-to-string form)))
                 (source (and form (ignore-errors (local-autodoc view syntax line column :source-only t)))))
            (cond ((null form) (show-arglist nil))
                  (source (show-arglist source))
                  ((not (connected-p)) (show-arglist (ignore-errors (local-autodoc view syntax line column))))
                  ((gethash key *autodoc-cache*) (show-arglist (gethash key *autodoc-cache*)))
                  (t (rex *connection* (swank-call "swank:autodoc" form :print-right-margin 200)
                          :package (view-package view)
                          :on-ok (lambda (result)
                                   (let ((doc (and (consp result) (stringp (first result)) (first result))))
                                     (when (and doc (second result)) (setf (gethash key *autodoc-cache*) doc))
                                     (show-arglist (or doc (ignore-errors (local-autodoc view syntax line column))))))
                          :on-abort (lambda (reason) (declare (ignore reason)) (show-arglist nil))))))))))

;;; Completion

(defun prefix-before-cursor (view)
  "The symbol characters just before VIEW's cursor, and where they start (an offset)."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (end (text-point gtk-buffer))
         (start end))
    (loop while (and (> start 0)
                     (let ((c (char (gtk:text-buffer-get-text gtk-buffer (iter-at gtk-buffer (1- start)) (iter-at gtk-buffer start) t) 0)))
                       (not (terminating-char-p c))))
          do (decf start))
    (values (gtk:text-buffer-get-text gtk-buffer (iter-at gtk-buffer start) (iter-at gtk-buffer end) t) start)))

(defun completion-kind (flags)
  "A word for Swank's completion flags string, such as \"-f------\", or
for a kind from hints.lisp, such as :function."
  (cond ((keywordp flags) (kind-name flags))
        ((not (stringp flags)) "")
        ((find #\m flags) "macro")
        ((find #\s flags) "special")
        ((find #\g flags) "generic")
        ((find #\f flags) "function")
        ((find #\c flags) "class")
        ((find #\b flags) "variable")
        ((find #\p flags) "package")
        (t "")))

(defun replace-prefix (view start text)
  (let ((gtk-buffer (view-gtk-buffer view)))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer start) (cursor-iter gtk-buffer))
      (gtk:text-buffer-insert-at-cursor gtk-buffer text -1))))

(define-command complete-symbol ()
  "Complete the symbol before the cursor, from the source, standard Common
Lisp and, if one is connected, the Lisp."
  (start-completion (current-view)))
