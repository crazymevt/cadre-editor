;;;; lisp-eval.lisp — M2 commands that use the connected Lisp: evaluating,
;;;; compiling, finding definitions, documentation, completion, and argument
;;;; hints in the status bar

(in-package #:cadre-ui)

(defun view-package (view)
  "The package for code in VIEW at the cursor: the buffer's own (as for
*Macroexpansion*), else from its IN-PACKAGE (in Clojure, its ns; in
Racket, its file's module), else the REPL's."
  (case (view-language view)
    (:clojure
     (let ((syntax (buffer-sexp-syntax (view-buffer view)))
           (connection (language-connection :clojure)))
       (or (buffer-local (view-buffer view) :package)
           (and syntax (clojure-namespace-at syntax (cursor-line-column view)))
           (and connection (connection-package connection))
           "user")))
    (:racket
     ;; nil: the REPL's module.
     (let ((file (buffer-file (view-buffer view))))
       (or (buffer-local (view-buffer view) :package)
           (and file (not (eq (buffer-major-mode (view-buffer view)) 'racket-repl-mode))
                (uiop:native-namestring file)))))
    (t
     (let ((syntax (buffer-syntax (view-buffer view))))
       (or (buffer-local (view-buffer view) :package)
           (and syntax (buffer-package-name syntax (cursor-line-column view)))
           (if (connected-p) (connection-package *connection*) "COMMON-LISP-USER"))))))

(defun racket-place-keys (view)
  "For a Racket file, the :text and :offset keys Racket's requests take for
the identifier at VIEW's cursor (check-syntax reads the buffer's text)."
  (when (and view (eq (view-language view) :racket) (buffer-file (view-buffer view)))
    (list :text (text-string (buffer-text (view-buffer view)))
          :offset (text-point (view-gtk-buffer view)))))

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

(defvar *inline-result-labels* (make-hash-table :test 'eq :weakness :key)
  "Text view → (label . buffer): its inline result label, used again for
each result (gtk_text_view_remove leaves an overlay child in the text view),
and the buffer whose result it shows.")

(defun clear-inline-result (buffer)
  (let ((shown (buffer-local buffer :inline-result)))
    (when shown
      (setf (buffer-local buffer :inline-result) nil)
      (gtk:widget-set-visible (cdr shown) nil))))

(defun inline-result-label (text-view buffer)
  "TEXT-VIEW's inline result label, made the first time, now BUFFER's."
  (let ((entry (gethash text-view *inline-result-labels*)))
    (cond ((null entry)
           (let ((label (make-instance 'gtk:label :css-classes '("cadre-inline-result") :visible nil)))
             (gtk:text-view-add-overlay text-view label 0 0)
             (setf entry (setf (gethash text-view *inline-result-labels*) (cons label buffer)))))
          ((not (eq buffer (cdr entry)))
           ;; It showed another buffer's result, before the view switched to BUFFER.
           (let ((owner (cdr entry)))
             (when (eq (car entry) (cdr (buffer-local owner :inline-result)))
               (clear-inline-result owner)))
           (setf (cdr entry) buffer)))
    (car entry)))

(defun show-inline-result (view line result)
  "Show RESULT after the end of LINE in VIEW, until the buffer changes."
  (let* ((buffer (view-buffer view))
         (text-view (view-text-view view))
         (rect (gtk:text-view-get-iter-location text-view (line-end-iter (view-gtk-buffer view) line)))
         (text (result-text result)))
    (clear-inline-result buffer)
    (let ((label (inline-result-label text-view buffer)))
      ;; Overlay children get their minimum width, so the label must not
      ;; ellipsize; long values are shortened here instead.
      (gtk:label-set-label label (format nil "⇒ ~a" (if (> (length text) 120)
                                                        (concatenate 'string (subseq text 0 119) "…")
                                                        text)))
      (gtk:widget-set-tooltip-text label (string-trim '(#\Newline) result))
      (gtk:text-view-move-overlay text-view label (+ (gdk:rectangle-x rect) 16) (gdk:rectangle-y rect))
      (gtk:widget-set-visible label t)
      (setf (buffer-local buffer :inline-result) (cons text-view label)))))

(defun eval-for-message (string package &optional view line)
  "Evaluate STRING; show the value in the status bar and, with VIEW, after LINE."
  (with-connection (connection :language (view-language view))
    (when view (follow-package connection package))
    (call-in-file-namespace
     view connection
     (lambda ()
       (request #'backend-eval connection (evaluable-source connection string) :package package
                :on-ok (lambda (result)
                         (show-result result)
                         (image-changed)
                         (when (and view (member (view-buffer view) (buffer-list)))
                           (show-inline-result view line result))))))))

;;; Evaluating

(define-command eval-last-expression ()
  "Evaluate the expression before the cursor and show its value."
  (:modes lisp-mode racket-mode clojure-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc) (backward-sexp-position (current-syntax) line column)
        (unless sl (editor-error "No expression before the cursor"))
        (flash-region view sl sc line column)
        (eval-for-message (view-region-text view sl sc line column) (view-package view) view line)))))

(define-command eval-defun ()
  "Evaluate the top-level form around the cursor and show its value."
  (:modes lisp-mode racket-mode clojure-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc el ec) (toplevel-form-bounds (current-syntax) line column)
        (unless sl (editor-error "Not in a top-level form"))
        (flash-region view sl sc el ec)
        (eval-for-message (view-region-text view sl sc el ec) (view-package view) view el)))))

(define-command eval-region ()
  "Evaluate the selected text."
  (:modes lisp-mode racket-mode clojure-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (unless has (editor-error "Nothing is selected"))
      (let ((text (gtk:text-buffer-get-text gtk-buffer start end t))
            (package (view-package view)))
        (with-connection (connection :language (view-language view))
          (follow-package connection package)
          (call-in-file-namespace
           view connection
           (lambda ()
             (request #'backend-eval-region connection (evaluable-source connection text) :package package
                      :on-ok (lambda (result) (show-result result) (image-changed))))))))))

(define-command eval-expression-or-region ()
  "Evaluate the selection if there is one, else the expression before the cursor."
  (:modes lisp-mode racket-mode clojure-mode)
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
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc el ec) (toplevel-form-bounds (current-syntax) line column)
        (unless sl (editor-error "Not in a top-level form"))
        (flash-region view sl sc el ec)
        (compile-form-region view sl sc el ec :policy policy :what what :then then)))))

(defun compile-form-region (view sl sc el ec &key policy (what "Compiled") then)
  "Compile the form from (SL, SC) to (EL, EC) in VIEW, with its breakpoints
in (breakpoints.lisp); show the compiler's notes and WHAT, then call THEN
with whether it compiled."
  (let* ((buffer (view-buffer view))
         (syntax (buffer-syntax buffer))
         (text (breakpoint-form-text buffer sl sc el ec (view-region-text view sl sc el ec)))
         (position (1+ (text-line-position (buffer-text buffer) sl sc)))
         (package (or (and syntax (not (buffer-local buffer :package)) (buffer-package-name syntax sl))
                      (view-package view))))
    (with-connection (connection :language (view-language view))
      (follow-package connection package)
      (call-in-file-namespace
       view connection
       (lambda ()
         (request #'backend-compile-form connection text
                  :buffer-name (buffer-name buffer) :position position :line (1+ sl) :column (1+ sc)
                  :file (and (buffer-file buffer) (uiop:native-namestring (buffer-file buffer)))
                  :policy policy
                  :package package
                  :on-ok (lambda (result)
                           (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                             (let ((notes (breakpoint-unshift-notes buffer notes)))
                               (show-notes notes :buffer buffer :replace-lines (cons sl el))
                               (when what (compilation-message notes successp duration what)))
                             (image-changed)
                             (when then (funcall then successp))))))))))

(define-command compile-defun ()
  "Compile the top-level form around the cursor; show the compiler's notes."
  (:modes lisp-mode racket-mode clojure-mode)
  (compile-toplevel-form))

(defun definition-form-p (syntax line column)
  "True if the top-level form at (LINE, COLUMN) is a definition: its head
defines something, as the buffer's Lisp sees it (DEF… in Common Lisp)."
  (multiple-value-bind (sl sc) (toplevel-form-bounds syntax line column)
    (when sl
      (let ((head (symbol-at syntax sl (1+ sc))))
        (and head (funcall (cadre::dialect-definer (syntax-dialect syntax)) head) t)))))

(define-command compile-or-eval-defun ()
  "Compile the top-level form if it is a definition (showing compiler notes);
otherwise evaluate it and show its value."
  (:modes lisp-mode racket-mode clojure-mode)
  (multiple-value-bind (line column) (cursor-line-column (current-view))
    (if (definition-form-p (current-syntax) line column)
        (compile-defun)
        (eval-defun))))

(define-command compile-and-load-file ()
  "Save the file, compile it and load the result; show the compiler's notes."
  (:modes lisp-mode racket-mode clojure-mode)
  (let* ((view (current-view))
         (buffer (view-buffer view)))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (flet ((compile-it ()
             (with-connection (connection :language (buffer-language buffer))
               (request #'backend-compile-file connection (uiop:native-namestring (buffer-file buffer))
                        :on-ok (lambda (result)
                                 (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                                   (show-notes notes :buffer buffer :replace-lines :all)
                                   (compilation-message notes successp duration
                                                        (format nil "Compiled ~a" (buffer-name buffer)))))
                        :on-loaded (lambda ()
                                     (handler-case (progn (follow-package connection (file-package buffer))
                                                          (image-changed)
                                                          ;; Loading replaced the code compiled with breaks in.
                                                          (reapply-breakpoints buffer))
                                       (error (e) (message "Error: ~a" e))))))))
      (if (buffer-modified-p buffer)
          (write-buffer buffer (buffer-file buffer) (lambda (ok) (when ok (compile-it))))
          (compile-it)))))

(define-command load-file ()
  "Load the current file's source into the Lisp."
  (:modes lisp-mode racket-mode clojure-mode)
  (let ((buffer (view-buffer (current-view))))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (with-connection (connection :language (buffer-language buffer))
      (request #'backend-load-file connection (uiop:native-namestring (buffer-file buffer))
           :on-ok (lambda (v) (declare (ignore v))
                    (message "Loaded ~a" (buffer-name buffer))
                    (follow-package connection (file-package buffer))
                    (image-changed))))))

(defun file-package (buffer)
  "The package BUFFER's code ends in: from its last top-level IN-PACKAGE."
  (let ((syntax (buffer-syntax buffer)))
    (and syntax (buffer-package-name syntax (1- (syntax-line-count syntax))))))

;;; Definitions

(defvar *definition-stack* '() "Where M-. came from: (buffer . offset) pairs.")

(defun push-position (view)
  (push (cons (view-buffer view) (text-point (view-gtk-buffer view))) *definition-stack*))

(defun goto-location (location &key then)
  "Show LOCATION (from parse-location) in a tab; then call THEN with its view."
  (flet ((visit (view)
           (let ((gtk-buffer (view-gtk-buffer view)))
             (cond ((getf location :position)
                    (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (min (breakpoint-unshift (view-buffer view) (getf location :position))
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
         (syntax (buffer-sexp-syntax (view-buffer view)))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c))))
         (language (view-language view)))
    (unless name (editor-error "No symbol at the cursor"))
    (let ((package (view-package view))
          (source (and (eq language :common-lisp) (source-definition-choices name (view-buffer view)))))
      (if (and (eq language :common-lisp) (not (connected-p)))
          (if source
              (go-to-definition-choices view name source)
              (message "No definition of ~a in the source (start a Lisp to look in loaded code)" name))
      (with-connection (connection :language language)
        (apply #'request #'backend-find-definitions connection name :package package
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
                                                           (goto-location (parse-location (second d)))))))))
             (racket-place-keys view)))))))

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
         (syntax (buffer-sexp-syntax (view-buffer view)))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c)))))
    (unless name (editor-error "No symbol at the cursor"))
    (let ((package (view-package view)))
      (with-connection (connection :language (view-language view))
        (request #'backend-describe connection name :package package
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
  (unless (eq (view-language view) :common-lisp)
    (return-from request-autodoc (request-language-autodoc view (view-language view))))
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
                  (t (request #'backend-autodoc *connection* form
                          :package (view-package view)
                          :on-ok (lambda (result)
                                   (let ((doc (and (consp result) (stringp (first result)) (first result))))
                                     (when (and doc (second result)) (setf (gethash key *autodoc-cache*) doc))
                                     (show-arglist (or doc (ignore-errors (local-autodoc view syntax line column))))))
                          :on-abort (lambda (reason) (declare (ignore reason)) (show-arglist nil))))))))))

(defun request-language-autodoc (view language)
  "The arglist for the call around VIEW's cursor, from LANGUAGE's (Clojure's
or Racket's) connection if there is one."
  (let ((syntax (buffer-sexp-syntax (view-buffer view)))
        (connection (language-connection language)))
    (if (not (and syntax connection (eq view (focused-view *window*))))
        (show-arglist nil)
        (multiple-value-bind (line column) (cursor-line-column view)
          (let* ((form (and (eq (context-at syntax line column) :code) (raw-form-at syntax line column)))
                 (key (and form (list language (view-package view) (sexp-to-string form)))))
            (cond ((null form) (show-arglist nil))
                  ((gethash key *autodoc-cache*) (show-arglist (gethash key *autodoc-cache*)))
                  (t (request #'backend-autodoc connection form
                              :package (view-package view)
                              :on-ok (lambda (result)
                                       (let ((doc (and (consp result) (stringp (first result)) (first result))))
                                         (when doc (setf (gethash key *autodoc-cache*) doc))
                                         (show-arglist doc)))
                              :on-abort (lambda (reason) (declare (ignore reason)) (show-arglist nil))))))))))

;;; Completion

(defun prefix-before-cursor (view)
  "The symbol characters just before VIEW's cursor, and where they start (an offset)."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (end (text-point gtk-buffer))
         (start end)
         (dialect (or (mode-dialect (buffer-major-mode (view-buffer view))) *common-lisp-dialect*)))
    (loop while (and (> start 0)
                     (let ((c (char (gtk:text-buffer-get-text gtk-buffer (iter-at gtk-buffer (1- start)) (iter-at gtk-buffer start) t) 0)))
                       (not (cadre::dialect-terminating-p c dialect))))
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
