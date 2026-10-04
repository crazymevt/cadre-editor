;;;; hints.lisp — what Cadre knows about symbols without asking a Lisp:
;;;; completion as you type, and the parameters of the function under the
;;;; mouse
;;;;
;;;; The definitions come from the source: the open buffers (as they are,
;;;; saved or not) and the project's Lisp files (read on a thread, and again
;;;; when they change), so functions you've written but not loaded have
;;;; hints too. Standard Common Lisp symbols come from Cadre's own Lisp.
;;;; When a Lisp is connected, its completions are added, and it answers
;;;; for symbols the source doesn't define.

(in-package #:cadre-ui)

(define-option *auto-complete* t boolean
  "Show completions while typing a symbol in Lisp code."
  :category "Editing")

(define-option *auto-complete-min-chars* 2 (integer 1 10)
  "How many characters of a symbol to type before completions appear."
  :category "Editing")

(define-option *symbol-hover* t boolean
  "Show a symbol's parameters and documentation when the mouse rests on it."
  :category "Editing")

;;; Definitions in the source

(defun definition-key (name)
  (string-downcase (subseq name (symbol-name-part name))))

(defun buffer-definitions (buffer)
  "The definitions in BUFFER's text, read again after it changes."
  (unless (buffer-local buffer :definitions-handler)
    (setf (buffer-local buffer :definitions-handler)
          (gobject:connect (buffer-text buffer) :changed
                           (lambda (b) (declare (ignore b)) (setf (buffer-local buffer :definitions) nil)))))
  (car (or (buffer-local buffer :definitions)
           (setf (buffer-local buffer :definitions)
                 (list (ignore-errors (source-definitions (buffer-string buffer))))))))

(defun lisp-buffer-p (buffer) (eq (buffer-major-mode buffer) 'lisp-mode))

(defvar *project-definitions* (make-hash-table :test 'equal)
  "Definition key → list of (definition . pathname), from the project's files.")
(defvar *project-definitions-root* nil)
(defvar *project-definitions-time* 0)
(defvar *project-definitions-scanning* nil)
(defvar *file-definitions* (make-hash-table :test 'equal)
  "Namestring → (write date . definitions), so unchanged files aren't read again.")
(defparameter *project-definitions-interval* 15 "Seconds before the project is looked at again.")
(defparameter *max-definition-files* 3000)

(defun scan-project-definitions (root)
  "Read the definitions in ROOT's Lisp files into *PROJECT-DEFINITIONS* (on a thread)."
  (let ((table (make-hash-table :test 'equal))
        (files (remove-if-not (lambda (path) (eq (major-mode-for-file path) 'lisp-mode))
                              (project-files root :hidden-names *explorer-hidden-names*
                                                  :hidden-types *explorer-hidden-types*))))
    (dolist (path (if (> (length files) *max-definition-files*) (subseq files 0 *max-definition-files*) files))
      (let* ((name (namestring path))
             (date (ignore-errors (file-write-date path)))
             (cached (gethash name *file-definitions*))
             (definitions (if (and cached (eql (car cached) date))
                              (cdr cached)
                              (let ((defs (ignore-errors (source-definitions (or (read-text-file path) "")))))
                                (setf (gethash name *file-definitions*) (cons date defs))
                                defs))))
        (dolist (d definitions)
          (push (cons d path) (gethash (definition-key (definition-name d)) table)))))
    (setf *project-definitions* table)))

(defun refresh-project-definitions ()
  "Read the project's definitions again on a thread, if it's been a while."
  (let ((root (and *window* (window-project *window*))))
    (cond ((null root) (clrhash *project-definitions*))
          ((and (not *project-definitions-scanning*)
                (or (not (equal root *project-definitions-root*))
                    (> (- (get-universal-time) *project-definitions-time*) *project-definitions-interval*)))
           (setf *project-definitions-scanning* t
                 *project-definitions-root* root)
           (sb-thread:make-thread
            (lambda ()
              (unwind-protect (ignore-errors (scan-project-definitions root))
                (setf *project-definitions-time* (get-universal-time)
                      *project-definitions-scanning* nil)))
            :name "cadre definitions")))))

(defun open-file-names ()
  (loop for b in (buffer-list)
        when (buffer-file b) collect (namestring (or (probe-file (buffer-file b)) (buffer-file b)))))

(defun find-source-definitions (name &optional buffer)
  "The definitions of NAME, as (definition . buffer or pathname): BUFFER's
first, then the other open buffers', then the project's files'."
  (refresh-project-definitions)
  (let ((key (definition-key name))
        (found '()))
    (flet ((from-buffer (b)
             (dolist (d (buffer-definitions b))
               (when (string= (definition-key (definition-name d)) key)
                 (push (cons d b) found)))))
      (when (and buffer (lisp-buffer-p buffer)) (from-buffer buffer))
      (dolist (b (buffer-list))
        (when (and (not (eq b buffer)) (lisp-buffer-p b)) (from-buffer b)))
      (let ((open (open-file-names)))
        (dolist (entry (gethash key *project-definitions*))
          (unless (member (namestring (cdr entry)) open :test #'string=)
            (push entry found)))))
    (nreverse found)))

(defun callable-kind-p (kind) (member kind '(:function :macro :generic :method :special :command)))

(defun find-callable-definition (name buffer)
  "NAME's definition with a lambda list, from the source; for a generic
function, its DEFGENERIC rather than a method."
  (let ((definitions (remove-if-not (lambda (entry) (definition-arglist (car entry)))
                                    (find-source-definitions name buffer))))
    (flet ((kind (entry) (definition-kind (car entry))))
      (or (find :generic definitions :key #'kind)
          (find-if-not (lambda (entry) (eq (kind entry) :method)) definitions)
          (first definitions)))))

(defun local-arglist (name buffer)
  "NAME's lambda list as text, from the source or from standard Common Lisp, or nil."
  (let ((entry (find-callable-definition name buffer)))
    (if entry
        (definition-arglist (car entry))
        (standard-arglist name))))

;;; Asking the connected Lisp

(defvar *remote-arglists* (make-hash-table :test 'equal)
  "(name . package) → arglist hint from the connected Lisp, or :none.")

(defun remote-arglist (name package then)
  "The connected Lisp's arglist hint for NAME, if it has one cached; else ask
for it and call THEN when it comes."
  (let* ((key (cons (string-downcase name) package))
         (cached (gethash key *remote-arglists*)))
    (cond ((eq cached :pending) nil)
          (cached (and (stringp cached) cached))
          ((connected-p)
           (setf (gethash key *remote-arglists*) :pending)
           (rex *connection* (swank-call "swank:operator-arglist" name package) :package package
                :on-ok (lambda (result)
                         (setf (gethash key *remote-arglists*)
                               (if (and (stringp result) (string/= result "")) result :none))
                         (when then (funcall then)))
                :on-abort (lambda (reason) (declare (ignore reason))
                            (setf (gethash key *remote-arglists*) :none)))
           nil))))

;;; Describing a symbol

(defun kind-name (kind)
  (case kind
    (:special "special operator")
    (:generic "generic function")
    ((nil) "")
    (t (string-downcase kind))))

(defun place-name (place)
  (cond ((null place) nil)
        ((pathnamep place) (file-namestring place))
        (t (buffer-display-name place))))

(defun first-paragraph (string &optional (limit 500))
  (let* ((string (string-trim '(#\Space #\Newline #\Tab) string))
         (end (or (search (format nil "~%~%") string) (length string)))
         (paragraph (subseq string 0 end)))
    (if (> (length paragraph) limit)
        (concatenate 'string (subseq paragraph 0 limit) "…")
        paragraph)))

(defun symbol-description (name buffer &key package on-update)
  "What Cadre knows about NAME, as (signature kind documentation place),
or nil. A signature is the call, as \"(name params…)\", or the name. If
only the connected Lisp knows, it is asked, ON-UPDATE is called when it
answers, and nil is returned for now."
  (let ((entry (or (find-callable-definition name buffer) (first (find-source-definitions name buffer)))))
    (if entry
        (let ((d (car entry)))
          (list (if (definition-arglist d)
                    (arglist-hint (definition-name d) (definition-arglist d))
                    (definition-name d))
                (definition-kind d) (definition-documentation d) (cdr entry)))
        (let* ((symbol (standard-symbol name))
               (kind (and symbol (standard-symbol-kind symbol))))
          (if kind
              (let ((arglist (standard-arglist name)))
                (list (if arglist (arglist-hint (string-downcase (symbol-name symbol)) arglist) (string-downcase name))
                      kind (standard-documentation name kind) nil))
              (let ((remote (and package (remote-arglist name package on-update))))
                (and remote (list remote :function nil nil))))))))

(defun signature-markup (signature)
  "SIGNATURE as Pango markup: the operator in bold."
  (let ((escaped (glib:markup-escape-text signature -1)))
    (if (and (plusp (length escaped)) (char= (char escaped 0) #\())
        (let ((end (or (position #\Space escaped) (1- (length escaped)))))
          (format nil "(<b>~a</b>~a" (subseq escaped 1 end) (subseq escaped end)))
        (format nil "<b>~a</b>" escaped))))

(defun description-markup (description)
  (destructuring-bind (signature kind documentation place) description
    (format nil "<tt>~a</tt>~@[~%<small>~a</small>~]~@[~%~%~a~]"
            (signature-markup signature)
            (let ((words (remove "" (list (kind-name kind) (or (place-name place) "")) :test #'string=)))
              (and words (glib:markup-escape-text (format nil "~{~a~^ · ~}" words) -1)))
            (and documentation (plusp (length documentation))
                 (glib:markup-escape-text (first-paragraph documentation) -1)))))

;;; Hovering

(defun symbol-token-at (syntax line column)
  "The symbol token at (LINE, COLUMN), strictly inside it, and its text."
  (let ((token (find-if (lambda (tk) (and (eq (token-type tk) :symbol)
                                          (<= (token-start tk) column) (< column (token-end tk))))
                        (line-tokens syntax line))))
    (and token (values token (subseq (text-line-string (syntax-text syntax) line)
                                     (token-start token) (token-end token))))))

(defun token-area (view line token)
  "TOKEN's place on LINE in VIEW's text view, in widget coordinates."
  (let* ((text-view (view-text-view view))
         (gtk-buffer (view-gtk-buffer view))
         (start (gtk:text-view-get-iter-location text-view (line-iter gtk-buffer line (token-start token))))
         (end (gtk:text-view-get-iter-location text-view (line-iter gtk-buffer line (token-end token)))))
    (multiple-value-bind (x y)
        (gtk:text-view-buffer-to-window-coords text-view :widget (gdk:rectangle-x start) (gdk:rectangle-y start))
      (gdk:make-rectangle :x x :y y :width (max 1 (- (gdk:rectangle-x end) (gdk:rectangle-x start)))
                          :height (gdk:rectangle-height start)))))

(defun hovered-symbol (view x y)
  "The symbol token under (X, Y) in VIEW's text view, its line, and its text."
  (let ((syntax (buffer-syntax (view-buffer view)))
        (text-view (view-text-view view)))
    (when syntax
      (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords text-view :widget x y)
        (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
          (when ok
            (let ((line (gtk:text-iter-get-line iter)))
              (multiple-value-bind (token name) (symbol-token-at syntax line (gtk:text-iter-get-line-offset iter))
                (and token (values token line name))))))))))

(defun setup-symbol-hover (view)
  "Show the parameters and documentation of the symbol under the mouse in VIEW."
  (let ((text-view (view-text-view view)))
    (gtk:widget-set-has-tooltip text-view t)
    (gobject:connect text-view :query-tooltip
                     (lambda (widget x y keyboard tooltip)
                       (declare (ignore widget))
                       (when (and *symbol-hover* (not keyboard))
                         (multiple-value-bind (token line name) (hovered-symbol view x y)
                           (let ((description
                                   (and token
                                        (symbol-description
                                         name (view-buffer view)
                                         :package (and (connected-p) (view-package view))
                                         :on-update (lambda () (gtk:widget-trigger-tooltip-query text-view))))))
                             (when description
                               (gtk:tooltip-set-markup tooltip (description-markup description))
                               (gtk:tooltip-set-tip-area tooltip (token-area view line token))
                               t))))))))

;;; Argument hints without a connection

(defun local-autodoc (view syntax line column &key (source-only nil))
  "The hint for the call around (LINE, COLUMN) in VIEW from what Cadre
knows itself (with SOURCE-ONLY, only from the source), or nil."
  (let ((form (raw-form-at syntax line column)))
    (when form
      (loop for (operator index keyword) in (form-argument-position form +cursor-marker+)
            for entry = (find-callable-definition operator (view-buffer view))
            for arglist = (if entry
                              (definition-arglist (car entry))
                              (and (not source-only) (standard-arglist operator)))
            when arglist
              return (arglist-hint (if entry (definition-name (car entry)) (string-downcase operator))
                                   arglist index keyword)))))

;;; Completion candidates

(defun buffer-symbol-names (buffer)
  "The symbols and keywords written in BUFFER."
  (let ((syntax (buffer-syntax buffer))
        (names (make-hash-table :test 'equal)))
    (when syntax
      (dotimes (line (syntax-line-count syntax))
        (let ((string (text-line-string (syntax-text syntax) line)))
          (loop for tk across (line-tokens syntax line)
                when (member (token-type tk) '(:symbol :keyword))
                  do (setf (gethash (subseq string (token-start tk) (token-end tk)) names) t)))))
    (loop for name being the hash-keys of names collect name)))

(defun completion-score (pattern name)
  "How well NAME completes PATTERN, or nil if it doesn't: its letters must
come in order, the first at the start of a word."
  (multiple-value-bind (score positions) (fuzzy-match pattern name)
    (when (and score (or (null positions) (cadre::word-start-p name (first positions))))
      (+ score (if (and (<= (length pattern) (length name)) (string-equal pattern name :end2 (length pattern))) 20 0)))))

(defun rank-completions (prefix items &key (limit 200))
  "ITEMS (completion lists, name first) that complete PREFIX, best first."
  (let* ((colon (position #\: prefix :from-end t))
         (pattern (if (and colon (plusp colon)) (subseq prefix (1+ colon)) prefix))
         (scored (loop for item in items
                       for name = (first item)
                       for part = (if (and colon (plusp colon)) (subseq name (min (length name) (1+ (or (position #\: name :from-end t) -1)))) name)
                       for score = (if (string= pattern "") 0 (completion-score pattern part))
                       when score collect (cons score item)))
         (sorted (stable-sort scored #'> :key #'car)))
    (mapcar #'cdr (if (> (length sorted) limit) (subseq sorted 0 limit) sorted))))

(defun local-completions (prefix buffer)
  "Completions for PREFIX from the source, standard Common Lisp and the
words in BUFFER, as (name nil nil kind), best first."
  (refresh-project-definitions)
  (let* ((colon (position #\: prefix :from-end t))
         (package-part (if (and colon (plusp colon)) (subseq prefix 0 (1+ colon)) ""))
         (keyword (and colon (zerop colon)))
         (seen (make-hash-table :test 'equal))
         (items '()))
    (flet ((add (name kind)
             (let ((key (string-downcase name)))
               (unless (or (gethash key seen) (string-equal name prefix))
                 (setf (gethash key seen) t)
                 (push (list (concatenate 'string package-part name) nil nil kind) items)))))
      (unless keyword
        (dolist (b (cons buffer (remove buffer (buffer-list))))
          (when (lisp-buffer-p b)
            (dolist (d (buffer-definitions b)) (add (definition-name d) (definition-kind d)))))
        (loop for entries being the hash-values of *project-definitions*
              do (dolist (e entries) (add (definition-name (car e)) (definition-kind (car e)))))
        (when (member package-part '("" "cl:" "common-lisp:") :test #'string-equal)
          (loop for (name . kind) in (standard-symbols) do (add name kind))))
      (when (string= package-part "")
        (dolist (name (buffer-symbol-names buffer))
          (unless (string-equal name prefix) (add name nil)))))
    (rank-completions prefix (nreverse items))))

(defun merge-completions (prefix local remote)
  "LOCAL completions with the connected Lisp's REMOTE ones (from
swank:fuzzy-completions) added, best first."
  (let ((seen (make-hash-table :test 'equal)))
    (dolist (item local) (setf (gethash (string-downcase (first item)) seen) t))
    (rank-completions prefix
                      (append local
                              (loop for (name nil nil flags) in remote
                                    unless (or (gethash (string-downcase name) seen) (string-equal name prefix))
                                      collect (list name nil nil flags))))))

(defun completion-signature (name buffer)
  "A line about the completion NAME for the popup: its call, or kind."
  (let ((description (ignore-errors (symbol-description name buffer))))
    (and description (first description))))

;;; Completing

(defvar *auto-complete-timer* nil)
(defvar *completion-generation* 0)

(defun start-completion (view &key auto)
  "Show completions for the symbol before VIEW's cursor. AUTO (while typing)
shows nothing for a single exact match, and stays quiet when there are none."
  (multiple-value-bind (prefix start) (prefix-before-cursor view)
    (cond
      ((string= prefix "") (unless auto (editor-error "Nothing to complete")))
      (t
       (let* ((buffer (view-buffer view))
              (local (local-completions prefix buffer))
              (generation (incf *completion-generation*)))
         (flet ((show (items)
                  (cond ((null items) (unless auto (message "No completions for ~a" prefix)))
                        ((and (not auto) (null (rest items))) (replace-prefix view start (first (first items))))
                        (t (show-completions view start items)))))
           (if (connected-p)
               (let ((package (view-package view)))
                 (when local (show local))
                 (rex *connection* (swank-call "swank:fuzzy-completions" prefix package
                                               :limit 100 :time-limit-in-msec 500)
                      :package package
                      :on-ok (lambda (result)
                               (multiple-value-bind (now-prefix now-start) (prefix-before-cursor view)
                                 (when (and (= generation *completion-generation*) (= now-start start)
                                            (string= now-prefix prefix))
                                   (let ((items (merge-completions prefix local (first result))))
                                     (if (and *completion* (eq (cp-view *completion*) view))
                                         (progn (setf (cp-items *completion*) items)
                                                (fill-completions *completion* items))
                                         (unless local (show items)))))))
                      :on-abort (lambda (reason) (declare (ignore reason))
                                  (unless (or local auto) (message "No completions for ~a" prefix)))))
               (show local))))))))

(defun auto-complete-p (view)
  "Whether completions should appear now, after typing in VIEW."
  (let ((syntax (buffer-syntax (view-buffer view))))
    (and *auto-complete* syntax (eq view (focused-view *window*))
         (multiple-value-bind (prefix) (prefix-before-cursor view)
           (and (>= (length (string-left-trim ":" prefix)) *auto-complete-min-chars*)
                (find-if #'alpha-char-p prefix)
                (multiple-value-bind (line column) (cursor-line-column view)
                  (eq (context-at syntax line column) :code)))))))

(defun schedule-auto-complete (view)
  (when *auto-complete-timer* (glib:source-remove *auto-complete-timer*))
  (setf *auto-complete-timer*
        (glib:timeout-add glib:+priority-default+ 120
                          (lambda ()
                            (setf *auto-complete-timer* nil)
                            (when (and (not (completion-open-p)) (auto-complete-p view))
                              (handler-case (start-completion view :auto t)
                                (error () nil)))
                            nil))))

(defun cancel-auto-complete ()
  (when *auto-complete-timer*
    (glib:source-remove *auto-complete-timer*)
    (setf *auto-complete-timer* nil)))
