;;;; tree-sitter.lisp — JavaScript, TypeScript and JSON through tree-sitter
;;;;
;;;; A buffer in a tree-sitter mode keeps a document (core tree-sitter.lisp):
;;;; it is parsed again a moment after each change, on a thread (a big file
;;;; takes tens of milliseconds), then the lines on screen
;;;; are colored from the grammar's highlight query, a chunk at a time, as
;;;; they come into view. Folding and the Outline page come from the same
;;;; tree. Install Language Grammar builds a grammar (cloning and compiling it
;;;; on a thread, with its output on the Output page).
;;;;
;;;; Editing: Return keeps the line's indentation and indents after an
;;;; opening bracket (putting a closing one right after the cursor on its own
;;;; line); Tab indents; Toggle Comment uses the language's line comment.

(in-package #:cadre-ui)

(define-option *code-indent-width* 2 (integer 1 16)
  "Spaces per indentation level in JavaScript, TypeScript and JSON."
  :category "Languages")

(defparameter *tree-sitter-parse-delay* 40 "Milliseconds after an edit before the text is parsed again.")

(defun buffer-ts-document (buffer) (buffer-local buffer :ts-document))

(defun tree-sitter-buffer-p (buffer)
  (and (tree-sitter-language-for-mode (buffer-major-mode buffer)) t))

;;; Attaching

(defun attach-tree-sitter (buffer)
  "Give BUFFER a tree-sitter document if its mode has a language whose
grammar is installed (once; again after its mode changes)."
  (let ((name (tree-sitter-language-for-mode (buffer-major-mode buffer)))
        (document (buffer-ts-document buffer)))
    (cond ((null name) (when document (detach-tree-sitter buffer)))
          ((and document (string= name (ts-language-name (ts-document-language document)))))
          ((not (tree-sitter-language-installed-p name))
           (unless (buffer-local buffer :ts-missing-noted)
             (setf (buffer-local buffer :ts-missing-noted) t)
             (message "For ~a highlighting, folding and outline: M-x install-language-grammar"
                      (getf (tree-sitter-language-spec name) :title))))
          (t
           (handler-case
               (let ((document (make-ts-document (load-ts-language name))))
                 (setf (buffer-local buffer :ts-document) document
                       (buffer-local buffer :ts-generation) 0)
                 (unless (buffer-local buffer :ts-handler)
                   (setf (buffer-local buffer :ts-handler)
                         (gobject:connect (buffer-text buffer) :changed
                                          (lambda (b) (declare (ignore b)) (tree-sitter-changed buffer)))))
                 (reparse-tree-sitter buffer)
                 (when (ts-language-problems (ts-document-language document))
                   (panel-log (window-panel *window*)
                              (format nil "Parts of the ~a highlight query were left out: ~{~a~^; ~}" name
                                      (ts-language-problems (ts-document-language document))))))
             (editor-error (e) (message "~a" (editor-error-message e))))))))

(defun detach-tree-sitter (buffer)
  (setf (buffer-local buffer :ts-document) nil)
  (let ((gtk-buffer (buffer-text buffer)))
    (remove-syntax-tags gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer) (gtk:text-buffer-get-end-iter gtk-buffer))))

;;; Parsing, a moment after each change

(defun tree-sitter-changed (buffer)
  (when (buffer-ts-document buffer)
    (setf (buffer-local buffer :ts-dirty) t)
    (let ((timer (buffer-local buffer :ts-timer)))
      (when timer (glib:source-remove timer)))
    (setf (buffer-local buffer :ts-timer)
          (glib:timeout-add glib:+priority-default+ *tree-sitter-parse-delay*
                            (lambda ()
                              (setf (buffer-local buffer :ts-timer) nil)
                              (when (and (member buffer (buffer-list)) (buffer-ts-document buffer))
                                (reparse-in-background buffer))
                              nil)))))

(defun parsed (buffer document)
  "DOCUMENT, BUFFER's, has just been parsed: color the lines on screen afresh."
  (declare (ignore document))
  (setf (buffer-local buffer :ts-colored) (make-array (gtk:text-buffer-get-line-count (buffer-text buffer))
                                                      :element-type 'bit :initial-element 0))
  (schedule-highlight buffer)
  (when (and *outline* (eq buffer (ol-buffer *outline*))) (refresh-outline :force t)))

(defun reparse-in-background (buffer)
  "Parse BUFFER's text on a thread (a big file takes tens of milliseconds),
then show the new tree. Edits meanwhile mean another parse after."
  (let ((document (buffer-ts-document buffer)))
    (if (buffer-local buffer :ts-parsing)
        (setf (buffer-local buffer :ts-parse-again) t)
        (let ((string (buffer-string buffer))
              (generation (buffer-local buffer :ts-generation)))
          (setf (buffer-local buffer :ts-parsing) t
                (buffer-local buffer :ts-dirty) nil)
          (sb-thread:make-thread
           (lambda ()
             (let ((state (ignore-errors (ts-parse-state document string))))
               (glib:call-in-main-thread (lambda () (background-parse-done buffer document generation state)))))
           :name "cadre tree-sitter parse")))))

(defun background-parse-done (buffer document generation state)
  (setf (buffer-local buffer :ts-parsing) nil)
  (let ((current (and (eq document (buffer-ts-document buffer))
                      (eql generation (buffer-local buffer :ts-generation)))))
    (cond ((null state))
          ;; Replaced, or parsed on the main thread meanwhile (newer text): drop this tree.
          ((not current) (cadre::%tree-delete (getf state :tree)))
          (t (incf (buffer-local buffer :ts-generation))
             (ts-install-state document state)))
    (cond ((not (eq document (buffer-ts-document buffer))))
          ((or (buffer-local buffer :ts-parse-again) (buffer-local buffer :ts-dirty))
           (setf (buffer-local buffer :ts-parse-again) nil
                 (buffer-local buffer :ts-dirty) t)
           (reparse-in-background buffer))
          ((and state current) (parsed buffer document)))))

(defun reparse-tree-sitter (buffer)
  "Parse BUFFER's text now, on this thread, and show the new tree."
  (let ((document (buffer-ts-document buffer)))
    (ts-parse document (buffer-string buffer))
    (setf (buffer-local buffer :ts-dirty) nil
          (buffer-local buffer :ts-generation) (1+ (or (buffer-local buffer :ts-generation) 0)))
    (parsed buffer document)))

(defun fresh-ts-document (buffer)
  "BUFFER's document, parsed now if an edit is waiting."
  (let ((document (buffer-ts-document buffer)))
    (when (and document (or (buffer-local buffer :ts-dirty) (buffer-local buffer :ts-parsing)))
      (reparse-tree-sitter buffer))
    document))

;;; Coloring the lines on screen

(defun highlight-tree-sitter-view (view)
  (let* ((buffer (view-buffer view))
         (document (buffer-ts-document buffer))
         (colored (buffer-local buffer :ts-colored)))
    (when (and document colored (not (buffer-local buffer :ts-dirty)) (not (buffer-local buffer :ts-parsing)))
      (let ((gtk-buffer (view-gtk-buffer view)))
        (multiple-value-bind (first last) (visible-lines view 40)
          (setf last (min last (1- (length colored))))
          ;; Each run of lines not colored yet, in one query.
          (loop with line = first
                while (<= line last)
                do (if (= 1 (aref colored line))
                       (incf line)
                       (let ((end line))
                         (loop while (and (< (1+ end) (1+ last)) (= 0 (aref colored (1+ end)))) do (incf end))
                         (color-lines gtk-buffer document line end)
                         (fill colored 1 :start line :end (1+ end))
                         (setf line (1+ end))))))))))

(defun color-lines (gtk-buffer document first last)
  (let* ((start (line-iter gtk-buffer first))
         (end (line-end-iter gtk-buffer last))
         (start-offset (gtk:text-iter-get-offset start))
         (end-offset (gtk:text-iter-get-offset end)))
    (remove-syntax-tags gtk-buffer start end)
    (dolist (span (ts-highlight-spans document start-offset end-offset))
      (destructuring-bind (s e face) span
        (let ((tag (face-tag gtk-buffer face)))
          (when tag
            (gtk:text-buffer-apply-tag gtk-buffer tag (iter-at gtk-buffer s) (iter-at gtk-buffer e))))))))

;;; Editing

(defun bracket-pairs () '((#\{ . #\}) (#\[ . #\]) (#\( . #\))))

(defparameter *void-elements*
  '("area" "base" "br" "col" "embed" "hr" "img" "input" "link" "meta" "source" "track" "wbr"))

(defun html-opening-tag-before (before &key (void-elements *void-elements*))
  "True if BEFORE (a line up to the cursor) ends with an opening tag that
takes content, such as <div class=\"x\">. VOID-ELEMENTS never do (HTML's
<br>; none in XML)."
  (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings "<([A-Za-z_][\\w.:-]*)[^<>]*>$" before)
    (and match
         (not (cl-ppcre:scan "/>$" before))
         (not (member (string-downcase (aref groups 0)) void-elements :test #'string=)))))

(define-command code-newline ()
  "Start a new line with this one's indentation, one level more after an
opening bracket; between brackets, put the closing one on its own line."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete-selection gtk-buffer t t)
      (multiple-value-bind (line column) (cursor-line-column view)
        (let* ((string (text-line-string gtk-buffer line))
               (indent (leading-space-count string))
               (before (string-right-trim " " (subseq string 0 column)))
               (after (string-left-trim " " (subseq string column)))
               (tag (case (buffer-major-mode (view-buffer view))
                      (html-mode (html-opening-tag-before before))
                      (xml-mode (html-opening-tag-before before :void-elements '()))))
               (opens (or tag (and (plusp (length before)) (assoc (char before (1- (length before))) (bracket-pairs)))))
               (closes (if tag
                           (and (>= (length after) 2) (string= "</" after :end2 2))
                           (and opens (plusp (length after)) (char= (char after 0) (cdr opens)))))
               (inner (if opens (+ indent *code-indent-width*) indent)))
          (gtk:text-buffer-insert-at-cursor gtk-buffer (format nil "~%~a" (make-string inner :initial-element #\Space)) -1)
          (when closes
            (let ((mark (gtk:text-buffer-create-mark gtk-buffer nil (cursor-iter gtk-buffer) t)))
              (gtk:text-buffer-insert-at-cursor gtk-buffer (format nil "~%~a" (make-string indent :initial-element #\Space)) -1)
              (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-iter-at-mark gtk-buffer mark))
              (gtk:text-buffer-delete-mark gtk-buffer mark))))))
    (scroll-to-cursor view)))

(define-command code-indent ()
  "Indent to the next level (with a selection, every selected line)."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (if has
          (with-user-action (gtk-buffer)
            (loop for line from (gtk:text-iter-get-line start)
                    to (- (gtk:text-iter-get-line end) (if (and (gtk:text-iter-starts-line end)
                                                                (> (gtk:text-iter-get-line end) (gtk:text-iter-get-line start)))
                                                           1 0))
                  do (gtk:text-buffer-insert gtk-buffer (line-iter gtk-buffer line)
                                             (make-string *code-indent-width* :initial-element #\Space) -1)))
          (multiple-value-bind (line column) (cursor-line-column view)
            (declare (ignore line))
            (gtk:text-buffer-insert-at-cursor gtk-buffer
                                              (make-string (- *code-indent-width* (mod column *code-indent-width*))
                                                           :initial-element #\Space)
                                              -1))))))

;;; Installing grammars

(defvar *installing-grammar* nil)

(defun grammar-installed (name)
  "After NAME's grammar is installed: use it in the buffers that want it."
  (remhash name cadre::*loaded-languages*)
  (dolist (buffer (buffer-list))
    (when (equal (tree-sitter-language-for-mode (buffer-major-mode buffer)) name)
      (setf (buffer-local buffer :ts-document) nil (buffer-local buffer :ts-missing-noted) nil)
      (attach-tree-sitter buffer))))

(define-command install-language-grammar ()
  "Install (or rebuild) a tree-sitter grammar: JavaScript, TypeScript, TSX or
JSON. Clones its repository at a pinned version and compiles it; needs
libtree-sitter and a C compiler."
  (when *installing-grammar* (editor-error "A grammar is being installed already"))
  (unless (tree-sitter-prefix)
    (editor-error "libtree-sitter isn't installed (with Homebrew: brew install tree-sitter)"))
  (open-picker (window-picker *window*)
               :items *tree-sitter-languages*
               :label (lambda (spec) (getf spec :title))
               :detail (lambda (spec)
                         (format nil "~:[not installed~;installed: choose to rebuild~] · .~{~a~^ .~}"
                                 (tree-sitter-language-installed-p (getf spec :name)) (getf spec :extensions)))
               :placeholder "Install which grammar?"
               :on-choose
               (lambda (spec)
                 (let ((name (getf spec :name)))
                   (setf *installing-grammar* name)
                   (call-command 'show-output)
                   (message "Installing the ~a grammar…" (getf spec :title))
                   (sb-thread:make-thread
                    (lambda ()
                      (let ((result (handler-case
                                        (progn (install-tree-sitter-language
                                                name :log (lambda (line)
                                                            (glib:call-in-main-thread
                                                             (lambda () (panel-log-raw (window-panel *window*) line)))))
                                               nil)
                                      (error (e) (princ-to-string e)))))
                        (glib:call-in-main-thread
                         (lambda ()
                           (setf *installing-grammar* nil)
                           (if result
                               (message "Couldn't install ~a: ~a" (getf spec :title) result)
                               (progn (grammar-installed name)
                                      (message "Installed the ~a grammar" (getf spec :title))))))))
                    :name "cadre grammar install")))))
