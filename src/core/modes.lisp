;;;; modes.lisp — major modes: what kind of text a buffer holds
;;;;
;;;; A major mode names a buffer's kind of text and carries the keymap and,
;;;; in later milestones, the highlighter and indenter for it. Files choose
;;;; their mode by extension.

(in-package #:cadre)

(defstruct (major-mode (:constructor make-major-mode (name title keymap extensions documentation)))
  name title keymap extensions documentation)

(defvar *major-modes* (make-hash-table :test 'eq))

(defmacro define-major-mode (name (&key title extensions) &optional documentation)
  "Define NAME as a major mode. TITLE is shown in the status bar; EXTENSIONS
lists the file types (without the dot) that open in this mode. The mode's
keymap, MAJOR-MODE-KEYMAP, is kept when the mode is redefined."
  `(let ((old (gethash ',name *major-modes*)))
     (setf (gethash ',name *major-modes*)
           (make-major-mode ',name ,(or title (string-capitalize (symbol-name name)))
                            (if old (major-mode-keymap old) (make-keymap ',name))
                            ',extensions ,documentation))
     ',name))

(defun find-major-mode (name)
  "The major mode named NAME, or nil."
  (gethash name *major-modes*))

(defun major-mode-for-file (pathname)
  "The name of the major mode for files like PATHNAME."
  (let ((type (and pathname (pathname-type pathname))))
    (or (and type
             (loop for mode being the hash-values of *major-modes*
                   when (member type (major-mode-extensions mode) :test #'string-equal)
                     return (major-mode-name mode)))
        'fundamental-mode)))

(define-major-mode fundamental-mode (:title "Text")
  "Plain text, with no special behaviour.")

(define-major-mode lisp-mode (:title "Lisp" :extensions ("lisp" "asd" "lsp" "cl" "l" "ros"))
  "Common Lisp source.")
