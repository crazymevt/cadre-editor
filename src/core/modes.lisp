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

;;; Minor modes: extra behaviour a buffer can turn on and off

(defstruct (minor-mode (:constructor make-minor-mode (name title keymap documentation)))
  name title keymap documentation)

(defvar *minor-modes* (make-hash-table :test 'eq))

(define-hook *minor-mode-hook* "Called with a buffer, a minor mode's name and t or nil when it is turned on or off.")

(defmacro define-minor-mode (name (&key title) &optional documentation)
  "Define NAME as a minor mode, with a keymap (MINOR-MODE-KEYMAP) that
applies in buffers where it is on, before the major mode's."
  `(let ((old (gethash ',name *minor-modes*)))
     (setf (gethash ',name *minor-modes*)
           (make-minor-mode ',name ,(or title (string-capitalize (substitute #\Space #\- (symbol-name name))))
                            (if old (minor-mode-keymap old) (make-keymap ',name))
                            ,documentation))
     ',name))

(defun find-minor-mode (name)
  (gethash name *minor-modes*))

(defun minor-mode-enabled-p (buffer name)
  (and (member name (buffer-local buffer :minor-modes)) t))

(defun set-minor-mode (buffer name on)
  "Turn the minor mode NAME on or off in BUFFER."
  (unless (find-minor-mode name) (error "~a is not a minor mode." name))
  (unless (eq (minor-mode-enabled-p buffer name) (and on t))
    (setf (buffer-local buffer :minor-modes)
          (if on
              (cons name (buffer-local buffer :minor-modes))
              (remove name (buffer-local buffer :minor-modes))))
    (run-hook '*minor-mode-hook* buffer name (and on t)))
  (and on t))

(defun buffer-minor-mode-keymaps (buffer)
  "The keymaps of BUFFER's minor modes, the most recently turned on first."
  (loop for name in (buffer-local buffer :minor-modes)
        for mode = (find-minor-mode name)
        when mode collect (minor-mode-keymap mode)))
