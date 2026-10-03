;;;; editor.lisp — what commands ask of whatever is showing the editor
;;;;
;;;; The GUI sets *FRONTEND* to an object implementing these generic
;;;; functions. With no frontend, as in tests, messages go to
;;;; *standard-output* and the current buffer is *CURRENT-BUFFER*.

(in-package #:cadre)

(defvar *frontend* nil
  "The object showing the editor, or nil.")

(defvar *current-buffer* nil
  "The current buffer when there is no frontend.")

(defgeneric frontend-current-buffer (frontend)
  (:documentation "The buffer the user is working in."))

(defgeneric frontend-message (frontend string)
  (:documentation "Show STRING to the user briefly, as in Emacs's echo area."))

(defun current-buffer ()
  "The buffer the user is working in, or nil."
  (if *frontend* (frontend-current-buffer *frontend*) *current-buffer*))

(defun message (control &rest arguments)
  "Show a message formatted from CONTROL and ARGUMENTS. Returns the string."
  (let ((string (apply #'format nil control arguments)))
    (if *frontend*
        (frontend-message *frontend* string)
        (format *standard-output* "~&~a~%" string))
    string))

(define-condition editor-error (error)
  ((message :initarg :message :reader editor-error-message))
  (:report (lambda (c s) (write-string (editor-error-message c) s)))
  (:documentation "An error to show the user as a message, without a backtrace."))

(defun editor-error (control &rest arguments)
  "Signal an editor-error: a problem to report to the user, such as a
command used where it does not apply."
  (error 'editor-error :message (apply #'format nil control arguments)))
