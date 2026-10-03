;;;; hooks.lisp — named lists of functions, run at points the editor defines

(in-package #:cadre)

(defmacro define-hook (name &optional documentation)
  "Define NAME as a hook: a special variable holding a list of functions."
  `(defvar ,name '() ,@(and documentation (list documentation))))

(defun add-hook (hook function)
  "Add FUNCTION (a function or a symbol naming one) to HOOK, a symbol naming a
hook. Adding the same function twice has no effect."
  (pushnew function (symbol-value hook) :test #'equal)
  function)

(defun remove-hook (hook function)
  "Remove FUNCTION from HOOK."
  (setf (symbol-value hook) (remove function (symbol-value hook) :test #'equal))
  function)

(defun run-hook (hook &rest arguments)
  "Call each function on HOOK with ARGUMENTS, oldest first."
  (dolist (function (reverse (symbol-value hook)))
    (apply function arguments)))

(define-hook *buffer-created-hook* "Called with each new buffer.")
(define-hook *buffer-killed-hook* "Called with each buffer as it is killed.")
(define-hook *before-save-hook* "Called with a buffer before it is saved.")
(define-hook *after-save-hook* "Called with a buffer after it is saved.")
(define-hook *after-command-hook* "Called with the command's name after each command runs.")
