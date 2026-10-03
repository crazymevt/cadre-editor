;;;; commands.lisp — commands: named functions users run by key, menu or palette
;;;;
;;;; A command is an ordinary function, defined with DEFINE-COMMAND, plus
;;;; metadata. Keymaps and menus refer to commands by symbol, so redefining
;;;; one takes effect at once.

(in-package #:cadre)

(defstruct (command (:constructor make-command (name title documentation modes)))
  name title documentation modes)

(defvar *commands* (make-hash-table :test 'eq)
  "Every command, by name.")

(defvar *this-command* nil "The command running now.")
(defvar *last-command* nil "The command that ran before this one.")

(defun command-title-from-name (name)
  (let ((title (substitute #\Space #\- (string-downcase (symbol-name name)))))
    (setf (char title 0) (char-upcase (char title 0)))
    title))

(defmacro define-command (name lambda-list &body body)
  "Define NAME as a command. BODY may start with a docstring, then options:
  (:modes mode ...)   only offer the command in buffers in these major modes
  (:title \"Text\")     the name shown in menus and the palette
The command is a function of LAMBDA-LIST; commands run from keys get no arguments."
  (let* ((documentation (when (and (stringp (first body)) (rest body)) (pop body)))
         (options (loop while (and (consp (first body)) (keywordp (car (first body))))
                        collect (pop body)))
         (modes (rest (assoc :modes options)))
         (title (second (assoc :title options))))
    `(progn
       (defun ,name ,lambda-list ,@(and documentation (list documentation)) ,@body)
       (setf (gethash ',name *commands*)
             (make-command ',name ,(or title (command-title-from-name name))
                           ,documentation ',modes))
       ',name)))

(defun find-command (name)
  "The command named NAME, or nil."
  (gethash name *commands*))

(defun command-applicable-p (command &optional (buffer (current-buffer)))
  "True if COMMAND can run with BUFFER current."
  (let ((modes (command-modes command)))
    (or (null modes)
        (and buffer (member (buffer-major-mode buffer) modes)))))

(defun list-commands (&key (buffer (current-buffer)) all)
  "The commands that apply in BUFFER (every command if ALL), sorted by title."
  (sort (loop for command being the hash-values of *commands*
              when (or all (command-applicable-p command buffer)) collect command)
        #'string< :key #'command-title))

(defun run-command (name &rest arguments)
  "Run the command named NAME with ARGUMENTS, then *after-command-hook*."
  (let ((command (or (find-command name) (editor-error "~a is not a command." name))))
    (unless (command-applicable-p command)
      (editor-error "~a does not apply here." (command-title command)))
    (multiple-value-prog1
        (let ((*this-command* name))
          (apply name arguments))
      (setf *last-command* name)
      (run-hook '*after-command-hook* name))))
