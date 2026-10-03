;;;; options.lisp — user options: special variables with a type and a docstring

(in-package #:cadre)

(defstruct (option (:constructor make-option (name default type documentation)))
  name default type documentation)

(defvar *options* (make-hash-table :test 'eq)
  "Every option defined with define-option, by name.")

(defmacro define-option (name default type documentation)
  "Define NAME as a user option: a special variable with DEFAULT as its
initial value, TYPE (a type specifier) and DOCUMENTATION. Options are what a
settings page lists; set them in your init file with setf."
  `(progn
     (defvar ,name ,default ,documentation)
     (setf (gethash ',name *options*) (make-option ',name ',default ',type ,documentation))
     ',name))

(defun find-option (name)
  (gethash name *options*))

(defun list-options ()
  "Every option, sorted by name."
  (sort (loop for option being the hash-values of *options* collect option)
        #'string< :key (lambda (o) (symbol-name (option-name o)))))
