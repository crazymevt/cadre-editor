;;;; options.lisp — user options: special variables with a type and a docstring
;;;;
;;;; Options are what the settings page lists. Each has a type, which the
;;;; page uses to choose how to edit it (OPTION-KIND), and a category to
;;;; group it under.

(in-package #:cadre)

(defstruct (option (:constructor make-option (name default type documentation &optional category)))
  name default type documentation category)

(defvar *options* (make-hash-table :test 'eq)
  "Every option defined with define-option, by name.")

(defmacro define-option (name default type documentation &key (category "Other"))
  "Define NAME as a user option: a special variable with DEFAULT as its
initial value, TYPE (a type specifier), DOCUMENTATION and a CATEGORY (a
string) for the settings page. Set options in your init file with setf."
  `(progn
     (defvar ,name ,default ,documentation)
     (setf (gethash ',name *options*) (make-option ',name ',default ',type ,documentation ,category))
     ',name))

(defun find-option (name)
  (gethash name *options*))

(defun list-options ()
  "Every option, sorted by name."
  (sort (loop for option being the hash-values of *options* collect option)
        #'string< :key (lambda (o) (symbol-name (option-name o)))))

(defun option-title (option)
  "OPTION's name for people: *editor-font* → Editor font."
  (let ((title (substitute #\Space #\- (string-downcase (string-trim "*" (symbol-name (option-name option)))))))
    (when (plusp (length title)) (setf (char title 0) (char-upcase (char title 0))))
    title))

(defun option-kind (option)
  "How to edit OPTION's value, from its type:
  :boolean           on or off
  (:integer min max) a whole number (MAX may be nil)
  (:choice values)   one of VALUES
  :string            text
  :optional-string   text, or nil when empty
  :lisp              any value, written as Lisp"
  (let ((type (option-type option)))
    (cond ((eq type 'boolean) :boolean)
          ((eq type 'string) :string)
          ((eq type 'integer) (list :integer nil nil))
          ((and (consp type) (eq (first type) 'integer))
           (list :integer (let ((min (second type))) (if (eq min '*) nil min))
                 (let ((max (third type))) (if (eq max '*) nil max))))
          ((and (consp type) (eq (first type) 'member)) (list :choice (rest type)))
          ((equal type '(or null string)) :optional-string)
          (t :lisp))))

(defun option-value (option)
  (symbol-value (option-name option)))

(defun valid-option-value-p (option value)
  (typep value (option-type option)))

(defun set-option-value (option value)
  "Set OPTION to VALUE, which must be of its type. Signals an editor-error otherwise."
  (unless (valid-option-value-p option value)
    (error 'editor-error :message (format nil "~s is not a valid value for ~a (~s)"
                                          value (option-title option) (option-type option))))
  (setf (symbol-value (option-name option)) value))

(defun read-option-value (option string)
  "The value written as STRING for OPTION: read as Lisp for :lisp options,
else taken as text. Signals an editor-error if it isn't valid."
  (let ((value (case (option-kind option)
                 (:string string)
                 (:optional-string (if (string= (string-trim " " string) "") nil string))
                 (t (handler-case
                        (with-standard-io-syntax
                          (let ((*read-eval* nil) (*package* (or (find-package :cadre-user) (find-package :cadre))))
                            (multiple-value-bind (v end) (read-from-string string)
                              (unless (string= "" (string-trim '(#\Space #\Tab #\Newline) (subseq string end)))
                                (error "more than one value"))
                              v)))
                      (error (e) (error 'editor-error :message (format nil "Not a Lisp value: ~a" e))))))))
    (unless (valid-option-value-p option value)
      (error 'editor-error :message (format nil "~s is not a valid value for ~a" value (option-title option))))
    value))

(defun write-option-value (option)
  "OPTION's value as text, as READ-OPTION-VALUE reads it."
  (let ((value (option-value option)))
    (case (option-kind option)
      (:string value)
      (:optional-string (or value ""))
      (t (with-standard-io-syntax
           (let ((*package* (or (find-package :cadre-user) (find-package :cadre))) (*print-readably* nil))
             (prin1-to-string value)))))))
