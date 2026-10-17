;;;; Evaluated in the connected Lisp, never in Cadre's image.
(defpackage #:cadre-clos-browser
  (:use #:cl)
  (:export #:catalog #:describe-entry #:object #:class-of-expression))
(in-package #:cadre-clos-browser)

(defun printed (value)
  (let ((*package* (find-package :keyword)) (*print-readably* nil)
        (*print-pretty* nil) (*print-circle* t) (*print-length* 40) (*print-level* 8))
    (prin1-to-string value)))

(defun read-name (text package)
  (let ((*package* (or (find-package package) (error "No package ~a" package)))
        (*read-eval* nil))
    (multiple-value-bind (name end) (read-from-string text)
      (unless (and (or (symbolp name)
                       (and (listp name) (= (length name) 2) (eq (first name) 'setf)
                            (symbolp (second name))))
                   (every (lambda (c) (find c '(#\Space #\Tab #\Newline #\Return)))
                          (subseq text end)))
        (error "Expected a class or generic function name"))
      name)))

(defun object (kind name package)
  (let ((symbol (read-name name package)))
    (ecase kind
      (:class (or (and (symbolp symbol) (find-class symbol nil))
                  (error "No class named ~a" name)))
      (:generic (let ((function (and (fboundp symbol) (fdefinition symbol))))
                  (if (typep function 'generic-function) function
                      (error "~a is not a generic function" name)))))))

(defun name-package (name)
  (let ((symbol (if (consp name) (second name) name)))
    (and (symbolp symbol) (symbol-package symbol) (package-name (symbol-package symbol)))))

(defun catalog (package)
  (handler-case
      (let ((pkg (or (find-package package) (error "No package ~a" package)))
            (entries '()) (seen (make-hash-table :test 'equal)))
        (do-symbols (symbol pkg)
          ;; Inherited symbols remain reachable through their home package.
          (when (eq (symbol-package symbol) pkg)
            (let ((class (find-class symbol nil)))
              (when class
                (let ((key (printed (class-name class))))
                  (unless (gethash (cons :class key) seen)
                    (setf (gethash (cons :class key) seen) t)
                    (push (list :kind :class :name key) entries)))))
            (dolist (name (list symbol (list 'setf symbol)))
              (when (and (fboundp name) (typep (fdefinition name) 'generic-function))
                (push (list :kind :generic :name (printed name)) entries)))))
        (list :packages (sort (mapcar #'package-name (list-all-packages)) #'string-lessp)
              :entries (stable-sort entries #'string-lessp :key (lambda (e) (getf e :name)))))
    (error (e) (list :error (princ-to-string e)))))

(defun source (object)
  (or (ignore-errors (swank-backend:find-source-location object))
      '(:error "No source location is available")))

(defun specializer-info (specializer)
  (if (typep specializer 'class)
      (list :name (printed (class-name specializer)) :class t)
      (list :name (printed (list 'eql (swank-mop:eql-specializer-object specializer))) :class nil)))

(defun method-info (method &optional selected-class)
  (let* ((gf (swank-mop:method-generic-function method))
         (name (swank-mop:generic-function-name gf)))
    (list :name (printed name) :package (name-package name)
          :qualifiers (mapcar #'printed (swank-mop:method-qualifiers method))
          :specializers (mapcar #'specializer-info (swank-mop:method-specializers method))
          :lambda-list (printed (swank-mop:method-lambda-list method))
          :inherited (and selected-class
                          (not (member selected-class (swank-mop:method-specializers method))))
          :source (source method))))

(defun describe-entry (kind name package)
  (handler-case
      (let* ((object (object kind name package))
             (warnings '())
             (name (if (eq kind :class) (class-name object)
                       (swank-mop:generic-function-name object)))
             (result (list :kind kind :name (printed name) :package (name-package name)
                           :metaclass (printed (class-name (class-of object)))
                           :documentation (or (ignore-errors (documentation object t)) "")
                           :source (source object))))
        (flet ((optional-value (label thunk)
                 (handler-case (funcall thunk)
                   (error (e) (push (format nil "~a: ~a" label e) warnings) nil))))
          (if (eq kind :class)
              (let* ((finalized (optional-value "Class finalization" (lambda () (swank-mop:class-finalized-p object))))
                     (precedence (when finalized
                                   (optional-value "Class precedence" (lambda () (swank-mop:class-precedence-list object)))))
                     (classes (or precedence (list object)))
                     (slots (optional-value "Slots"
                                            (lambda () (if finalized (swank-mop:class-slots object)
                                                           (swank-mop:class-direct-slots object)))))
                     (methods (optional-value "Methods"
                                              (lambda () (remove-duplicates
                                                          (loop for class in classes
                                                                append (copy-list (swank-mop:specializer-direct-methods class)))
                                                          :test #'eq)))))
                (unless finalized (push "Class is not finalized; showing direct slots only." warnings))
                (setf (getf result :finalized) finalized
                      (getf result :superclasses)
                      (optional-value "Superclasses" (lambda () (mapcar (lambda (c) (printed (class-name c)))
                                                                         (swank-mop:class-direct-superclasses object))))
                      (getf result :subclasses)
                      (optional-value "Subclasses" (lambda () (mapcar (lambda (c) (printed (class-name c)))
                                                                       (swank-mop:class-direct-subclasses object))))
                      (getf result :precedence) (mapcar (lambda (c) (printed (class-name c))) precedence)
                      (getf result :slots)
                      (loop for slot in slots collect
                        (optional-value "Slot details"
                         (lambda ()
                           (let ((slot-name (swank-mop:slot-definition-name slot)))
                             (list :name (printed slot-name)
                                   :type (printed (swank-mop:slot-definition-type slot))
                                   :allocation (printed (swank-mop:slot-definition-allocation slot))
                                   :initargs (mapcar #'printed (swank-mop:slot-definition-initargs slot))
                                   :defined-by
                                   (loop for class in classes
                                         when (find slot-name (swank-mop:class-direct-slots class)
                                                    :key #'swank-mop:slot-definition-name)
                                         collect (printed (class-name class))))))))
                      (getf result :methods)
                      (remove nil (mapcar (lambda (m) (optional-value "Method details" (lambda () (method-info m object)))) methods))))
              (setf (getf result :lambda-list) (printed (swank-mop:generic-function-lambda-list object))
                    (getf result :method-combination) (printed (swank-mop:generic-function-method-combination object))
                    (getf result :methods)
                    (mapcar #'method-info (swank-mop:generic-function-methods object)))))
        (setf (getf result :slots) (remove nil (getf result :slots))
              (getf result :warnings) (nreverse warnings))
        result)
    (error (e) (list :error (princ-to-string e)))))

(defun class-of-expression (expression)
  "Used by the inspector's Browse Class action, in its evaluation thread."
  (let* ((*read-eval* nil) (value (eval (read-from-string expression)))
         (class (if (typep value 'class) value (class-of value))))
    (list :name (printed (class-name class)) :package (name-package (class-name class)))))
