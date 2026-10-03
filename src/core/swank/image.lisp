;;;; image.lisp — what the running Lisp knows about the symbols in a buffer
;;;;
;;;; Highlighting's second layer (design doc 7.3) colors symbols by what
;;;; they are in the image: macros, special variables, constants, and calls
;;;; to functions that do not exist. The GUI asks the Lisp to classify names
;;;; with IMAGE-CLASSIFY-SOURCE and caches the answers.
;;;;
;;;; "Undefined function" is only claimed where a symbol is surely called:
;;;; the head of a list whose own place is evaluated (a top-level form, an
;;;; argument of a function, the body of LET, WHEN, DEFUN…), and that is
;;;; not a local function from FLET, LABELS or MACROLET. Elsewhere — a LET
;;;; binding, a CASE key, a slot specifier — a list is data, and its head
;;;; is left alone.

(in-package #:cadre)

;;; Asking the Lisp

(defparameter *image-classes*
  '(:no-package :unknown :symbol :special-operator :macro :function :constant :special-variable)
  "What IMAGE-CLASSIFY-SOURCE says about a name.")

(defun classifiable-name-p (name)
  "True if NAME can be looked up without reading it: no escapes."
  (and (plusp (length name))
       (notany (lambda (c) (member c '(#\| #\\ #\" #\#))) name)
       (char/= (char name 0) #\:)))

(defun image-classify-source (package names)
  "Source for the other Lisp that classifies NAMES (symbol names as written,
possibly with a package prefix) in PACKAGE: its value is a list of
*image-classes* keywords, one for each name. It never interns a symbol, and
returns nil rather than signal an error."
  (format nil "(cl:ignore-errors
 (cl:let ((#1=#:default (cl:find-package ~s)))
  (cl:mapcar
   (cl:lambda (#2=#:name)
    (cl:let* ((#3=#:colon (cl:position #\\: #2#))
              (#4=#:package (cl:if #3# (cl:find-package (cl:string-upcase (cl:subseq #2# 0 #3#))) #1#))
              (#5=#:symbol (cl:and #4# (cl:find-symbol (cl:string-upcase (cl:string-left-trim \":\" (cl:subseq #2# (cl:or #3# 0)))) #4#))))
     (cl:cond ((cl:null #4#) :no-package)
              ((cl:null #5#) :unknown)
              ((cl:special-operator-p #5#) :special-operator)
              ((cl:macro-function #5#) :macro)
              ((cl:fboundp #5#) :function)
              ((cl:constantp #5#) :constant)
              ((cl:boundp #5#) :special-variable)
              (cl:t :symbol))))
   '~s)))"
          package (mapcar #'string-downcase names)))

(defun parse-image-classes (reply count)
  "The classes from a reply of swank:eval-and-grab-output on IMAGE-CLASSIFY-SOURCE,
as a list of COUNT keywords (nil for those it could not classify)."
  (let ((value (ignore-errors (read-sexp (second reply)))))
    (loop for i below count
          for class = (and (listp value) (nth i value))
          collect (and (member class *image-classes*) class))))

;;; Where a head is surely a call

(defparameter *evaluated-arguments*
  (let ((table (make-hash-table :test 'equal)))
    (loop for (from . names) in
          '((1 progn prog1 prog2 when unless if and or not return setq setf psetf psetq incf decf
             push pushnew unwind-protect multiple-value-prog1 multiple-value-list multiple-value-call
             funcall apply catch throw tagbody locally values print princ prin1 format list list*
             cons car cdr first second rest nth elt aref gethash error warn signal cerror
             assert check-type)
            (2 let let* flet labels macrolet symbol-macrolet lambda multiple-value-bind
             destructuring-bind dolist dotimes block return-from the eval-when handler-bind
             with-open-file with-open-stream with-output-to-string with-input-from-string
             with-simple-restart with-slots with-accessors print-unreadable-object progv)
            (3 defun defmacro))
          do (dolist (name names) (setf (gethash (string-downcase (symbol-name name)) table) from)))
    (dolist (name '(handler-case restart-case ignore-errors))
      (setf (gethash (string-downcase (symbol-name name)) table) (list 1)))
    table)
  "Operator name → where its evaluated arguments start (1 is the first
argument), or (n) when only argument N is evaluated.")

(defparameter *inner-forms*
  '((let 1 :bindings 1) (let* 1 :bindings 1) (handler-bind 1 :bindings 1)
    (flet 1 :bindings 2) (labels 1 :bindings 2) (macrolet 1 :bindings 2)
    (do 1 :bindings 1) (do* 1 :bindings 1) (do 2 :list 0) (do* 2 :list 0)
    (dolist 1 :list 1) (dotimes 1 :list 1) (with-open-file 1 :list 1) (with-open-stream 1 :list 1)
    (with-output-to-string 1 :list 1) (with-input-from-string 1 :list 1)
    (cond (1) :list 0) (case (2) :list 1) (ecase (2) :list 1) (ccase (2) :list 1)
    (typecase (2) :list 1) (etypecase (2) :list 1) (ctypecase (2) :list 1)
    (handler-case (2) :list 2) (restart-case (2) :list 2))
  "Lists inside special forms and macros that hold forms: (operator argument
kind from). ARGUMENT is the argument's index (the first is 1), or (n) for every
argument from N on.
KIND :list means the argument is a list whose elements from FROM are forms;
:bindings means it is a list of bindings, each with forms from FROM.")

(defun inner-form-from (operator argument kind)
  "Where forms start in OPERATOR's ARGUMENT of KIND, or nil."
  (loop for (op arg k from) in *inner-forms*
        when (and (eq k kind) (string= operator (string-downcase (symbol-name op)))
                  (if (consp arg) (>= argument (first arg)) (= argument arg)))
          return from))

(defun cl-function-p (base)
  "True if BASE (lower case, no package) names a Common Lisp function."
  (let ((symbol (find-symbol (string-upcase base) :common-lisp)))
    (and symbol (fboundp symbol) (not (macro-function symbol)) (not (special-operator-p symbol)))))

(defun element-index (elements line column)
  "The index in ELEMENTS (from LIST-ELEMENTS) of the one starting at (LINE, COLUMN)."
  (position-if (lambda (e) (and (= (first e) line) (= (second e) column))) elements))

(defun list-head-name (syntax elements)
  "The base name of the symbol heading a list with ELEMENTS, or nil."
  (let ((head (first elements)))
    (and head (eq (token-type (third head)) :symbol)
         (values (symbol-base-name (token-text syntax (first head) (third head)))
                 (token-text syntax (first head) (third head))))))

(defun place-in-parent (syntax line column)
  "For the list or atom starting at (LINE, COLUMN): the elements of the list
around it, its index there, and that list's position, or nil at top level."
  (multiple-value-bind (pl pc) (up-list-position syntax line column)
    (when pl
      (multiple-value-bind (ol oi) (open-place syntax pl pc)
        (when ol
          (let ((elements (list-elements syntax ol oi)))
            (values elements (element-index elements line column) pl pc
                    (quoted-open-p syntax ol oi))))))))

(defun inner-place-from (syntax line column)
  "If the list at (LINE, COLUMN) is one of *inner-forms*, where its forms start."
  (multiple-value-bind (outer index ol oc) (place-in-parent syntax line column)
    (when (and outer index)
      (or (let ((head (list-head-name syntax outer)))
            (and head (inner-form-from head index :list)))
          ;; A binding: its parent is the binding list.
          (multiple-value-bind (outer2 index2) (place-in-parent syntax ol oc)
            (let ((head (and outer2 (list-head-name syntax outer2))))
              (and head index2 (inner-form-from head index2 :bindings))))))))

(defun evaluated-place-p (syntax line column function-p)
  "True if the list opened at (LINE, COLUMN) is in a place that is evaluated.
FUNCTION-P says whether a name (as written) is a function in the image."
  (multiple-value-bind (elements index pl pc quoted) (place-in-parent syntax line column)
    (cond ((null pl) t)                 ; a top-level form
          ((or quoted (null index)) nil)
          (t (let ((from (inner-place-from syntax pl pc)))
               (if from
                   (>= index from)
                   (multiple-value-bind (base name) (list-head-name syntax elements)
                     (when (and base (plusp index))
                       (let ((from (gethash base *evaluated-arguments*)))
                         (cond ((consp from) (= index (first from)))
                               (from (>= index from))
                               ((cl-operator-p base) nil)
                               ((cl-function-p base) t)
                               (t (funcall function-p name))))))))))))

(defun local-function-names (syntax line)
  "The names of the local functions (FLET, LABELS, MACROLET) defined in the
top-level form around LINE, in lower case without package."
  (multiple-value-bind (sl sc el) (toplevel-form-bounds syntax line 0)
    (declare (ignore sc))
    (let ((names '()))
      (when sl
        (loop for l from sl to el
              do (loop for tk across (line-tokens syntax l)
                       for i from 0
                       when (and (eq (token-type tk) :symbol) (eq (token-subtype tk) :head)
                                 (member (symbol-base-name (token-text syntax l tk))
                                         '("flet" "labels" "macrolet") :test #'string=))
                         do (multiple-value-bind (bl bc) (down-list-position syntax l (token-end tk))
                              (when bl
                                ;; BL BC is just inside the binding list.
                                (multiple-value-bind (ol oi) (open-place syntax bl (1- bc))
                                  (dolist (binding (and ol (list-elements syntax ol oi)))
                                    (when (eq (token-type (third binding)) :open)
                                      (multiple-value-bind (dl di) (open-place syntax (first binding) (second binding))
                                        (let ((name (list-head-name syntax (list-elements syntax dl di))))
                                          (when name (push name names))))))))))))
      names)))

(defun surely-called-p (syntax line token function-p &optional local-names)
  "True if TOKEN, a head symbol on LINE, is called as a function: its list
is evaluated, and it is not one of LOCAL-NAMES."
  (and (eq (token-subtype token) :head)
       (not (member (symbol-base-name (token-text syntax line token)) local-names :test #'string=))
       (let ((open (find-if (lambda (tk) (and (eq (token-type tk) :open) (< (token-start tk) (token-start token))))
                            (line-tokens syntax line) :from-end t)))
         (and open
              (not (quoted-open-p syntax line (position open (line-tokens syntax line))))
              (evaluated-place-p syntax line (token-start open) function-p)))))
