;;;; breakpoints.lisp — where a breakpoint on a line stops, in Common Lisp
;;;;
;;;; Cadre's breakpoints are compiled in: the top-level form is compiled
;;;; again with (break …) before the statement the breakpoint's line is in
;;;; (ui/breakpoints.lisp). A statement is an element of a body: the forms
;;;; of a DEFUN, LET, WHEN, a COND clause, a macro with &body and so on,
;;;; after any docstring and declarations. A line that starts a statement
;;;; stops there; a line inside one stops at the innermost statement around
;;;; it; the first line of a definition, or its docstring or declarations,
;;;; stops at the first statement of its body.
;;;;
;;;; A few forms are statements without being in a body, such as the
;;;; protected form of UNWIND-PROTECT: a break in front of it would become
;;;; the protected form, so those are wrapped, (progn (break …) form).

(in-package #:cadre)

(defparameter *body-starts*
  '(("progn" . 1) ("prog1" . 2) ("prog2" . 3) ("locally" . 1) ("block" . 2) ("catch" . 2)
    ("tagbody" . 1) ("unwind-protect" . 2) ("let" . 2) ("let*" . 2) ("flet" . 2) ("labels" . 2)
    ("macrolet" . 2) ("symbol-macrolet" . 2) ("when" . 2) ("unless" . 2) ("dolist" . 2)
    ("dotimes" . 2) ("do" . 3) ("do*" . 3) ("lambda" . 2) ("defun" . 3) ("defmacro" . 3)
    ("define-compiler-macro" . 3) ("handler-bind" . 2) ("restart-bind" . 2)
    ("multiple-value-bind" . 3) ("destructuring-bind" . 3) ("eval-when" . 2) ("progv" . 3)
    ("with-slots" . 3) ("with-accessors" . 3) ("with-open-file" . 2) ("with-open-stream" . 2)
    ("with-output-to-string" . 2) ("with-input-from-string" . 2) ("with-simple-restart" . 2)
    ("with-standard-io-syntax" . 1) ("with-compilation-unit" . 2)
    ("print-unreadable-object" . 2))
  "Forms whose elements from this index on are statements.")

(defparameter *wrapped-statements*
  '(("unwind-protect" . 1) ("prog1" . 1) ("multiple-value-prog1" . 1))
  "Forms whose element at this index is evaluated but not in a body: a
break there must wrap it.")

(defparameter *clause-body-starts*
  '(("cond" 1 . 1) ("case" 2 . 1) ("ccase" 2 . 1) ("ecase" 2 . 1) ("typecase" 2 . 1)
    ("ctypecase" 2 . 1) ("etypecase" 2 . 1) ("handler-case" 2 . 2) ("restart-case" 2 . 2))
  "Forms of clauses: (head first-clause-index . clause-body-start).")

(defparameter *documented-heads*
  '("defun" "defmacro" "defmethod" "define-compiler-macro" "lambda")
  "Forms whose body may start with a docstring.")

(defun element-head-name (syntax element)
  "ELEMENT's name in lower case without a package, if it's a symbol."
  (destructuring-bind (line column token) element
    (declare (ignore column))
    (and (eq (token-type token) :symbol)
         (symbol-base-name (token-text syntax line token)))))

(defun list-at (syntax line column)
  "The elements of the list opened at (LINE, COLUMN), or nil if there's no
open paren there or it's quoted data."
  (multiple-value-bind (l i) (open-place syntax line column)
    (and l (not (quoted-open-p syntax l i))
         (values (list-elements syntax l i) t))))

(defun skip-preamble (syntax elements start documented)
  "START moved past declarations, and a docstring if DOCUMENTED, that
aren't the last element."
  (loop while (< (1+ start) (length elements))
        do (let* ((element (nth start elements))
                  (token (third element)))
             (cond ((and documented (eq (token-type token) :string)) (setf documented nil))
                   ((and (eq (token-type token) :open)
                         (let ((inner (list-at syntax (first element) (second element))))
                           (and inner (equal "declare" (element-head-name syntax (first inner)))))))
                   (t (return)))
             (incf start)))
  start)

(defun body-start (syntax line column)
  "For the list opened at (LINE, COLUMN): its elements, and the index of the
first that's a statement (nil if none is)."
  (let ((elements (list-at syntax line column)))
    (when elements
      (let* ((head (element-head-name syntax (first elements)))
             (parent (multiple-value-list (up-list-position syntax line column)))
             (parent-elements (and (first parent) (list-at syntax (first parent) (second parent))))
             (parent-head (and parent-elements (element-head-name syntax (first parent-elements))))
             (index (position-if (lambda (e) (and (= (first e) line) (= (second e) column))) parent-elements))
             (clause (and parent-head (assoc parent-head *clause-body-starts* :test #'string=)))
             (start (cond ((and clause index (>= index (second clause))) (cddr clause))
                          ((and (first parent) (local-function-list-p syntax line column)) 2)
                          ((null head) nil)
                          ((string= head "defmethod")
                           (let ((lambda-list (position-if (lambda (e) (eq (token-type (third e)) :open))
                                                           elements :start 2)))
                             (and lambda-list (1+ lambda-list))))
                          ((cdr (assoc head *body-starts* :test #'string=)))
                          ((or (gethash head *learned-indentation*)
                               (and (> (length head) 5) (string= "with-" head :end2 5))
                               (and (> (length head) 3) (string= "do-" head :end2 3)))
                           (let ((spec (indentation-spec head)))
                             (and spec (1+ spec)))))))
        (when (and start (< start (length elements)))
          (values elements
                  (skip-preamble syntax elements start
                                 (or (member head *documented-heads* :test #'string=)
                                     (and (first parent) (local-function-list-p syntax line column))))))))))

(defun line-first-element (syntax line)
  "The (line column) where the first element starting on LINE starts, or nil."
  (let ((tokens (line-tokens syntax line)))
    (loop for index from 0 below (length tokens)
          for token = (aref tokens index)
          unless (or (member (token-type token) '(:comment :close :invalid))
                     (and (zerop index) (continues-from-previous-line-p syntax line index)))
            do (return (list line (token-start token))))))

(defun element-end-position (syntax element)
  "The position after ELEMENT, an entry of LIST-ELEMENTS."
  (let ((token (third element)))
    (loop for line from (first element) below (syntax-line-count syntax)
          for index = (position token (line-tokens syntax line))
          when index return (element-end syntax line index))))

(defun wrapped-statement (syntax elements index)
  "If element INDEX of the list ELEMENTS is a statement to wrap, its place
as BREAKPOINT-PLACE returns it."
  (let* ((head (and elements (element-head-name syntax (first elements))))
         (wrapped (and head (assoc head *wrapped-statements* :test #'string=)))
         (element (and wrapped (eql index (cdr wrapped)) (nth index elements))))
    (when element
      (multiple-value-bind (el ec) (element-end-position syntax element)
        (and el (list (first element) (second element) el ec))))))

(defun breakpoint-place (syntax line)
  "Where a breakpoint on LINE stops: the (line column) of the statement to
break before, or nil if LINE isn't in a body anywhere. For a statement to
wrap (*WRAPPED-STATEMENTS*), (line column end-line end-column)."
  (let ((place (line-first-element syntax line))
        (first t))
    (loop while place
          do (destructuring-bind (l c) place
               (multiple-value-bind (pl pc) (up-list-position syntax l c)
                 ;; A statement itself?
                 (when pl
                   (let* ((elements (list-at syntax pl pc))
                          (index (position-if (lambda (e) (and (= (first e) l) (= (second e) c))) elements)))
                     (let ((wrapped (wrapped-statement syntax elements index)))
                       (when wrapped (return wrapped)))
                     (let ((start (nth-value 1 (body-start syntax pl pc))))
                       (when (and start index (>= index start))
                         (return (list l c))))))
                 ;; The first line of a form with a body, or a line in a
                 ;; top-level form's preamble: its first statement.
                 (when (or first (null pl))
                   (let* ((elements (list-at syntax l c))
                          (head (and elements (element-head-name syntax (first elements))))
                          (wrapped (and head (wrapped-statement syntax elements
                                                                (cdr (assoc head *wrapped-statements* :test #'string=))))))
                     (when wrapped (return wrapped)))
                   (multiple-value-bind (elements start) (body-start syntax l c)
                     (when start
                       (let ((statement (nth start elements)))
                         (return (list (first statement) (second statement)))))))
                 (setf first nil
                       place (and pl (list pl pc))))))))

;;; Compiling with breaks in

(defun insert-at-offsets (string insertions)
  "STRING with each (offset . text) of INSERTIONS put in at its offset;
those at the same offset in the order given."
  (with-output-to-string (out)
    (let ((from 0))
      (loop for (offset . text) in (stable-sort (copy-list insertions) #'< :key #'car)
            do (write-string string out :start from :end offset)
               (write-string text out)
               (setf from offset))
      (write-string string out :start from))))

(defun unshift-position (position insertions)
  "POSITION in text that INSERTIONS (as for insert-at-offsets, with offsets
in the same coordinates) were put into, as a position in the text before."
  (let ((shift 0))
    (loop for (offset . text) in (stable-sort (copy-list insertions) #'< :key #'car)
          do (cond ((< position (+ offset shift)) (return))
                   ((< position (+ offset shift (length text))) (return-from unshift-position offset))
                   (t (incf shift (length text)))))
    (- position shift)))
