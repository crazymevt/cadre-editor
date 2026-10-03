;;;; refactor.lisp — extracting a function or a variable from Lisp source
;;;;
;;;; Both work on the text of one top-level form, read into a tree of
;;;; nodes: (:list start end children) and (:atom start end text), offsets
;;;; into the text. Comments are left out; quotes and other prefixes stay
;;;; atoms of their own. Like the paredit operations, each returns edits
;;;; for APPLY-EDITS rather than changing the text.

(in-package #:cadre)

(defun node-kind (node) (first node))
(defun node-start (node) (second node))
(defun node-end (node) (third node))
(defun node-children (node) (fourth node))
(defun node-text (node) (fourth node))

(defun read-form-tree (string &optional (offset 0))
  "The top-level nodes of STRING (Lisp source), with offsets plus OFFSET."
  (let ((stack (list (list :list offset nil '())))
        (state '(:code 0))
        (line-start 0))
    (dolist (line (split-text-lines string))
      (multiple-value-bind (tokens end) (lex-line line state)
        (setf state end)
        (dolist (tk tokens)
          (let ((start (+ offset line-start (token-start tk)))
                (end (+ offset line-start (token-end tk))))
            (case (token-type tk)
              (:comment nil)
              (:open (push (list :list start nil '()) stack))
              (:close (when (rest stack)
                        (let ((node (pop stack)))
                          (setf (third node) end
                                (fourth node) (nreverse (fourth node)))
                          (push node (fourth (first stack))))))
              (t (push (list :atom start end (subseq line (token-start tk) (token-end tk)))
                       (fourth (first stack))))))))
      (incf line-start (1+ (length line))))
    (nreverse (fourth (car (last stack))))))

(defun atom-name (node)
  (and (eq (node-kind node) :atom) (string-downcase (node-text node))))

(defun symbol-node-p (node)
  (and (eq (node-kind node) :atom)
       (let ((text (node-text node)))
         (and (plusp (length text))
              (symbol-constituent-p (char text 0))
              (not (digit-char-p (char text 0)))
              (not (char= (char text 0) #\:))
              (not (member (char text 0) '(#\" #\# #\' #\` #\,)))))))

(defun node-contains-p (node start end)
  (and (<= (node-start node) start) (<= end (node-end node))))

(defun enclosing-nodes (nodes start end)
  "The lists containing START–END, outermost first."
  (let ((node (find-if (lambda (n) (and (eq (node-kind n) :list) (node-contains-p n start end)
                                        (not (and (= (node-start n) start) (= (node-end n) end)))))
                       nodes)))
    (and node (cons node (enclosing-nodes (node-children node) start end)))))

;;; Variables bound around a place

(defun lambda-list-variables (node)
  "The variables a lambda list (a :list node) binds."
  (when (eq (node-kind node) :list)
    (loop for element in (node-children node)
          append (cond ((symbol-node-p element)
                        (unless (char= (char (node-text element) 0) #\&) (list (atom-name element))))
                       ((eq (node-kind element) :list)
                        ;; (var default), (var type) in defmethod, ((:key var) default)
                        (let ((first (first (node-children element))))
                          (cond ((null first) nil)
                                ((symbol-node-p first) (list (atom-name first)))
                                ((eq (node-kind first) :list)
                                 (let ((v (second (node-children first))))
                                   (and v (symbol-node-p v) (list (atom-name v))))))))))))

(defun binding-list-variables (node)
  "The variables a LET-style binding list binds: x or (x value)."
  (when (eq (node-kind node) :list)
    (loop for element in (node-children node)
          for var = (if (eq (node-kind element) :list) (first (node-children element)) element)
          when (and var (symbol-node-p var)) collect (atom-name var))))

(defparameter *lambda-list-forms* '("defun" "defmacro" "lambda" "defgeneric" "define-compiler-macro")
  "Forms whose lambda list comes after the head (and a name, except for lambda).")

(defun form-bindings (form inside)
  "The variables FORM (a :list node) binds for the code in its child INSIDE."
  (let* ((children (node-children form))
         (head (atom-name (first children)))
         (after-head (rest children)))
    (flet ((body-p (n) (and inside (member inside (nthcdr n children)))))
      (cond
        ((null head) nil)
        ((string= head "lambda") (and (body-p 2) (lambda-list-variables (second children))))
        ((member head '("defun" "defmacro" "define-compiler-macro") :test #'string=)
         (and (body-p 3) (lambda-list-variables (third children))))
        ((string= head "defmethod")
         (let ((list (find :list (cddr children) :key #'node-kind)))
           (and list (member inside (rest (member list children))) (lambda-list-variables list))))
        ((member head '("let" "let*" "symbol-macrolet") :test #'string=)
         (and (body-p 2) (binding-list-variables (second children))))
        ((member head '("destructuring-bind" "multiple-value-bind") :test #'string=)
         (and (body-p 3) (lambda-list-variables (second children))))
        ((member head '("dolist" "dotimes" "do-symbols" "do-external-symbols" "do-all-symbols") :test #'string=)
         (let ((spec (second children)))
           (and (body-p 2) spec (eq (node-kind spec) :list)
                (let ((v (first (node-children spec)))) (and v (symbol-node-p v) (list (atom-name v)))))))
        ((member head '("do" "do*") :test #'string=)
         (and (body-p 2) (binding-list-variables (second children))))
        ((member head '("flet" "labels" "macrolet") :test #'string=)
         ;; Inside one of the local functions: its lambda list.
         (let ((definitions (second children)))
           (and definitions (eq (node-kind definitions) :list) (eq inside definitions) nil)))
        ((string= head "handler-case")
         (and (member inside (cddr children)) (eq (node-kind inside) :list)
              (let ((spec (second (node-children inside))))
                (and spec (lambda-list-variables spec)))))
        ((string= head "loop")
         (loop for (keyword var) on after-head
               when (and (member (atom-name keyword) '("for" "as" "with") :test #'equal)
                         var (symbol-node-p var))
                 collect (atom-name var)))
        ((and (> (length head) 5) (string= "with-" head :end2 5))
         (let ((spec (second children)))
           (and (body-p 2) spec (eq (node-kind spec) :list)
                (let ((v (first (node-children spec)))) (and v (symbol-node-p v) (list (atom-name v)))))))
        (t nil)))))

(defun local-function-variables (form inside)
  "In FLET or LABELS, the lambda list variables of the local function INSIDE."
  (let ((head (atom-name (first (node-children form)))))
    (when (and head (member head '("flet" "labels" "macrolet") :test #'string=)
               (eq (node-kind inside) :list)
               (member inside (node-children (second (node-children form)))))
      (let ((lambda-list (second (node-children inside))))
        (and lambda-list (lambda-list-variables lambda-list))))))

(defun bound-variables-at (nodes start end)
  "The variables bound around START–END by the forms that enclose it."
  (let ((chain (enclosing-nodes nodes start end)))
    (remove-duplicates
     (loop for (form inside) on chain
           for child = (or inside (find-if (lambda (n) (node-contains-p n start end)) (node-children form)))
           append (form-bindings form child)
           append (and child (local-function-variables form child))
           ;; The local function's own definition list is two levels down.
           append (let ((grand (and inside (second (member inside chain)))))
                    (and grand (local-function-variables inside grand))))
     :test #'string=)))

(defun symbols-in (nodes)
  "The names of the symbols in NODES, in order of first appearance."
  (let ((names '()))
    (labels ((walk (node)
               (if (eq (node-kind node) :list)
                   (mapc #'walk (node-children node))
                   (when (symbol-node-p node) (pushnew (atom-name node) names :test #'string=)))))
      (mapc #'walk nodes))
    (nreverse names)))

;;; The refactorings

(defun toplevel-node-at (text start end)
  "The top-level form of TEXT containing START–END, and the tree of TEXT."
  (let* ((tree (read-form-tree text))
         (node (find-if (lambda (n) (node-contains-p n start end)) tree)))
    (values node tree)))

(defun trim-region (text start end)
  "START–END without the whitespace around it."
  (loop while (and (< start end) (whitespace-char-p (char text start))) do (incf start))
  (loop while (and (> end start) (whitespace-char-p (char text (1- end)))) do (decf end))
  (values start end))

(defun extract-function-edits (text start end name)
  "Edits that move the code from START to END of TEXT (Lisp source) into a
new function NAME, defined before the top-level form, and call it there.
The new function's parameters are the variables bound around the code that
it uses. Also returns the parameters."
  (multiple-value-bind (start end) (trim-region text start end)
    (when (= start end) (editor-error "Select the code to extract"))
    (multiple-value-bind (top tree) (toplevel-node-at text start end)
      (unless top (editor-error "The selection must be inside one top-level form"))
      (let* ((selected (nodes-within tree start end))
             (bound (bound-variables-at tree start end))
             (used (symbols-in selected))
             (params (remove-if-not (lambda (s) (member s bound :test #'string=)) used))
             (code (subseq text start end))
             (indented (with-output-to-string (out)
                         (loop for line in (split-text-lines code)
                               for first = t then nil
                               do (unless first (format out "~%"))
                                  (format out "~:[~;  ~]~a" (not first) line))))
             (definition (format nil "(defun ~a (~{~a~^ ~})~%  ~a)~%~%" name params indented))
             (call (format nil "(~a~{ ~a~})" name params)))
        (values (list (list start end call)
                      (list (node-start top) (node-start top) definition))
                params)))))

(defun nodes-within (nodes start end)
  "The outermost nodes among NODES (and inside them) lying within START–END."
  (loop for n in nodes
        append (cond ((and (<= start (node-start n)) (<= (node-end n) end)) (list n))
                     ((eq (node-kind n) :list) (nodes-within (node-children n) start end)))))

(defun extract-variable-edits (text start end name)
  "Edits that bind the expression from START to END of TEXT to NAME with a
LET around the form containing it, using NAME in its place."
  (multiple-value-bind (start end) (trim-region text start end)
    (when (= start end) (editor-error "Select the expression to extract"))
    (let* ((tree (read-form-tree text))
           (chain (enclosing-nodes tree start end))
           (form (car (last chain))))
      (unless form (editor-error "The expression must be inside a form"))
      (values (list (list (node-end form) (node-end form) ")")
                    (list start end name)
                    (list (node-start form) (node-start form)
                          (format nil "(let ((~a ~a))~%" name (subseq text start end))))
              form))))
