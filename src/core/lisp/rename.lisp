;;;; rename.lisp — what renaming a Common Lisp symbol should change
;;;;
;;;; Two kinds of rename. A local variable or function (bound by LET, a
;;;; lambda list, DOLIST, FLET and the like around the cursor) is renamed
;;;; where it refers to that binding: in its form, not where an inner binding
;;;; of the same name shadows it, nor elsewhere. Any other symbol is renamed
;;;; across the project's files, but only where the occurrence is that
;;;; symbol: each one is put in its package (from its prefix, or the
;;;; IN-PACKAGE it follows) and resolved to the symbol's home package, by
;;;; the running Lisp when one is connected. Local variables of the same
;;;; name, and DEFPACKAGE clauses that name it, are told apart, and what
;;;; can't be decided is marked so for the user to judge.
;;;;
;;;; Positions are offsets into the text; read-form-tree (refactor.lisp)
;;;; gives the forms.

(in-package #:cadre)

;;; Binding forms. Each gives groups of (SITES . SCOPE): the symbol nodes
;;; that bind names, and the nodes in which those names refer to them.

(defun child-list (node) (and node (eq (node-kind node) :list) (node-children node)))

(defun lambda-list-groups (node scope &key destructuring)
  "Binding groups for the lambda list NODE whose body is SCOPE (a list of
nodes): each parameter is seen by the body and by the default values of the
parameters after it (&optional (b a): a is seen there), its supplied-p
variable likewise. With DESTRUCTURING (DEFMACRO, DESTRUCTURING-BIND), a
required parameter written as a list is a pattern of variables; otherwise
it is (var specializer), as in DEFMETHOD."
  (let ((elements (child-list node)) (optional nil) (groups '()))
    (loop for (element . later) on elements
          for defaults = (loop for e in later
                               when (eq (node-kind e) :list) append (rest (child-list e)))
          do (cond
               ((and (symbol-node-p element) (char= (char (node-text element) 0) #\&))
                (setf optional t))
               ((symbol-node-p element)
                (push (list (list element) (append defaults scope) t :variable) groups))
               ((eq (node-kind element) :list)
                (let* ((parts (child-list element))
                       (first (first parts))
                       (sites (cond ((and (not optional) destructuring) (destructuring-site-nodes element))
                                    ((null first) nil)
                                    ((symbol-node-p first) (list first))
                                    ((eq (node-kind first) :list)
                                     (let ((v (second (child-list first)))) (and v (symbol-node-p v) (list v))))))
                       (supplied (and optional (third parts) (symbol-node-p (third parts)) (list (third parts)))))
                  (when (or sites supplied)
                    (push (list (append sites supplied) (append defaults scope) t :variable) groups))))))
    ;; Later parameters first, so they shadow earlier ones.
    groups))

(defun binding-site-node (element)
  "The variable a LET-style binding ELEMENT binds: x or (x value)."
  (let ((var (if (eq (node-kind element) :list) (first (node-children element)) element)))
    (and var (symbol-node-p var) var)))

(defun destructuring-site-nodes (node)
  "The symbols a destructuring pattern binds, nested lists included."
  (cond ((symbol-node-p node)
         (unless (char= (char (node-text node) 0) #\&) (list node)))
        ((eq (node-kind node) :list) (mapcan #'destructuring-site-nodes (node-children node)))
        (t nil)))

(defparameter *with-macros-binding-all* '("with-slots" "with-accessors")
  "WITH- forms whose first argument lists several bindings, var or (var name).")

(defun binding-groups (form)
  "The bindings FORM (a :list node) makes: a list of (SITES SCOPE CERTAIN
NAMESPACE), SITES the symbol nodes binding names, SCOPE the nodes where those
names refer to them, CERTAIN false when FORM is only guessed to bind (a WITH-
macro), and NAMESPACE :function for local functions' names, else :variable."
  (let* ((children (child-list form))
         (head (and (first children) (atom-name (first children)))))
    (flet ((group (sites scope &optional (certain t) (namespace :variable))
             (and sites (list (list sites scope certain namespace)))))
      (cond
        ((null head) nil)
        ((string= head "lambda") (lambda-list-groups (second children) (cddr children)))
        ((string= head "defun") (lambda-list-groups (third children) (cdddr children)))
        ((member head '("defmacro" "define-compiler-macro") :test #'string=)
         (lambda-list-groups (third children) (cdddr children) :destructuring t))
        ((string= head "defmethod")
         (let ((lambda-list (find :list (cddr children) :key #'node-kind)))
           (lambda-list-groups lambda-list (rest (member lambda-list children)))))
        ((member head '("let" "symbol-macrolet") :test #'string=)
         (group (remove nil (mapcar #'binding-site-node (child-list (second children)))) (cddr children)))
        ((member head '("let*" "do*") :test #'string=)
         ;; Each binding is seen by the later bindings' values and the body.
         ;; Later bindings come first, so they shadow earlier ones.
         (let ((bindings (child-list (second children))))
           (reverse
            (loop for (binding . later) on bindings
                  for site = (binding-site-node binding)
                  when site
                    collect (list (list site)
                                  (append (loop for b in later
                                                when (eq (node-kind b) :list) append (rest (node-children b)))
                                          (and (string= head "do*")
                                               (eq (node-kind binding) :list) (cddr (node-children binding)))
                                          (cddr children))
                                  t :variable)))))
        ((string= head "do")
         (let ((bindings (child-list (second children))))
           (group (remove nil (mapcar #'binding-site-node bindings))
                  (append (loop for b in bindings
                                when (eq (node-kind b) :list) append (cddr (node-children b)))
                          (cddr children)))))
        ((string= head "destructuring-bind")
         (lambda-list-groups (second children) (cdddr children) :destructuring t))
        ((string= head "multiple-value-bind")
         (group (destructuring-site-nodes (second children)) (cdddr children)))
        ((member head '("dolist" "dotimes" "do-symbols" "do-external-symbols" "do-all-symbols") :test #'string=)
         (let ((spec (second children)))
           (group (let ((v (first (child-list spec)))) (and v (symbol-node-p v) (list v)))
                  (append (cddr (child-list spec)) (cddr children)))))
        ((member head '("flet" "labels" "macrolet") :test #'string=)
         (let ((definitions (child-list (second children))))
           (append
            ;; The local functions' names, in the body (and, for LABELS,
            ;; in the definitions).
            (group (loop for d in definitions
                         for name = (first (child-list d))
                         when (and name (symbol-node-p name)) collect name)
                   (append (cddr children) (and (string= head "labels") definitions))
                   t :function)
            ;; Each one's parameters, in its body.
            (loop for d in definitions
                  for parts = (child-list d)
                  append (lambda-list-groups (second parts) (cddr parts)
                                             :destructuring (string= head "macrolet"))))))
        ((member head '("handler-case" "restart-case") :test #'string=)
         (loop for clause in (cddr children)
               for parts = (child-list clause)
               append (lambda-list-groups (second parts) (cddr parts))))
        ((string= head "loop")
         (group (loop for (keyword var) on (rest children)
                      when (and (member (atom-name keyword) '("for" "as" "with") :test #'equal) var)
                        append (destructuring-site-nodes var))
                (rest children)))
        ((member head *with-macros-binding-all* :test #'string=)
         (group (remove nil (mapcar #'binding-site-node (child-list (second children)))) (cddr children) nil))
        ((and (> (length head) 5) (string= "with-" head :end2 5))
         (let ((v (first (child-list (second children)))))
           (group (and v (symbol-node-p v) (list v)) (cddr children) nil)))
        (t nil)))))

(defun node-within-p (node container)
  (and (<= (node-start container) (node-start node)) (<= (node-end node) (node-end container))))

(defun unqualified-symbol-p (node)
  (and (symbol-node-p node) (not (find #\: (node-text node)))))

(defparameter *case-heads* '("case" "ecase" "ccase" "typecase" "etypecase" "ctypecase")
  "Forms whose clauses start with keys or types, which aren't evaluated.")

(defun list-head (node) (let ((first (first (child-list node)))) (and first (atom-name first))))

(defun element-before (parent child)
  "The element of PARENT just before CHILD, or nil."
  (loop for (a b) on (child-list parent) when (eq b child) return a))

(defun prefix-atom-p (node text)
  (and node (eq (node-kind node) :atom) (string= (node-text node) text)))

(defun data-position-p (chain node)
  "Whether NODE, inside the lists of CHAIN (outermost first), is data rather
than code: quoted ('x, '(x), (quote x)), in a backquote but not unquoted
(`(x ,y): x), or a CASE clause's keys."
  (let ((quoted nil) (backquoted nil))
    (loop for (parent child) on (append chain (list node))
          for grandparent = nil then previous
          for previous = parent
          while child
          do (let ((before (element-before parent child)))
               (cond ((prefix-atom-p before "'") (setf quoted t))
                     ((prefix-atom-p before "`") (setf backquoted t))
                     ((and backquoted (or (prefix-atom-p before ",") (prefix-atom-p before ",@")))
                      (setf backquoted nil quoted nil)))
               (when (and (equal (list-head parent) "quote") (not (eq child (first (child-list parent)))))
                 (setf quoted t))
               ;; (case x (key …)): the key, or keys, at the head of a clause.
               (when (and grandparent (member (list-head grandparent) *case-heads* :test #'equal)
                          (member parent (cddr (child-list grandparent)))
                          (eq child (first (child-list parent))))
                 (setf quoted t))))
    (or quoted backquoted)))

(defun reference-namespace (chain node)
  "Whether NODE, with CHAIN the lists around it (outermost first), names a
function (first in a list, or after #') or a variable. The first element of
a COND clause, or of DO's end clause, is a test: a variable."
  (let* ((parent (car (last chain)))
         (grandparent (car (last chain 2)))
         (siblings (child-list parent)))
    (cond ((prefix-atom-p (element-before parent node) "#'") :function)
          ((not (eq node (first siblings))) :variable)
          ((and grandparent (not (eq grandparent parent))
                (or (and (equal (list-head grandparent) "cond") (member parent (rest (child-list grandparent))))
                    (and (member (list-head grandparent) '("do" "do*") :test #'equal)
                         (eq parent (third (child-list grandparent))))))
           :variable)
          (t :function))))

(defun lexical-binding (tree node)
  "The site binding NODE (an unqualified symbol node in TREE, a list of
top-level nodes), if it refers to a local variable or function: (values SITE
CERTAIN). SITE is NODE itself when NODE is where it is bound. A use in a
function's place refers only to local functions, other uses only to
variables."
  (when (unqualified-symbol-p node)
    (let* ((name (atom-name node))
           (chain (enclosing-nodes tree (node-start node) (node-end node)))
           (namespace (reference-namespace chain node)))
      ;; Quoted symbols are data: renaming a variable leaves them alone.
      (when (data-position-p chain node)
        (return-from lexical-binding nil))
      (dolist (form (reverse chain))
        (dolist (group (binding-groups form))
          (destructuring-bind (sites scope certain group-namespace) group
            (when (member node sites :test #'eq)
              (return-from lexical-binding (values node certain)))
            (let ((site (and (eq namespace group-namespace)
                             (find name sites :key #'atom-name :test #'string=))))
              (when (and site (some (lambda (s) (node-within-p node s)) scope))
                (return-from lexical-binding (values site certain))))))))))

(defun symbol-nodes (tree)
  "Every symbol atom in TREE, in order."
  (let ((nodes '()))
    (labels ((walk (node)
               (if (eq (node-kind node) :list)
                   (mapc #'walk (node-children node))
                   (when (symbol-node-p node) (push node nodes)))))
      (mapc #'walk tree))
    (nreverse nodes)))

(defun atom-node-at (tree position)
  "The atom of TREE at or just before POSITION."
  (find-if (lambda (n) (<= (node-start n) position (node-end n))) (symbol-nodes tree)))

(defun lexical-rename-places (text position)
  "If the symbol at POSITION in TEXT is a local variable or function, where
renaming it changes: (values PLACES OTHERS NAME CERTAIN). PLACES are the
(start end) of its binding and the uses that refer to it; OTHERS those of
the same name in its top-level form that are another binding's or not bound
there. Nil if it isn't bound locally."
  (let* ((tree (read-form-tree text))
         (node (atom-node-at tree position)))
    (when node
      (multiple-value-bind (site certain) (lexical-binding tree node)
        (when site
          (let ((top (find-if (lambda (n) (node-within-p site n)) tree))
                (places '()) (others '()))
            (dolist (n (symbol-nodes (list top)))
              (when (and (unqualified-symbol-p n) (string= (atom-name n) (atom-name site)))
                (if (eq (lexical-binding tree n) site)
                    (push (list (node-start n) (node-end n)) places)
                    (push (list (node-start n) (node-end n)) others))))
            (values (nreverse places) (nreverse others) (node-text site) certain)))))))

;;; Occurrences across files

(defstruct (symbol-context (:conc-name sc-))
  "An occurrence of a symbol's name in a file. START and END are the name's
offsets, without its prefix: what a rename replaces."
  line start end column prefix package local local-certain clause clause-package)

(defun designator-text-name (text)
  "The package name the designator TEXT (:foo, #:foo, \"FOO\", foo) names."
  (package-designator-name text))

(defparameter *defpackage-heads* '("defpackage" "define-package" "uiop:define-package" "uiop/package:define-package"))

(defun defpackage-clause (tree node)
  "If NODE is inside a DEFPACKAGE form of TREE, (values CLAUSE PACKAGE):
CLAUSE the keyword heading its clause (\":export\" …), PACKAGE the package
the occurrence's name belongs to there (for :import-from, the one it comes
from; otherwise the package being defined)."
  (let ((top (find-if (lambda (n) (node-within-p node n)) tree)))
    (when (and top (eq (node-kind top) :list))
      (let* ((children (node-children top))
             (head (and (first children) (atom-name (first children)))))
        (when (and head (member head *defpackage-heads* :test #'string=) (second children))
          (let ((clause (find-if (lambda (c) (and (eq (node-kind c) :list) (node-within-p node c))) (cddr children))))
            (when clause
              (let* ((parts (node-children clause))
                     (keyword (and (first parts) (eq (node-kind (first parts)) :atom)
                                   (string-downcase (node-text (first parts))))))
                (values keyword
                        (if (member keyword '(":import-from" ":shadowing-import-from") :test #'equal)
                            (and (second parts) (designator-text-name (node-text (second parts))))
                            (designator-text-name (node-text (second children)))))))))))))

(defparameter *symbol-naming-clauses* '(":export" ":shadow" ":intern" ":import-from" ":shadowing-import-from")
  "DEFPACKAGE clauses that name symbols; the others name packages or say other things.")

(defun package-name-position-p (tree node)
  "Whether NODE names a package, not a symbol: what IN-PACKAGE or DEFPACKAGE
is given, an :import-from clause's package, or anything in a DEFPACKAGE
clause that doesn't name symbols (:use, :nicknames, :local-nicknames…)."
  (let ((top (find-if (lambda (n) (node-within-p node n)) tree)))
    (when (and top (eq (node-kind top) :list))
      (let* ((children (node-children top))
             (head (and (first children) (atom-name (first children)))))
        (cond ((null head) nil)
              ((string= "in-package" (symbol-base-name (node-text (first children))))
               (eq node (second children)))
              ((member head *defpackage-heads* :test #'string=)
               (or (eq node (second children))
                   (let* ((clause (find-if (lambda (c) (and (eq (node-kind c) :list) (node-within-p node c)))
                                           (cddr children)))
                          (parts (child-list clause))
                          (keyword (and (first parts) (eq (node-kind (first parts)) :atom)
                                        (string-downcase (node-text (first parts))))))
                     (and clause
                          (or (not (member keyword *symbol-naming-clauses* :test #'equal))
                              (and (member keyword '(":import-from" ":shadowing-import-from") :test #'equal)
                                   (eq node (second parts))))))))
              (t nil))))))

(defun in-package-positions (tree)
  "(start . package-name) of each top-level IN-PACKAGE in TREE, in order."
  (loop for node in tree
        for children = (child-list node)
        when (and children (atom-name (first children))
                  (string= "in-package" (symbol-base-name (node-text (first children))))
                  (second children) (eq (node-kind (second children)) :atom))
          collect (cons (node-start node) (designator-text-name (node-text (second children))))))

(defun name-nodes (tree)
  "Every atom of TREE that names a symbol: foo, pkg:foo, :foo, #:foo."
  (let ((nodes '()))
    (labels ((walk (node)
               (if (eq (node-kind node) :list)
                   (mapc #'walk (node-children node))
                   (let ((text (node-text node)))
                     (when (and (plusp (length text))
                                (or (symbol-node-p node)
                                    (and (> (length text) 1) (char= (char text 0) #\:))
                                    (and (> (length text) 2) (string= "#:" text :end2 2))))
                       (push node nodes))))))
      (mapc #'walk tree))
    (nreverse nodes)))

(defun symbol-contexts (text name)
  "Each occurrence of the symbol NAME (its name; prefixes aside) in TEXT,
Common Lisp source, as a symbol-context: where it is, its prefix, the
package it is read in, whether it is a local binding, and its DEFPACKAGE
clause if it is in one."
  (let* ((base (subseq name (symbol-name-part name)))
         (tree (read-form-tree text))
         (packages (in-package-positions tree))
         (starts (coerce (line-starts text) 'vector)))
    (loop for node in (name-nodes tree)
          for token = (node-text node)
          for offset = (symbol-name-part token)
          ;; A keyword is a symbol of its own, except as a DEFPACKAGE
          ;; clause's way of naming one.
          when (and (string-equal base token :start2 offset)
                    (not (package-name-position-p tree node))
                    (or (string/= (subseq token 0 offset) ":") (defpackage-clause tree node)))
            collect (let* ((start (+ (node-start node) offset))
                           (line (1- (or (position-if (lambda (s) (> s start)) starts) (length starts)))))
                      (multiple-value-bind (site certain) (lexical-binding tree node)
                        (multiple-value-bind (clause clause-package) (defpackage-clause tree node)
                          (make-symbol-context
                           :line line :start start :end (node-end node) :column (- start (aref starts line))
                           :prefix (subseq token 0 offset)
                           ;; Before any IN-PACKAGE, a file loads in CL-USER.
                           :package (or (cdr (find-if (lambda (p) (< (car p) (node-start node))) packages :from-end t))
                                        "COMMON-LISP-USER")
                           :local (and site t) :local-certain certain
                           :clause clause :clause-package clause-package)))))))

(defun prefix-package (prefix)
  "The package a symbol prefix (\"foo:\", \"foo::\") names, or nil for none
or one that isn't a package (\"#:\", \":\")."
  (let ((name (string-right-trim ":" prefix)))
    (and (plusp (length name)) (not (string= prefix "#:")) (string-upcase name))))

(defun classify-symbol-context (context target-home resolve)
  "How CONTEXT's occurrence relates to the symbol being renamed, whose home
package is TARGET-HOME: (values KIND NOTE). RESOLVE takes a package name and
gives the home package of the symbol of the renamed name read there, or nil
if it can't be told. KIND is :same (rename it), :export (a DEFPACKAGE
clause naming it), :local (a local variable or function of that name),
:other (another symbol of that name) or :unknown."
  (cond
    ((sc-clause context)
     (let ((clause (sc-clause context)) (package (sc-clause-package context)))
       (cond ((not (member clause '(":export" ":shadow" ":intern" ":import-from" ":shadowing-import-from")
                           :test #'string=))
              (values :other (format nil "~a in a DEFPACKAGE" clause)))
             ((and target-home package (string= (or (funcall resolve package) package) target-home))
              (values :export (format nil "~a of ~a" clause package)))
             ((and target-home package) (values :other (format nil "~a of ~a" clause package)))
             (t (values :unknown (format nil "~a: can't tell which package" clause))))))
    ((sc-local context)
     (values :local (if (sc-local-certain context) "local binding" "local binding (guessed)")))
    ((string= (sc-prefix context) "#:") (values :unknown "uninterned name"))
    (t
     (let* ((package (or (prefix-package (sc-prefix context)) (sc-package context)))
            (home (and package (funcall resolve package))))
       (cond ((null target-home) (values :unknown "can't tell which package"))
             ((null package) (values :unknown "no IN-PACKAGE before it"))
             ((null home) (values :unknown (format nil "in ~a: can't tell" package)))
             ((string= home target-home) (values :same nil))
             (t (values :other (format nil "~a's symbol" home))))))))

(defun context-packages (contexts)
  "The packages CONTEXTS' occurrences are read in, whose symbols of the
renamed name must be resolved to classify them."
  (remove-duplicates
   (loop for context in contexts
         unless (or (sc-local context) (string= (sc-prefix context) "#:"))
           append (remove nil (list (or (prefix-package (sc-prefix context)) (sc-package context))
                                    (sc-clause-package context))))
   :test #'string=))
