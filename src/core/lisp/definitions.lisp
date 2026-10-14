;;;; definitions.lisp — what Lisp source defines, for completion and hints
;;;;
;;;; SOURCE-DEFINITIONS reads the definitions in a file's text (DEFUN,
;;;; DEFMACRO, DEFVAR, DEFCLASS and so on, at top level or inside PROGN,
;;;; EVAL-WHEN or LET) with their lambda lists and documentation as written,
;;;; so hints work for code that hasn't been loaded. ARGLIST-HINT marks the
;;;; parameter the cursor is on, as Swank's autodoc does:
;;;;   (format destination ===> control-string <=== &rest args)

(in-package #:cadre)

(defstruct (definition (:conc-name definition-))
  name kind arglist documentation line)

(defparameter *definition-kinds*
  '(("defun" . :function) ("defmacro" . :macro) ("defgeneric" . :generic) ("defmethod" . :method)
    ("define-compiler-macro" . nil) ("define-modify-macro" . :macro)
    ("defvar" . :variable) ("defparameter" . :variable) ("defconstant" . :constant)
    ("define-symbol-macro" . :variable) ("define-option" . :variable)
    ("defclass" . :class) ("defstruct" . :class) ("define-condition" . :class) ("deftype" . :type)
    ("define-command" . :command))
  "Definition forms (by name, without package) and the kind of thing each defines.")

(defun operator-name (node)
  "The name, downcased and without its package, of the atom NODE; nil if it isn't one."
  (and node (eq (node-kind node) :atom)
       (let ((text (node-text node)))
         (string-downcase (subseq text (symbol-name-part text))))))

(defun string-literal-end (text start)
  "The offset after the string literal whose opening quote is at START."
  (loop for i from (1+ start) below (length text)
        do (case (char text i)
             (#\\ (incf i))
             (#\" (return (1+ i))))
        finally (return (length text))))

(defun string-literal-value (text start)
  (let ((end (string-literal-end text start)))
    (with-output-to-string (out)
      (loop for i from (1+ start) below (max (1+ start) (1- end))
            do (let ((c (char text i)))
                 (if (char= c #\\)
                     (progn (incf i) (when (< i (length text)) (write-char (char text i) out)))
                     (write-char c out)))))))

(defun string-node-p (node text)
  (and (eq (node-kind node) :atom) (char= (char text (node-start node)) #\")))

(defun form-elements (node text)
  "NODE's children, with each string that runs over several lines as one
element (the lexer gives a piece for each line)."
  (let ((elements '()) (skip-to -1))
    (dolist (child (node-children node))
      (when (>= (node-start child) skip-to)
        (if (string-node-p child text)
            (let ((end (string-literal-end text (node-start child))))
              (push (list :atom (node-start child) end (subseq text (node-start child) end)) elements)
              (setf skip-to end))
            (push child elements))))
    (nreverse elements)))

(defun collapse-whitespace (string)
  (with-output-to-string (out)
    (let ((space nil))
      (loop for c across string
            do (if (whitespace-char-p c)
                   (setf space t)
                   (progn (when space (write-char #\Space out) (setf space nil))
                          (write-char c out)))))))

(defun node-source (node text)
  (collapse-whitespace (subseq text (node-start node) (node-end node))))

(defun documentation-option (elements text)
  "The string in a (:documentation \"…\") among ELEMENTS."
  (loop for e in elements
        when (and (eq (node-kind e) :list)
                  (let ((items (form-elements e text)))
                    (and (first items) (eq (node-kind (first items)) :atom)
                         (string-equal (node-text (first items)) ":documentation")
                         (second items) (string-node-p (second items) text))))
          return (string-literal-value text (node-start (second (form-elements e text))))))

(defun read-definition (head kind elements text)
  "The definition made by the form ELEMENTS (whose operator is HEAD), or nil."
  (let* ((name-node (second elements))
         (name (cond ((null name-node) nil)
                     ((eq (node-kind name-node) :atom) (node-text name-node))
                     ;; (defstruct (point (:conc-name p-)) …)
                     ((member kind '(:class))
                      (let ((first (first (form-elements name-node text))))
                        (and first (eq (node-kind first) :atom) (node-text first))))
                     ;; (defun (setf foo) …)
                     (t (node-source name-node text)))))
    (when (and name kind)
      (let* ((rest (cddr elements))
             (lambda-list (case kind
                            ((:function :macro :generic :command) (first rest))
                            ;; Qualifiers such as :around come before the lambda list.
                            (:method (find :list rest :key #'node-kind))))
             (after (and lambda-list (rest (member lambda-list rest))))
             (documentation
               (case kind
                 ((:function :macro :method :command)
                  (and (rest after) (string-node-p (first after) text)
                       (string-literal-value text (node-start (first after)))))
                 (:generic (documentation-option after text))
                 ((:variable :constant)
                  (let ((doc (if (equal head "define-symbol-macro") nil (second rest))))
                    (and doc (string-node-p doc text) (string-literal-value text (node-start doc)))))
                 (:class (or (documentation-option rest text)
                             (and (equal head "defstruct") (second rest) (string-node-p (first rest) text)
                                  (string-literal-value text (node-start (first rest))))))
                 (:type (and (rest after) (string-node-p (first after) text)
                             (string-literal-value text (node-start (first after))))))))
        (make-definition :name name :kind kind
                         :arglist (and lambda-list (eq (node-kind lambda-list) :list)
                                       (node-source lambda-list text))
                         :documentation documentation
                         :line (line-at-position text (node-start name-node)))))))

(defparameter *definition-containers*
  '("progn" "eval-when" "let" "let*" "flet" "labels" "macrolet" "symbol-macrolet" "locally")
  "Forms whose bodies may hold definitions.")

(defvar *newline-positions* nil
  "While reading TEXT's definitions: TEXT and where its newlines are.")

(defun line-at-position (text position)
  "The line (from 0) of POSITION in TEXT. Within SOURCE-DEFINITIONS, a binary
search over the newlines found once, rather than counting them each time."
  (if (and *newline-positions* (eq (car *newline-positions*) text))
      (let* ((newlines (cdr *newline-positions*))
             (low 0) (high (length newlines)))
        ;; The number of newlines before POSITION.
        (loop while (< low high)
              do (let ((mid (floor (+ low high) 2)))
                   (if (< (aref newlines mid) position) (setf low (1+ mid)) (setf high mid))))
        low)
      (count #\Newline text :end position)))

(defun newline-positions (text)
  (let ((positions (make-array 64 :adjustable t :fill-pointer 0)))
    (loop for i = (position #\Newline text) then (position #\Newline text :start (1+ i))
          while i do (vector-push-extend i positions))
    positions))

(defun source-definitions (text)
  "The definitions in TEXT (Lisp source), in order."
  (let ((definitions '())
        (*newline-positions* (cons text (newline-positions text))))
    (labels ((walk (nodes depth)
               (dolist (node nodes)
                 (when (eq (node-kind node) :list)
                   (let* ((elements (form-elements node text))
                          (head (operator-name (first elements)))
                          (entry (and head (assoc head *definition-kinds* :test #'string=))))
                     (cond (entry
                            (let ((definition (read-definition head (cdr entry) elements text)))
                              (when definition (push definition definitions))))
                           ((and head (< depth 4) (member head *definition-containers* :test #'string=))
                            (walk (rest elements) (1+ depth)))))))))
      (walk (read-form-tree text) 0))
    (nreverse definitions)))

;;; Marking the argument at the cursor

(defparameter *lambda-list-keywords*
  '("&optional" "&rest" "&body" "&key" "&aux" "&allow-other-keys" "&whole" "&environment"))

(defun parameter-name (node text)
  "The name of the parameter NODE, as in a, (a 1) or ((:key a) 1), downcased."
  (if (eq (node-kind node) :atom)
      (string-downcase (node-text node))
      (let ((first (first (form-elements node text))))
        (cond ((null first) "")
              ((eq (node-kind first) :atom) (string-downcase (node-text first)))
              (t (let ((inner (form-elements first text)))
                   (if inner (string-left-trim ":" (string-downcase (node-text (first inner)))) "")))))))

(defun arglist-parameter (arglist index keyword)
  "Which parameter of ARGLIST (a lambda list's text) the INDEXth argument
(from 0) fills, as a node; KEYWORD is the keyword before it, if any."
  (let* ((list (first (read-form-tree arglist)))
         (params (and list (eq (node-kind list) :list) (form-elements list arglist)))
         (mode :required)
         (position 0))
    (loop while params
          do (let* ((param (pop params))
                    (name (and (eq (node-kind param) :atom) (string-downcase (node-text param)))))
               (cond ((member name '("&whole" "&environment") :test #'equal) (pop params))
                     ((equal name "&optional") (setf mode :required))
                     ((member name '("&rest" "&body") :test #'equal)
                      (when (and params (>= index position) (not (eq mode :key)))
                        (return (first params)))
                      (pop params))
                     ((equal name "&key") (setf mode :key))
                     ((equal name "&aux") (return nil))
                     ((equal name "&allow-other-keys"))
                     ((eq mode :key)
                      (when (and keyword (>= index position)
                                 (string-equal (parameter-name param arglist) (string-left-trim ":" keyword)))
                        (return param)))
                     (t (when (= position index) (return param))
                        (incf position)))))))

(defun arglist-hint (name arglist &optional index keyword)
  "\"(NAME params…)\" from ARGLIST (a lambda list's text), with the
parameter the INDEXth argument fills between ===> and <===."
  (let* ((arglist (collapse-whitespace arglist))
         (param (and index (arglist-parameter arglist index keyword)))
         (marked (if param
                     (concatenate 'string (subseq arglist 0 (node-start param)) "===> "
                                  (subseq arglist (node-start param) (node-end param)) " <==="
                                  (subseq arglist (node-end param)))
                     arglist))
         (inner (string-trim " " (subseq marked (min 1 (length marked)) (max 1 (1- (length marked)))))))
    (format nil "(~a~:[ ~a~;~*~])" name (string= inner "") inner)))

(defun form-argument-position (form marker)
  "For FORM from RAW-FORM-AT, the innermost list with an operator, as
(operator index keyword) for each enclosing list, innermost first: INDEX
is the argument the cursor is in (from 0), KEYWORD the keyword before it."
  (let ((result '()))
    (labels ((walk (list)
               (let ((cursor (position-if (lambda (e) (or (eq e marker) (consp e))) list :from-end t)))
                 (when cursor
                   (let ((element (nth cursor list)))
                     (when (consp element) (walk element))
                     (when (stringp (first list))
                       (let* ((index (- (if (eq element marker) (1- cursor) cursor) 1))
                              (before (and (> index 0) (nth index list)))
                              (keyword (and (stringp before) (plusp (length before))
                                            (char= (char before 0) #\:) before)))
                         (when (>= index 0)
                           (push (list (first list) index keyword) result)))))))))
      (walk form))
    (nreverse result)))

;;; The editor's own Common Lisp, for standard symbols without a connection

(defun standard-symbol (name)
  "The external COMMON-LISP symbol named NAME (any case, any package prefix), or nil."
  (multiple-value-bind (symbol status)
      (find-symbol (string-upcase (subseq name (symbol-name-part name))) '#:common-lisp)
    (and (eq status :external) symbol)))

(defun standard-symbol-kind (symbol)
  (cond ((special-operator-p symbol) :special)
        ((macro-function symbol) :macro)
        ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function)) :generic)
        ((fboundp symbol) :function)
        ((constantp symbol) :constant)
        ((boundp symbol) :variable)
        ((find-class symbol nil) :class)
        (t nil)))

(defun standard-arglist (name)
  "The lambda list of the standard function or macro NAME, as text, or nil."
  (let ((symbol (standard-symbol name)))
    (when (and symbol (fboundp symbol))
      (let ((list (ignore-errors
                   (sb-introspect:function-lambda-list (or (macro-function symbol) (fdefinition symbol))))))
        (unless (eq list :unknown)
          (lambda-list-text list))))))

(defun lambda-list-text (list)
  "LIST, a lambda list, printed with symbols by name alone, downcased."
  (labels ((strip (x)
             (cond ((keywordp x) x)
                   ((and (symbolp x) x (not (eq x t))) (make-symbol (symbol-name x)))
                   ((consp x) (cons (strip (car x)) (strip (cdr x))))
                   (t x))))
    (let ((*print-case* :downcase) (*print-gensym* nil) (*package* (find-package '#:keyword)))
      (prin1-to-string (strip list)))))

(defun standard-documentation (name kind)
  (let ((symbol (standard-symbol name)))
    (and symbol (documentation symbol (if (member kind '(:variable :constant)) 'variable 'function)))))

(defvar *standard-symbols* nil)

(defun standard-symbols ()
  "Every external COMMON-LISP symbol, as (name . kind), names downcased."
  (or *standard-symbols*
      (setf *standard-symbols*
            (let ((symbols '()))
              (do-external-symbols (s '#:common-lisp)
                (push (cons (string-downcase (symbol-name s)) (standard-symbol-kind s)) symbols))
              (sort symbols #'string< :key #'car)))))

;;; Racket and Clojure: what a file defines, from its tokens

(defparameter *racket-definition-kinds*
  '(("define-syntax" . :macro) ("define-syntax-rule" . :macro) ("define-syntaxes" . :macro)
    ("define-values" . :variable) ("struct" . :class) ("define-struct" . :class)
    ("module" . :module) ("module*" . :module) ("module+" . :module)))

(defparameter *clojure-definition-kinds*
  '(("defn" . :function) ("defn-" . :function) ("defmacro" . :macro) ("def" . :variable)
    ("defonce" . :variable) ("defmulti" . :generic) ("defmethod" . :method)
    ("defprotocol" . :interface) ("definterface" . :interface) ("defrecord" . :class)
    ("deftype" . :class) ("defstruct" . :class) ("ns" . :namespace) ("deftest" . :test)))

(defun dialect-definition-kind (dialect head named-by-list)
  "The kind of thing a top-level form headed by HEAD defines. NAMED-BY-LIST:
its name is the head of a list, as in (define (f x) …)."
  (ecase (dialect-name dialect)
    (:racket (or (cdr (assoc head *racket-definition-kinds* :test #'string=))
                 (cond ((member head '("define" "define/contract" "define/public" "define/private")
                                :test #'string=)
                        (if named-by-list :function :variable)))))
    (:clojure (cdr (assoc (base-symbol-name head #\/) *clojure-definition-kinds* :test #'string=)))))

(defun dialect-definitions (syntax)
  "The top-level definitions in SYNTAX's text (Racket or Clojure), in order,
as DEFINITION structs with their name, kind and line."
  (let ((dialect (syntax-dialect syntax))
        (definitions '())
        (head nil))                     ; the definer heading the current top-level form
    (ensure-lexed syntax (1- (syntax-line-count syntax)))
    (dotimes (line (syntax-line-count syntax))
      (loop with tokens = (line-tokens syntax line)
            for index from 0 below (length tokens)
            for tk = (aref tokens index)
            do (case (token-subtype tk)
                 (:head (when (and (= (token-depth tk) 1) (eq (token-type tk) :symbol))
                          (let ((text (token-text syntax line tk)))
                            (setf head (and (funcall (dialect-definer dialect) text) text)))))
                 (:name (when (and head (<= (token-depth tk) 2) (eq (token-type tk) :symbol))
                          (let ((named-by-list (= (token-depth tk) 2)))
                            (push (make-definition :name (token-text syntax line tk)
                                                   :kind (dialect-definition-kind dialect head named-by-list)
                                                   :line line)
                                  definitions))
                          (setf head nil))))))
    (nreverse definitions)))
