;;;; faces.lisp — what each token looks like
;;;;
;;;; TOKEN-FACE names a face for a token: the GUI maps faces to colors.
;;;; Symbols are classified lexically: definers (DEF…) and their names,
;;;; Common Lisp's macros and special operators, keywords, lambda-list
;;;; keywords, *special* variables and +constants+. Colors that depend on
;;;; what the running image knows come later (design doc 7.3).

(in-package #:cadre)

(defparameter *faces*
  '(:comment :string :number :character :keyword :builtin :definer :definition-name
    :special-variable :constant :lambda-keyword :reader-conditional :quote :invalid)
  "Every face TOKEN-FACE returns, apart from (:paren n).")

(defparameter *paren-face-count* 6
  "Rainbow parentheses cycle through this many colors.")

(defvar *cl-operator-cache* (make-hash-table :test 'equal))

(defun cl-operator-p (name)
  "True if NAME (lower case, no package) names a Common Lisp macro or special operator."
  (multiple-value-bind (value found) (gethash name *cl-operator-cache*)
    (if found
        value
        (setf (gethash name *cl-operator-cache*)
              (let ((symbol (find-symbol (string-upcase name) :common-lisp)))
                (and symbol (or (special-operator-p symbol) (macro-function symbol)) t))))))

(defun package-prefix (string)
  (let ((colon (position #\: string)))
    (and colon (plusp colon) (string-downcase (subseq string 0 colon)))))

(defun earmuffs-p (base)
  (and (> (length base) 2) (char= (char base 0) #\*) (char= (char base (1- (length base))) #\*)))

(defparameter *racket-builtins*
  '("define" "define-values" "define-syntax" "define-syntax-rule" "define-syntaxes" "define-struct"
    "define/contract" "define-for-syntax" "lambda" "λ" "case-lambda" "let" "let*" "letrec" "letrec*"
    "let-values" "let*-values" "letrec-values" "let-syntax" "letrec-syntax" "if" "cond" "case" "when"
    "unless" "and" "or" "begin" "begin0" "begin-for-syntax" "set!" "quote" "quasiquote" "unquote"
    "unquote-splicing" "syntax" "quasisyntax" "unsyntax" "match" "match*" "match-define" "match-let"
    "match-lambda" "for" "for*" "for/list" "for*/list" "for/vector" "for/hash" "for/fold" "for*/fold"
    "for/and" "for/or" "for/sum" "for/first" "for/last" "do" "require" "provide" "module" "module*"
    "module+" "struct" "class" "class*" "interface" "new" "send" "parameterize" "with-handlers"
    "with-handlers*" "dynamic-wind" "syntax-rules" "syntax-case" "syntax-parse" "with-syntax" "delay"
    "force" "assert" "λ" "else" "#%app" "#%datum" "#%module-begin" "only-in" "except-in" "prefix-in"
    "rename-in" "all-defined-out" "all-from-out" "contract-out" "for-syntax" "time" "local")
  "Racket's core forms and everyday macros, highlighted as built in at the head of a list.")

(defparameter *clojure-builtins*
  '("def" "if" "do" "let" "quote" "var" "fn" "loop" "recur" "throw" "try" "catch" "finally"
    "monitor-enter" "monitor-exit" "new" "set!" "ns" "in-ns" "defn" "defn-" "defmacro" "defonce"
    "defmulti" "defmethod" "defprotocol" "defrecord" "deftype" "definterface" "defstruct" "reify"
    "proxy" "extend-type" "extend-protocol" "extend" "when" "when-not" "when-let" "when-some"
    "when-first" "if-not" "if-let" "if-some" "cond" "condp" "case" "cond->" "cond->>" "->" "->>"
    "as->" "some->" "some->>" "doto" "and" "or" "not" "for" "doseq" "dotimes" "while" "binding"
    "letfn" "with-open" "with-redefs" "with-local-vars" "with-meta" "comment" "declare" "import"
    "require" "use" "refer" "refer-clojure" "gen-class" "future" "delay" "lazy-seq" "locking"
    "dosync" "assert" "time" "fn*" "let*" "loop*" "letfn*" "case*" "deftest" "testing" "is" "are")
  "Clojure's special forms and clojure.core's everyday macros, highlighted as built in at the
head of a list.")

(defun base-name-in (text dialect)
  "TEXT without its package or namespace, lower-cased, as DIALECT writes them."
  (let ((separator (dialect-package-separator dialect)))
    (if (and (eql separator #\/) (string= text "/"))
        "/"
        (base-symbol-name text separator))))

(defun symbol-face (text subtype &optional (dialect *common-lisp-dialect*))
  (if (eq dialect *common-lisp-dialect*)
      (lisp-symbol-face text subtype)
      (let ((base (base-name-in text dialect))
            (builtins (if (eq (dialect-name dialect) :racket) *racket-builtins* *clojure-builtins*)))
        (cond
          ((and (eq subtype :name) (earmuffs-p base)) :special-variable)
          ((eq subtype :name) :definition-name)
          ((and (eq subtype :head) (funcall (dialect-definer dialect) text)) :definer)
          ((and (eq subtype :head) (member base builtins :test #'string=)) :builtin)
          ((member base '("&" "&rest" "&optional" "&key" "&body") :test #'string=) :lambda-keyword)
          ((earmuffs-p base) :special-variable)
          ((and (eq (dialect-name dialect) :clojure) (member text '("true" "false" "nil") :test #'string=))
           :constant)
          ((and (eq (dialect-name dialect) :racket) (member text '("null" "empty" "void") :test #'string=))
           :constant)))))

(defun lisp-symbol-face (text subtype)
  (let ((base (symbol-base-name text))
        (prefix (package-prefix text)))
    (cond
      ((eq subtype :feature) :reader-conditional)
      ((and (eq subtype :name) (earmuffs-p base)) :special-variable)   ; (defvar *x* …)
      ((eq subtype :name) :definition-name)
      ((and (eq subtype :head) (definer-name-p text)) :definer)
      ((and (eq subtype :head)
            (or (null prefix) (member prefix '("cl" "common-lisp") :test #'string=))
            (cl-operator-p base))
       :builtin)
      ((and (plusp (length base)) (char= (char base 0) #\&)) :lambda-keyword)
      ((earmuffs-p base) :special-variable)
      ((or (and (> (length base) 2) (char= (char base 0) #\+) (char= (char base (1- (length base))) #\+))
           (member base '("t" "nil") :test #'string=))
       :constant))))

(defun token-face (token line &optional (dialect *common-lisp-dialect*))
  "The face for TOKEN, which is on LINE (a string) of DIALECT's source: a
keyword from *faces*, (:paren depth), or nil for the default look."
  (case (token-type token)
    ((:open :close) (list :paren (mod (token-depth token) *paren-face-count*)))
    (:symbol (symbol-face (subseq line (token-start token) (token-end token)) (token-subtype token) dialect))
    ((:comment :string :number :character :keyword :constant :reader-conditional :invalid :quote)
     (token-type token))
    (:datum-comment :comment)
    (:reader :quote)))

(defun datum-comment-columns (syntax line)
  "The column ranges (start end) of LINE that #; or #_ comments out: the
prefix and the element after it, which can start on an earlier line and
run past this one."
  (let ((ranges '())
        (length (line-info-length (line-info syntax line))))
    (unless (eq (syntax-dialect syntax) *common-lisp-dialect*)
      ;; Start from the top-level form this line is in (with its prefixes),
      ;; since a commented-out form may begin above it.
      (multiple-value-bind (l i) (toplevel-open-before syntax line 0 :inclusive t)
        (if l
            (multiple-value-setq (l i) (element-start-place syntax l i))
            (setf l line i 0))
        (loop while (and l (<= l line))
              do (when (< i (length (line-tokens syntax l)))
                   (let ((tk (token-ref syntax l i)))
                     (when (eq (token-type tk) :datum-comment)
                       (multiple-value-bind (el ec) (forward-sexp-position syntax l (token-end tk))
                         (unless el (setf el l ec (token-end tk)))
                         (when (>= el line)
                           (let ((start (if (= l line) (token-start tk) 0))
                                 (end (if (= el line) ec length)))
                             (when (< start end) (push (list start end) ranges))))))))
                 (multiple-value-setq (l i) (next-token syntax l i)))))
    (nreverse ranges)))
