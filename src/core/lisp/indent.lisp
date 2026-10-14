;;;; indent.lisp — Lisp indentation, in the style of SLIME's common-lisp-indent
;;;;
;;;; A line is indented by the list it is in:
;;;;   - data (quoted lists, vectors, lists headed by a list, keyword or
;;;;     number): under the first element;
;;;;   - forms whose head has an indentation spec N (WHEN 1, LET 1, DEFUN 2,
;;;;     and any DEF…, WITH-… or DO-… head): the first N arguments are
;;;;     "distinguished" and indented 4, the body 2;
;;;;   - other calls: under the first argument when it is on the head's line,
;;;;     otherwise 1 past the paren.
;;;; Local functions in FLET, LABELS and MACROLET indent like DEFUN.
;;;; In LOOP, the forms of a DO (DOING, INITIALLY, FINALLY) clause line up
;;;; under the first of them.
;;;;
;;;; Racket uses the same rules with its own table (racket-mode's and
;;;; DrRacket's conventions). Clojure follows cljfmt's default style: a form
;;;; with a spec indents all its arguments 2, whether or not they are
;;;; distinguished, and [ ], { } and #{ } line up their elements.

(in-package #:cadre)

(defvar *indentation-specs* (make-hash-table :test 'equal)
  "Head symbol name (lower case, without package) → number of distinguished arguments.")

(defmacro define-indentation (name spec)
  "Indent forms headed by NAME (a symbol or string) with SPEC distinguished
arguments: they get 4 extra columns, the body after them 2."
  `(setf (gethash (string-downcase (string ',name)) *indentation-specs*) ,spec))

(loop for (name spec) in
      '((block 1) (case 1) (ccase 1) (ecase 1) (typecase 1) (ctypecase 1) (etypecase 1)
        (catch 1) (defun 2) (defmacro 2) (defmethod 2) (defgeneric 2) (defclass 2)
        (defstruct 1) (defpackage 1) (define-condition 2) (defsetf 2) (deftype 2)
        (defvar 1) (defparameter 1) (defconstant 1)
        (destructuring-bind 2) (multiple-value-bind 2) (do 2) (do* 2) (dolist 1) (dotimes 1)
        (do-symbols 1) (do-external-symbols 1) (do-all-symbols 1) (eval-when 1)
        (flet 1) (labels 1) (macrolet 1) (symbol-macrolet 1)
        (handler-bind 1) (handler-case 1) (restart-case 1) (restart-bind 1)
        (lambda 1) (let 1) (let* 1) (locally 0) (multiple-value-prog1 1) (prog1 1) (prog2 2)
        (progn 0) (progv 2) (return-from 1) (tagbody 0) (the 1) (unless 1) (when 1)
        (unwind-protect 1) (with-accessors 2) (with-slots 2) (with-open-file 1)
        (with-open-stream 1) (with-output-to-string 1) (with-input-from-string 1)
        (with-standard-io-syntax 0) (with-simple-restart 1) (with-hash-table-iterator 1)
        (with-package-iterator 1) (with-compilation-unit 1) (print-unreadable-object 1)
        (pprint-logical-block 1) (in-package 0))
      do (setf (gethash (string-downcase (symbol-name name)) *indentation-specs*) spec))

(defvar *learned-indentation* (make-hash-table :test 'equal)
  "Names whose spec came from the Lisp (learn-indentation), not the table above.")

(defun body-position (indent)
  "The number of arguments before &body, from INDENT as Swank reports it:
a number, or (with its indentation contrib) an Emacs spec such as (4 4 &body).
Nil for anything else."
  (cond ((integerp indent) indent)
        ((consp indent)
         (position-if (lambda (x)
                        (let ((name (cond ((stringp x) x)
                                          ((remote-symbol-p x) (remote-symbol-name x)))))
                          (and name (string= "&body" (symbol-base-name name)))))
                      indent))))

(defun learn-indentation (updates)
  "Use the indentation the Lisp reports for macros with &body: Swank's
:indentation-update sends (name indent packages) for each, where indent
gives the position of &body, or is nil when a macro no longer has one."
  (dolist (update updates)
    (when (and (consp update) (stringp (first update)) (consp (rest update)))
      (let ((name (string-downcase (first update)))
            (indent (body-position (second update))))
        (cond ((integerp indent)
               (setf (gethash name *indentation-specs*) indent
                     (gethash name *learned-indentation*) t))
              ((and (null indent) (gethash name *learned-indentation*))
               (remhash name *indentation-specs*)
               (remhash name *learned-indentation*)))))))

(defun symbol-base-name (string)
  "STRING without any package prefix, in lower case."
  (string-downcase (subseq string (1+ (or (position #\: string :from-end t) -1)))))

(defun indentation-spec (name)
  "The number of distinguished arguments for a form headed by NAME, or nil
for an ordinary call."
  (let ((base (symbol-base-name name)))
    (or (gethash base *indentation-specs*)
        (cond ((and (> (length base) 3) (string= "def" base :end2 3)) 2)
              ((and (> (length base) 5) (string= "with-" base :end2 5)) 1)
              ((and (> (length base) 3) (string= "do-" base :end2 3)) 1)))))

(defun list-elements (syntax open-line open-index &optional limit-line)
  "The elements of the list opened at (OPEN-LINE, OPEN-INDEX), as a list of
(line column token), stopping before LIMIT-LINE if given."
  (let* ((open (token-ref syntax open-line open-index))
         (inner (1+ (token-depth open)))
         (elements '())
         (prefix nil))                  ; (line column) of prefixes before the next element
    (multiple-value-bind (l i) (next-token syntax open-line open-index)
      (loop
        (when (or (null l) (and limit-line (>= l limit-line))) (return))
        (let ((tk (token-ref syntax l i)))
          (when (and (eq (token-type tk) :close) (< (token-depth tk) inner)) (return))
          (when (and (= (token-depth tk) inner)
                     (not (member (token-type tk) '(:close :comment :invalid)))
                     (not (continues-from-previous-line-p syntax l i)))
            (if (member (token-type tk) *prefix-token-types*)
                (unless prefix (setf prefix (list l (token-start tk))))
                (progn
                  (push (list (if prefix (first prefix) l) (if prefix (second prefix) (token-start tk)) tk)
                        elements)
                  (setf prefix nil)))))
        (multiple-value-setq (l i) (next-token syntax l i))))
    (nreverse elements)))

(defun open-place (syntax line column)
  "The token place of the open paren at (LINE, COLUMN)."
  (let ((i (position-if (lambda (tk) (and (eq (token-type tk) :open) (= (token-start tk) column)))
                        (line-tokens syntax line))))
    (and i (values line i))))

(defun quoted-open-p (syntax line index)
  "True if the open paren at (LINE, INDEX) is a vector, or quoted."
  (let ((tk (token-ref syntax line index)))
    (or (> (- (token-end tk) (token-start tk)) 1)        ; #(
        (multiple-value-bind (l i) (previous-token syntax line index)
          (and l (let ((p (token-ref syntax l i)))
                   (and (eq (token-type p) :quote) (= l line) (= (token-end p) (token-start tk))
                        (member (char (text-line-string (syntax-text syntax) l) (token-start p))
                                '(#\' #\`)))))))))

(defun token-text (syntax line token)
  (subseq (text-line-string (syntax-text syntax) line) (token-start token) (token-end token)))

(defun local-function-list-p (syntax line column)
  "True if the list opened at (LINE, COLUMN) defines a local function in an
FLET, LABELS or MACROLET."
  (multiple-value-bind (bl bc) (up-list-position syntax line column)
    (when bl
      (multiple-value-bind (fl fc) (up-list-position syntax bl bc)
        (when fl
          (multiple-value-bind (ol oi) (open-place syntax fl fc)
            (let ((elements (list-elements syntax ol oi)))
              (and (>= (length elements) 2)
                   (eq (token-type (third (first elements))) :symbol)
                   (member (symbol-base-name (token-text syntax (first (first elements))
                                                         (third (first elements))))
                           '("flet" "labels" "macrolet") :test #'string=)
                   (destructuring-bind (l c tk) (second elements)
                     (declare (ignore tk))
                     (and (= l bl) (= c bc)))))))))))

(defparameter *loop-keywords*
  '("named" "with" "for" "as" "and" "initially" "finally" "do" "doing" "return"
    "collect" "collecting" "append" "appending" "nconc" "nconcing" "count" "counting"
    "sum" "summing" "maximize" "maximizing" "minimize" "minimizing" "into"
    "repeat" "while" "until" "always" "never" "thereis" "when" "if" "unless" "else" "end"
    "in" "on" "by" "from" "upfrom" "downfrom" "to" "upto" "downto" "below" "above"
    "across" "being" "each" "the" "of" "using" "then" "=")
  "The words that make up LOOP clauses.")

(defparameter *loop-body-keywords* '("do" "doing" "initially" "finally")
  "The LOOP keywords followed by any number of forms.")

(defun loop-keyword (syntax line token)
  "TOKEN's name if it is a LOOP keyword (FOR, :FOR, IN…), else nil."
  (when (member (token-type token) '(:symbol :keyword))
    (find (symbol-base-name (token-text syntax line token)) *loop-keywords* :test #'string=)))

(defun loop-indentation (syntax line elements)
  "The column for LINE, in a LOOP whose elements before it are ELEMENTS, when
it continues the forms of a DO (or similar) clause; else nil."
  (let ((first (find-if (lambda (tk) (not (eq (token-type tk) :comment))) (line-tokens syntax line))))
    (unless (and first (loop-keyword syntax line first))
      (let ((keyword (position-if (lambda (e) (loop-keyword syntax (first e) (third e)))
                                  elements :from-end t)))
        (when (and keyword (plusp keyword)
                   (member (loop-keyword syntax (first (nth keyword elements)) (third (nth keyword elements)))
                           *loop-body-keywords* :test #'string=))
          (let ((form (nth (1+ keyword) elements)))
            (if form (second form) (+ (second (nth keyword elements)) 2))))))))

(defparameter *racket-indentation-specs*
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name spec) in
          '(("define" 1) ("define-values" 1) ("define-syntax" 1) ("define-syntax-rule" 1)
            ("define/contract" 1) ("define-for-syntax" 1) ("lambda" 1) ("λ" 1) ("case-lambda" 0)
            ("let" 1) ("let*" 1) ("letrec" 1) ("letrec*" 1) ("let-values" 1) ("let*-values" 1)
            ("letrec-values" 1) ("let-syntax" 1) ("letrec-syntax" 1) ("when" 1) ("unless" 1)
            ("begin" 0) ("begin0" 1) ("begin-for-syntax" 0) ("cond" 0) ("case" 1) ("match" 1) ("match*" 1)
            ("match-let" 1) ("match-let*" 1) ("match-lambda" 0) ("match-lambda*" 0)
            ("match-define" 1) ("for" 1) ("for*" 1) ("for/fold" 2) ("for*/fold" 2)
            ("for/foldr" 2) ("for*/foldr" 2) ("do" 2) ("module" 2) ("module*" 2) ("module+" 1)
            ("struct" 1) ("define-struct" 1) ("class" 1) ("class*" 2) ("mixin" 2) ("interface" 1)
            ("parameterize" 1) ("parameterize*" 1) ("with-handlers" 1) ("with-handlers*" 1)
            ("dynamic-wind" 0) ("syntax-case" 2) ("syntax-case*" 3) ("syntax-rules" 1)
            ("syntax-parse" 1) ("syntax-parser" 0) ("with-syntax" 1) ("with-syntax*" 1)
            ("quasisyntax/loc" 1) ("syntax/loc" 1) ("local" 1) ("instantiate" 2) ("unit" 2)
            ("test-case" 1) ("test-suite" 1) ("check-exn" 1) ("place" 1) ("thread" 0)
            ("with-output-to-string" 0) ("with-input-from-string" 1) ("call-with-output-string" 0)
            ("call-with-input-file" 1) ("call-with-output-file" 1) ("with-output-to-file" 1)
            ("with-input-from-file" 1))
          do (setf (gethash name table) spec))
    table)
  "Racket heads → the number of distinguished arguments.")

(defun name-starts-with-p (prefix name)
  (and (> (length name) (length prefix)) (string= prefix name :end2 (length prefix))))

(defun racket-indentation-spec (name)
  "The number of distinguished arguments for a Racket form headed by NAME, or nil."
  (or (gethash name *racket-indentation-specs*)
      (and (some (lambda (prefix) (name-starts-with-p prefix name)) '("define" "with-" "for/" "for*/"))
           1)))

(defparameter *clojure-body-forms*
  '("ns" "fn" "fn*" "let" "letfn" "loop" "binding" "when" "when-not" "when-let" "when-some"
    "when-first" "if-let" "if-some" "if-not" "doseq" "dotimes" "for" "while" "do" "try" "catch"
    "finally" "case" "cond" "condp" "cond->" "cond->>" "as->" "with-open" "with-redefs"
    "with-local-vars" "locking" "future" "delay" "lazy-seq" "dosync" "reify" "proxy" "extend"
    "extend-type" "extend-protocol" "comment" "testing" "deftest" "are" "go" "go-loop" "thread"
    "match" "doto" "defrecord" "deftype" "defprotocol" "definterface" "defmethod" "defmulti"
    "specify" "specify!" "add-watch" "bound-fn" "areduce" "amap" "struct-map")
  "Clojure heads whose arguments indent 2 (cljfmt's block and inner forms).")

(defun clojure-body-form-p (name)
  "True for a Clojure head whose arguments all indent 2: def…, with-…, and
the forms in *clojure-body-forms*."
  (let ((base (base-symbol-name name #\/)))
    (or (member base *clojure-body-forms* :test #'string=)
        (string= base "def")
        (name-starts-with-p "def" base)
        (name-starts-with-p "with-" base))))

(defun lisp-indentation (syntax line)
  "The column LINE should be indented to, or nil if it should be left alone
(it starts inside a string or comment)."
  (case (dialect-name (syntax-dialect syntax))
    (:clojure (clojure-indentation syntax line))
    (:racket (common-lisp-indentation syntax line #'racket-indentation-spec nil))
    (t (common-lisp-indentation syntax line #'indentation-spec t))))

(defun list-head-context (syntax line)
  "For LINE inside a list: the open token's place and column, and the list's
elements before LINE. Nil at top level."
  (multiple-value-bind (ol oc) (up-list-position syntax line 0)
    (when ol
      (multiple-value-bind (ol oi) (open-place syntax ol oc)
        (values ol oi oc (list-elements syntax ol oi line))))))

(defun clojure-indentation (syntax line)
  "cljfmt's default style: body forms indent 2; other calls (a keyword at the
head too, as in (:require …)) line up under the first argument when it is on
the head's line, else 1 past the paren; [ ], { }, #{ } and data line up
under the first element."
  (let ((state (line-start-state syntax line)))
    (cond
      ((not (eq (state-mode state) :code)) nil)
      ((zerop (state-depth state)) 0)
      (t
       (multiple-value-bind (ol oi oc elements) (list-head-context syntax line)
         (declare (ignore oc))
         (if (null ol)
             0
             (let* ((open (token-ref syntax ol oi))
                    (paren (1- (token-end open)))
                    (head (first elements)))
               (flet ((under-first ()
                        (if (and head (= (first head) ol)) (second head) (1+ paren))))
                 (cond
                   ((or (null head) (not (eql (token-subtype open) #\()) (quoted-open-p syntax ol oi)
                        (not (member (token-type (third head)) '(:symbol :keyword))))
                    (under-first))
                   ((and (eq (token-type (third head)) :symbol)
                         (clojure-body-form-p (token-text syntax (first head) (third head))))
                    (+ paren 2))
                   ((and (second elements) (= (first (second elements)) (first head)))
                    (second (second elements)))
                   (t (1+ paren)))))))))))

(defun common-lisp-indentation (syntax line spec-function cl-rules)
  "SLIME's rules (see the top of this file), with SPEC-FUNCTION giving a
head's number of distinguished arguments. CL-RULES adds Common Lisp's own:
LOOP clauses and local functions in FLET and LABELS."
  (let ((state (line-start-state syntax line)))
    (cond
      ((not (eq (state-mode state) :code)) nil)
      ((zerop (state-depth state)) 0)
      (t
       (multiple-value-bind (ol oc) (up-list-position syntax line 0)
         (if (null ol)
             0
             (multiple-value-bind (ol oi) (open-place syntax ol oc)
               (let* ((open (token-ref syntax ol oi))
                      (paren (1- (token-end open)))
                      (elements (list-elements syntax ol oi line))
                      (head (first elements)))
                 (flet ((under-first ()
                          (if (and head (= (first head) ol)) (second head) (1+ paren))))
                   (cond
                     ((or (null head) (quoted-open-p syntax ol oi)
                          (not (eq (token-type (third head)) :symbol)))
                      (under-first))
                     (t
                      (let* ((name (token-text syntax (first head) (third head)))
                             (spec (if (and cl-rules (local-function-list-p syntax ol oc))
                                       1
                                       (funcall spec-function name)))
                             (arguments (1- (length elements))))
                        (cond
                          ((and cl-rules
                                (string= (symbol-base-name name) "loop")
                                (loop-indentation syntax line elements)))
                          (spec (if (< arguments spec) (+ paren 4) (+ paren 2)))
                          ((and (second elements) (= (first (second elements)) (first head)))
                           (second (second elements)))
                          (t (1+ paren)))))))))))))))
