;;;; dialect.lisp — how each Lisp reads its source
;;;;
;;;; Common Lisp, Racket and Clojure share the lexer (lexer.lisp) and
;;;; everything built on its tokens. What differs is how they read
;;;; characters: which brackets open lists, what ends a symbol, what follows
;;;; #, how characters and keywords are written. A DIALECT holds those rules.
;;;; The lexer branches on DIALECT-NAME for the # dispatch, where the three
;;;; differ most, and reads the rest from the slots.

(in-package #:cadre)

(defstruct (dialect (:constructor make-dialect
                        (&key name title opens terminating comma-whitespace block-comments
                              bar-symbols colon-keywords package-separator definer named-head
                              at-expressions (initial-state '(:code 0)))))
  "NAME is :common-lisp, :racket or :clojure. OPENS lists the characters
that open a list by themselves. TERMINATING lists the characters, apart
from whitespace, that end a symbol. COMMA-WHITESPACE is true where a comma
is whitespace (Clojure). BLOCK-COMMENTS: #| |# nests. BAR-SYMBOLS: |…| is
part of a symbol and \\ escapes inside one. COLON-KEYWORDS: a symbol
starting with : is a keyword. PACKAGE-SEPARATOR splits a qualified symbol.
DEFINER is a function of a head symbol's text, true if the list defines
something named by its next element. NAMED-HEAD: after a definer, a list's
head is the name, as in Racket's (define (f x) …). AT-EXPRESSIONS: Scribble's
@-expressions, @cmd[args]{text}, whose text bodies are prose. INITIAL-STATE
is the lexer's state at the top of a file (lexer.lisp)."
  name title opens terminating comma-whitespace block-comments bar-symbols
  colon-keywords package-separator definer named-head at-expressions initial-state)

(defparameter +brackets+ '((#\( . #\)) (#\[ . #\]) (#\{ . #\}))
  "Each opening bracket and the one that closes it.")

(defun closing-bracket (open)
  (cdr (assoc open +brackets+)))

(defun close-bracket-char-p (c)
  (rassoc c +brackets+))

(defun base-symbol-name (text separator)
  "TEXT without its package or namespace, lower-cased."
  (string-downcase (subseq text (1+ (or (and separator (position separator text :from-end t)) -1)))))

(defun lisp-definer-p (text)
  "Common Lisp: DEF… heads (defun, defclass, define-condition, …)."
  (let ((name (base-symbol-name text #\:)))
    (and (> (length name) 3) (string= "def" name :end2 3))))

(defparameter *racket-definers*
  '("struct" "module" "module*" "module+")
  "Racket heads that name something but don't start with define.")

(defun racket-definer-p (text)
  "Racket: define and its family (define-syntax, define/contract, …), struct, module."
  (let ((name (string-downcase text)))
    (or (and (>= (length name) 6) (string= "define" name :end2 6))
        (member name *racket-definers* :test #'string=))))

(defun clojure-definer-p (text)
  "Clojure: def and DEF… heads (defn, defn-, defmacro, defprotocol, …), and ns."
  (let ((name (base-symbol-name text #\/)))
    (or (and (>= (length name) 3) (string= "def" name :end2 3))
        (string= name "ns"))))

(defparameter *common-lisp-dialect*
  (make-dialect :name :common-lisp :title "Common Lisp"
                :opens '(#\()
                :terminating '(#\( #\) #\" #\' #\` #\, #\;)
                :block-comments t :bar-symbols t :colon-keywords t
                :package-separator #\: :definer #'lisp-definer-p))

(defparameter *racket-dialect*
  (make-dialect :name :racket :title "Racket"
                :opens '(#\( #\[ #\{)
                :terminating '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\;)
                :block-comments t :bar-symbols t
                :definer #'racket-definer-p :named-head t))

(defparameter *scribble-dialect*
  (make-dialect :name :racket :title "Scribble"
                :opens '(#\( #\[ #\{)
                :terminating '(#\( #\) #\[ #\] #\{ #\} #\" #\' #\` #\, #\; #\@)
                :block-comments t :bar-symbols t
                :definer #'racket-definer-p :named-head t
                :at-expressions t
                ;; A Scribble document is prose from the top: a text body
                ;; with no closing brace.
                :initial-state '(:text 0 ((:text 0 :top))))
  "Racket read with Scribble's @-expressions (named :racket: it runs as Racket).")

(defparameter *clojure-dialect*
  (make-dialect :name :clojure :title "Clojure"
                :opens '(#\( #\[ #\{)
                :terminating '(#\( #\) #\[ #\] #\{ #\} #\" #\; #\@ #\^ #\` #\~ #\\)
                :comma-whitespace t :colon-keywords t
                :package-separator #\/ :definer #'clojure-definer-p))

(defun find-dialect (name)
  "The dialect named NAME (:common-lisp, :racket or :clojure), or nil."
  (case name
    (:common-lisp *common-lisp-dialect*)
    (:racket *racket-dialect*)
    (:clojure *clojure-dialect*)))

(defun mode-dialect (mode)
  "The dialect MODE's text is read as, or nil if MODE isn't a Lisp."
  (find-dialect (mode-dialect-name mode)))
