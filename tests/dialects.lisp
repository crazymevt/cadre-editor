(in-package #:cadre-tests)

;;; Racket and Clojure through the shared lexer (src/core/lisp/dialect.lisp)

(defun racket-tokens (line &optional (state '(:code 0)))
  (token-summary line state c:*racket-dialect*))

(defun clojure-tokens (line &optional (state '(:code 0)))
  (token-summary line state c:*clojure-dialect*))

(define-test racket-lexer :parent cadre-tests
  (is equal '((:open "(") (:symbol "define") (:open "(") (:symbol "f") (:symbol "x") (:close ")")
              (:open "[") (:symbol "x") (:number "1") (:close "]") (:close ")"))
      (racket-tokens "(define (f x) [x 1])"))
  (let ((tokens (c:lex-line "(define (f x) [x 1])" '(:code 0) c:*racket-dialect*)))
    (is eq :head (c:token-subtype (second tokens)))
    (is eq :name (c:token-subtype (fourth tokens)) "(define (f …)) names f")
    (is eql #\[ (c:token-subtype (nth 6 tokens)))
    (is eql #\] (c:token-subtype (nth 9 tokens))))
  (is eq :name (c:token-subtype (third (c:lex-line "(define x 1)" '(:code 0) c:*racket-dialect*))))
  (is equal '((:datum-comment "#;") (:open "(") (:symbol "a") (:close ")") (:keyword "#:kw")
              (:character "#\\λ") (:character "#\\space") (:constant "#t") (:constant "#false")
              (:symbol ":x"))
      (racket-tokens "#;(a) #:kw #\\λ #\\space #t #false :x"))
  (is equal '((:string "#rx\"a.b\"") (:string "#px#\"x\"") (:string "#\"bytes\"") (:open "#hash(")
              (:open "(") (:symbol "a") (:symbol ".") (:number "1") (:close ")") (:close ")")
              (:open "#(") (:number "1") (:close ")") (:open "#s(") (:symbol "p") (:close ")"))
      (racket-tokens "#rx\"a.b\" #px#\"x\" #\"bytes\" #hash((a . 1)) #(1) #s(p)"))
  (is equal '((:quote "#'") (:symbol "x") (:quote "#`") (:symbol "y") (:quote "#,") (:symbol "z")
              (:quote "#,@") (:symbol "w") (:quote ",@") (:symbol "v"))
      (racket-tokens "#'x #`y #,z #,@w ,@v"))
  (is equal '((:number "#x1F") (:number "#e1.5") (:number "+inf.0") (:number "-1/2") (:symbol "1+"))
      (racket-tokens "#x1F #e1.5 +inf.0 -1/2 1+"))
  (is equal '((:reader "#lang") (:symbol "racket/base")) (racket-tokens "#lang racket/base"))
  (is equal '((:comment "#! /usr/bin/env racket")) (racket-tokens "#! /usr/bin/env racket"))
  (is equal '((:comment "#| a |#") (:symbol "|a b|") (:comment "; c"))
      (racket-tokens "#| a |# |a b| ; c"))
  (is equal '(:block 0 1) (nth-value 1 (c:lex-line "#| open" '(:code 0) c:*racket-dialect*)))
  ;; Here strings run until a line that is just their terminator.
  (multiple-value-bind (tokens end) (racket-tokens "(f #<<END")
    (is equal '((:open "(") (:symbol "f") (:string "#<<END")) tokens)
    (is equal '(:here 1 "END") end)
    (multiple-value-bind (tokens2 end2) (racket-tokens "  (not code \"" end)
      (is equal '((:string "  (not code \"")) tokens2)
      (is equal end end2))
    (multiple-value-bind (tokens3 end3) (racket-tokens "END" end)
      (is equal '((:string "END")) tokens3)
      (is equal '(:code 1) end3))))

(define-test clojure-lexer :parent cadre-tests
  (is equal '((:open "(") (:symbol "defn") (:symbol "foo") (:open "[") (:symbol "x") (:close "]")
              (:open "{") (:keyword ":a") (:number "1") (:keyword ":b") (:open "#{") (:number "2")
              (:close "}") (:close "}") (:close ")"))
      (clojure-tokens "(defn foo [x] {:a 1, :b #{2}})"))
  (let ((tokens (c:lex-line "(defn foo [x] x)" '(:code 0) c:*clojure-dialect*)))
    (is eq :head (c:token-subtype (second tokens)))
    (is eq :name (c:token-subtype (third tokens))))
  (is eq :name (c:token-subtype (third (c:lex-line "(ns my.app)" '(:code 0) c:*clojure-dialect*))))
  (is eq :name (c:token-subtype (third (c:lex-line "(clojure.core/def x 1)" '(:code 0) c:*clojure-dialect*))))
  (let ((tokens (c:lex-line "#(+ % 1)" '(:code 0) c:*clojure-dialect*)))
    (is eq :head (c:token-subtype (second tokens)) "#( is a function: its first element is called"))
  (is equal '((:datum-comment "#_") (:open "(") (:symbol "a") (:close ")") (:string "#\"re\\d\"")
              (:quote "#'") (:symbol "v") (:quote "@") (:symbol "at") (:quote "~") (:symbol "x")
              (:quote "~@") (:symbol "xs") (:quote "^") (:keyword ":private") (:symbol "y"))
      (clojure-tokens "#_(a) #\"re\\d\" #'v @at ~x ~@xs ^:private y"))
  (is equal '((:character "\\a") (:character "\\newline") (:character "\\(") (:character "\\,")
              (:character "\\u00e9"))
      (clojure-tokens "\\a \\newline \\( \\, \\u00e9"))
  (is equal '((:number "42N") (:number "1.5M") (:number "0x1F") (:number "2r101") (:number "-3/4")
              (:number "##Inf") (:symbol "1d5"))
      (clojure-tokens "42N 1.5M 0x1F 2r101 -3/4 ##Inf 1d5"))
  (is equal '((:symbol "foo/bar") (:keyword "::kw") (:keyword ":ns/k") (:symbol "x'") (:symbol "a#")
              (:symbol "|a|"))
      (clojure-tokens "foo/bar ::kw :ns/k x' a# |a|"))
  (is equal '((:open "#?(") (:keyword ":clj") (:number "1") (:close ")") (:open "#?@(")
              (:keyword ":cljs") (:open "[") (:number "2") (:close "]") (:close ")") (:open "#:user{")
              (:keyword ":a") (:number "1") (:close "}"))
      (clojure-tokens "#?(:clj 1) #?@(:cljs [2]) #:user{:a 1}"))
  (is equal '((:reader "#inst") (:string "\"2020\"") (:reader "#my/tag") (:open "[") (:close "]"))
      (clojure-tokens "#inst \"2020\" #my/tag []"))
  (is equal '(:code 0) (nth-value 1 (c:lex-line "|a #| b" '(:code 0) c:*clojure-dialect*))
      "No |symbols| or #| comments in Clojure")
  (is equal '(:string 1) (nth-value 1 (c:lex-line "(str \"two" '(:code 0) c:*clojure-dialect*))))

(define-test dialect-syntax :parent cadre-tests
  (flet ((syntax (string dialect) (c:make-lisp-syntax (c:make-string-text string) :dialect dialect)))
    (let ((s (syntax "(let ([x 1]) x)" c:*racket-dialect*)))
      (is equal '(0 15) (multiple-value-list (c:forward-sexp-position s 0 0)))
      (is equal '(0 11) (multiple-value-list (c:forward-sexp-position s 0 6)) "over a [ ] list"))
    ;; A bracket of the wrong kind still pairs by depth, but isn't a match.
    (is eq nil (fifth (multiple-value-list (c:paren-match-at (syntax "(a]" c:*racket-dialect*) 0 3))))
    (is eq t (fifth (multiple-value-list (c:paren-match-at (syntax "[a]" c:*racket-dialect*) 0 3))))
    (is eq t (fifth (multiple-value-list (c:paren-match-at (syntax "#{1}" c:*clojure-dialect*) 0 0))))
    ;; #_ belongs to the form after it, as #+ does in Common Lisp.
    (is equal '(0 7) (multiple-value-list (c:forward-sexp-position (syntax "#_(a b) c" c:*clojure-dialect*) 0 0)))
    (is equal '(0 0) (multiple-value-list (c:backward-sexp-position (syntax "#_(a b) c" c:*clojure-dialect*) 0 7)))
    ;; In Common Lisp, [ and ] are still parts of symbols.
    (is equal '((:open "(") (:symbol "a[b]") (:close ")")) (token-summary "(a[b])" '(:code 0)))
    (is eq c:*common-lisp-dialect* (c:syntax-dialect (syntax-of "(a)")))))

;;; R1: indentation, faces, outlines and paredit in Racket and Clojure

(defun dialect-syntax (dialect &rest lines)
  (c:make-lisp-syntax (c:make-string-text (format nil "~{~a~^~%~}" lines)) :dialect dialect))

(defun misindented-lines (dialect &rest lines)
  "The lines of LINES (already indented as DIALECT's style wants) whose
indentation Cadre would change, as (line wanted)."
  (let ((syntax (apply #'dialect-syntax dialect lines)))
    (loop for string in lines
          for line from 0
          for wanted = (c:lisp-indentation syntax line)
          unless (eql wanted (position #\Space string :test-not #'char=))
            collect (list line wanted))))

(define-test dialect-indentation :parent cadre-tests
  (is equal '() (misindented-lines c:*racket-dialect*
                                   "(define (f x)"
                                   "  (let ([y 1]"
                                   "        [z 2])"
                                   "    (+ x"
                                   "       y)))"))
  (is equal '() (misindented-lines c:*racket-dialect*
                                   "(let loop ([i 0])"
                                   "  (when (< i 3)"
                                   "    (loop (add1 i))))"))
  (is equal '() (misindented-lines c:*racket-dialect*
                                   "(for/fold ([acc 0])"
                                   "    ([x xs])"
                                   "  (+ acc x))"))
  (is equal '() (misindented-lines c:*racket-dialect*
                                   "(cond"
                                   "  [(zero? n)"
                                   "   1]"
                                   "  [else (* n"
                                   "           2)])"))
  (is equal '() (misindented-lines c:*clojure-dialect*
                                   "(defn foo"
                                   "  [x]"
                                   "  (let [y 1"
                                   "        z 2]"
                                   "    (str x"
                                   "         y)))"))
  (is equal '() (misindented-lines c:*clojure-dialect* "{:a 1" " :b #{2" "      3}}"))
  (is equal '() (misindented-lines c:*clojure-dialect* "(foo" " bar)"))
  (is equal '() (misindented-lines c:*clojure-dialect*
                                   "(ns my.app"
                                   "  (:require [a]"
                                   "            [b]))"))
  (is equal '() (misindented-lines c:*clojure-dialect* "(cond" "  a b" "  c d)"))
  (is equal '() (misindented-lines c:*clojure-dialect* "(clojure.core/let [a 1]" "  a)"))
  (is equal '((1 2)) (misindented-lines c:*clojure-dialect* "(defn f []" "    1)") "a body line is put back at 2"))

(defun faces-of (line dialect)
  (let ((faces (mapcar (lambda (tk) (c:token-face tk line dialect)) (c:lex-line line '(:code 0) dialect))))
    (remove-if (lambda (f) (or (null f) (consp f))) faces)))

(define-test dialect-faces :parent cadre-tests
  (is equal '(:definer :definition-name :builtin :constant :constant :string)
      (faces-of "(define (f x) (if #t null \"s\"))" c:*racket-dialect*))
  (is equal '(:definer :definition-name :builtin :constant :lambda-keyword)
      (faces-of "(defn f [x] (when nil &))" c:*clojure-dialect*))
  (is equal '(:definer :quote :keyword :definition-name)
      (faces-of "(defn ^:private f [])" c:*clojure-dialect*) "metadata doesn't hide the name")
  (is equal '(:definer :definition-name)
      (faces-of "(clojure.core/defn f [])" c:*clojure-dialect*))
  (is equal '(:builtin) (faces-of "(class object% (super-new))" c:*racket-dialect*)
      "class isn't a definer, so object% isn't a name")
  ;; What #_ comments out, across lines.
  (let ((s (dialect-syntax c:*clojure-dialect* "(a #_(b" " c) d)" "e")))
    (is equal '((3 7)) (c:datum-comment-columns s 0))
    (is equal '((0 3)) (c:datum-comment-columns s 1))
    (is equal '() (c:datum-comment-columns s 2)))
  (is equal '((0 7)) (c:datum-comment-columns (dialect-syntax c:*racket-dialect* "#;(a b) c") 0))
  (is equal '() (c:datum-comment-columns (syntax-of "#+nil (a b)") 0) "not in Common Lisp"))

(define-test dialect-outline :parent cadre-tests
  (flet ((outline (dialect &rest lines)
           (mapcar (lambda (d) (list (c:definition-name d) (c:definition-kind d) (c:definition-line d)))
                   (c:dialect-definitions (apply #'dialect-syntax dialect lines)))))
    (is equal '(("f" :function 1) ("y" :variable 2) ("p" :class 3) ("m" :macro 4))
        (outline c:*racket-dialect* "#lang racket" "(define (f x) x)" "(define y 2)" "(struct p (a))"
                 "(define-syntax-rule (m) 1)"))
    (is equal '(("my.app" :namespace 0) ("g" :function 1) ("z" :variable 2) ("mm" :macro 3))
        (outline c:*clojure-dialect* "(ns my.app)" "(defn- g [x] (let [q 1] q))" "(def ^:private z 1)"
                 "(defmacro mm [] nil)"))))

(defun dialect-paredit (dialect function string &rest args)
  "STRING read as DIALECT after FUNCTION, with the cursor shown as |."
  (let* ((text (text-with-cursor string))
         (syntax (c:make-lisp-syntax text :dialect dialect)))
    (multiple-value-bind (line column) (c:text-position-line text (c:text-point text))
      (multiple-value-bind (edits point stick delta) (apply function syntax line column args)
        (c:apply-edits text edits point (or stick :before) (or delta 0))
        (shown-with-cursor text)))))

(define-test dialect-paredit :parent cadre-tests
  (is string= "(let ([|]))"
      (dialect-paredit c:*racket-dialect* #'c:paredit-open "(let (|))" :open "[" :close "]"))
  (is string= "[a b]|"
      (dialect-paredit c:*clojure-dialect* #'c:paredit-close "[a |b ]" :close "]"))
  (is string= "[a |b]" (dialect-paredit c:*racket-dialect* #'c:paredit-join "[a] |[b]"))
  (fail (dialect-paredit c:*racket-dialect* #'c:paredit-join "[a] |(b)") 'c:editor-error)
  (is string= "\\(|" (dialect-paredit c:*clojure-dialect* #'c:paredit-open "\\|") "a character, not a list")
  (is string= "(a [| b] c)" (dialect-paredit c:*clojure-dialect* #'c:paredit-slurp-forward "(a [|] b c)")))

;;; Scribble: @-expressions, whose bodies are prose

(define-test scribble-lexing :parent cadre-tests
  (flet ((lex (lines)
           (let ((state (c:dialect-initial-state c:*scribble-dialect*)) (all '()))
             (dolist (line lines (values (nreverse all) state))
               (multiple-value-bind (tokens end) (c:lex-line line state c:*scribble-dialect*)
                 (push (mapcar (lambda (tk) (list (c:token-type tk) (subseq line (c:token-start tk) (c:token-end tk))))
                               tokens)
                       all)
                 (setf state end))))))
    (multiple-value-bind (lines state)
        (lex '("#lang scribble/manual"
               "@title{Don't \"quote\" | me}"
               "Some @bold{nested {braces}} text."
               "@racket[(define x 1)] and @(+ 1 2)."
               "@section{Two"
               "lines}"))
      (is equal '((:reader "#lang scribble/manual")) (first lines))
      (is equal '((:quote "@") (:symbol "title") (:open "{") (:text "Don't \"quote\" | me") (:close "}"))
          (second lines) "prose in a body isn't code: quotes and bars are text")
      (is equal '((:text "Some ") (:quote "@") (:symbol "bold") (:open "{") (:text "nested {braces}")
                  (:close "}") (:text " text."))
          (third lines) "balanced braces in prose are prose")
      (is equal '((:quote "@") (:symbol "racket") (:open "[") (:open "(") (:symbol "define") (:symbol "x")
                  (:number "1") (:close ")") (:close "]") (:text " and ") (:quote "@") (:open "(")
                  (:symbol "+") (:number "1") (:number "2") (:close ")") (:text "."))
          (fourth lines) "[…] and (…) after @ are code")
      (is equal '((:text "lines") (:close "}")) (sixth lines) "a body can span lines")
      (is equal '(:text 0 ((:text 0 :top))) state "and the file ends back in its top-level prose"))))

;;; The project outline's groups (core lisp/definitions.lisp)

(define-test file-outline :parent cadre-tests
  (let ((cl (c:file-outline "(defun before () 1)
(in-package #:my-app)
(defun start () 2)
(eval-when (:compile-toplevel) (defmacro m () 3))
(in-package :other)
(defvar *x* 4)
" 'c:lisp-mode)))
    (is equal '((nil . "before") ("my-app" . "start") ("my-app" . "m") ("other" . "*x*"))
        (mapcar (lambda (e) (cons (car e) (c:definition-name (cdr e)))) cl)))
  (let ((clj (c:file-outline "(ns app.core
  (:require [clojure.string :as s]))
(defn greet [n] n)
(def answer 42)
" 'c:clojure-mode)))
    (is equal '(("app.core" . "greet") ("app.core" . "answer"))
        (mapcar (lambda (e) (cons (car e) (c:definition-name (cdr e)))) clj)))
  (let ((rkt (c:file-outline "#lang racket
(define (f x) x)
" 'c:racket-mode)))
    (is equal '((nil . "f")) (mapcar (lambda (e) (cons (car e) (c:definition-name (cdr e)))) rkt))))
