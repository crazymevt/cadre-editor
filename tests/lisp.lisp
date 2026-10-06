(in-package #:cadre-tests)

(defun syntax-of (string)
  (c:make-lisp-syntax (c:make-string-text string)))

(defun token-summary (line state)
  (multiple-value-bind (tokens end) (c:lex-line line state)
    (values (mapcar (lambda (tk) (list (c:token-type tk) (subseq line (c:token-start tk) (c:token-end tk))))
                    tokens)
            end)))

(define-test lexer :parent cadre-tests
  (is equal '((:open "(") (:symbol "defun") (:symbol "foo") (:open "(") (:symbol "x") (:close ")")
              (:string "\"doc\"") (:comment "; c"))
      (token-summary "(defun foo (x) \"doc\" ; c" '(:code 0)))
  (is equal '(:code 1) (nth-value 1 (c:lex-line "(defun foo (x)" '(:code 0))))
  (is equal '((:character "#\\Space") (:character "#\\(") (:quote "#'") (:symbol "car")
              (:keyword ":k") (:number "1.5e3") (:number "-2/3") (:number "#x1F") (:symbol "1+"))
      (token-summary "#\\Space #\\( #'car :k 1.5e3 -2/3 #x1F 1+" '(:code 0)))
  (is equal '(:block 0 1) (nth-value 1 (c:lex-line "#| a #| b |# c" '(:code 0))))
  (is equal '((:comment "end |#") (:symbol "x")) (token-summary "end |# x" '(:block 0 1)))
  (is equal '(:string 3) (nth-value 1 (c:lex-line "\"open" '(:code 3))))
  (is equal '((:string "rest\"") (:open "(") (:symbol "f") (:close ")"))
      (token-summary "rest\" (f)" '(:string 0)))
  (is equal '((:reader-conditional "#+") (:symbol "sbcl") (:open "#(") (:number "1") (:close ")")
              (:symbol "#:g") (:symbol "|a b|") (:symbol "p::s"))
      (token-summary "#+sbcl #(1) #:g |a b| p::s" '(:code 0)))
  (is equal '((:close ")") (:invalid ")")) (token-summary "))" '(:code 1)))
  (let ((tokens (c:lex-line "(defun foo #+x y)" '(:code 0))))
    (is eq :head (c:token-subtype (second tokens)))
    (is eq :name (c:token-subtype (third tokens)))
    (is eq :feature (c:token-subtype (fifth tokens)))))

(define-test faces :parent cadre-tests
  (let* ((line "(defun foo (&key x) (when *v* +c+ :k t) \"s\" 1)")
         (faces (mapcar (lambda (tk) (c:token-face tk line)) (c:lex-line line '(:code 0)))))
    (is equal '((:paren 0) :definer :definition-name (:paren 1) :lambda-keyword nil (:paren 1)
                (:paren 1) :builtin :special-variable :constant :keyword :constant (:paren 1)
                :string :number (:paren 0))
        faces)))

(define-test incremental-lexing :parent cadre-tests
  (let* ((text (c:make-string-text (format nil "(a~% b)~%c")))
         (syntax (c:make-lisp-syntax text)))
    (c:ensure-lexed syntax 2)
    (is equal '(:code 0) (c:line-end-state syntax 2))
    ;; Typing a quote at the start turns everything into an unterminated string.
    (c:text-insert text 0 "\"")
    (c:syntax-lines-changed syntax 0 1 1)
    (is equal '(:string 0) (c:line-end-state syntax 2))
    (is eq :string (c:token-type (aref (c:line-tokens syntax 1) 0)))
    ;; Inserting a newline splits a line.
    (c:text-delete text 0 1)
    (c:syntax-lines-changed syntax 0 1 1)
    (c:text-insert text 2 (string #\Newline))
    (c:syntax-lines-changed syntax 0 1 2)
    (is = 4 (c:syntax-line-count syntax))
    (is equal '(:code 1) (c:line-end-state syntax 1))
    (is equal '(:code 0) (c:line-end-state syntax 3))
    ;; Changes compared with a fresh lex, line by line.
    (let ((fresh (c:make-lisp-syntax text)))
      (dotimes (i 4)
        (is equalp (c:line-tokens fresh i) (c:line-tokens syntax i))))))

(define-test lazy-lexing :parent cadre-tests
  (let* ((text (c:make-string-text (format nil "~{~a~%~}" (loop repeat 1000 collect "(foo bar)"))))
         (syntax (c:make-lisp-syntax text)))
    (c:ensure-lexed syntax 10)
    (is = 11 (cadre::syntax-first-unchecked syntax))
    ;; An edit at line 5 re-lexes line 5 and stops when the next line agrees.
    (c:ensure-lexed syntax 1000)
    (c:text-insert text (c:text-line-position text 5) ";")
    (c:syntax-lines-changed syntax 5 1 1)
    (c:ensure-lexed syntax 1000)
    (is eq nil (cadre::syntax-first-unchecked syntax))
    (is eq :comment (c:token-type (aref (c:line-tokens syntax 5) 0)))))

(define-test sexp-navigation :parent cadre-tests
  (let ((s (syntax-of "(a (b c) \"s\" 'd) e")))
    (flet ((fwd (col) (multiple-value-list (c:forward-sexp-position s 0 col)))
           (back (col) (multiple-value-list (c:backward-sexp-position s 0 col))))
      (is equal '(0 16) (fwd 0))
      (is equal '(0 2) (fwd 1))
      (is equal '(0 8) (fwd 2))
      (is equal '(0 12) (fwd 8))
      (is equal '(0 15) (fwd 12))
      (is equal '(nil) (fwd 15))
      (is equal '(0 18) (fwd 16))
      (is equal '(0 0) (back 16))
      (is equal '(0 13) (back 15))
      (is equal '(0 9) (back 12))
      (is equal '(nil) (back 1))
      (is equal '(0 3) (multiple-value-list (c:up-list-position s 0 5)))
      (is equal '(0 0) (multiple-value-list (c:up-list-position s 0 3)))
      (is equal '(nil) (multiple-value-list (c:up-list-position s 0 17)))
      (is equal '(0 4) (multiple-value-list (c:down-list-position s 0 2)))))
  (let ((s (syntax-of (format nil "(foo \"a~%b\" c)"))))
    (is equal '(1 2) (multiple-value-list (c:forward-sexp-position s 0 4)))
    (is equal '(0 5) (multiple-value-list (c:backward-sexp-position s 1 2)))))

(define-test defun-navigation :parent cadre-tests
  (let ((s (syntax-of (format nil "(defun a ()~%  1)~%~%'(defun b ()~%  2)"))))
    (is equal '(0 0) (multiple-value-list (c:beginning-of-defun-position s 1 3)))
    (is equal '(1 4) (multiple-value-list (c:end-of-defun-position s 1 0)))
    (is equal '(4 4) (multiple-value-list (c:end-of-defun-position s 2 0)))
    (is equal '(0 0 1 4) (multiple-value-list (c:toplevel-form-bounds s 1 4)))
    (is equal '(3 0) (multiple-value-list (c:beginning-of-defun-position s 4 1)))))

(define-test paren-matching :parent cadre-tests
  (let ((s (syntax-of "(a (b) c)) #(1)")))
    (is equal '(0 3 0 5 t) (multiple-value-list (c:paren-match-at s 0 6)))
    (is equal '(0 8 0 0 t) (multiple-value-list (c:paren-match-at s 0 0)))
    (is equal '(0 9 0 9 nil) (multiple-value-list (c:paren-match-at s 0 10)))
    (is equal '(0 14 0 12 t) (multiple-value-list (c:paren-match-at s 0 11)))
    (is equal '(nil) (multiple-value-list (c:paren-match-at s 0 2)))))

(defparameter *indented-sample*
  "(defun foo (x y)
  \"Doc.
Second line of the docstring.\"
  (let ((a 1)
        (b 2))
    (when (> a b)
      (print a))
    (if x
        y
        (list a
              b))
    (mapcar #'car
            '((1 2)
              (3 4)))
    (frob
     x)
    (flet ((helper (z)
             (* z 2)))
      (helper a))
    ;; a comment
    (list :a 1
          :b 2)))

(defclass point ()
  ((x :initarg :x)
   (y :initarg :y))
  (:documentation \"A point.\"))

(cadre:define-command thing ()
  (:modes lisp-mode)
  (with-open-file (s \"f\")
    (read s)))")

(define-test indentation :parent cadre-tests
  (let* ((text (c:make-string-text *indented-sample*))
         (s (c:make-lisp-syntax text)))
    (dotimes (line (c:text-line-count text))
      (let* ((string (c:text-line-string text line))
             (actual (or (position #\Space string :test-not #'char=) (length string)))
             (expected (c:lisp-indentation s line)))
        (when (and expected (plusp (length string)))
          (is = actual expected "line ~d: ~s" (1+ line) string))))
    (is eq nil (c:lisp-indentation s 2) "inside a docstring")))

(define-test indentation-specs :parent cadre-tests
  (is = 1 (c:indentation-spec "when"))
  (is = 2 (c:indentation-spec "cadre:define-command"))
  (is = 1 (c:indentation-spec "with-foo"))
  (is eq nil (c:indentation-spec "list"))
  ;; Swank's :indentation-update: (name indent packages) for macros with &body.
  (unwind-protect
       (progn
         (c:learn-indentation (list '("then-when" (4 "&body") ("CADRE-SMOKE"))
                                    (list "frob-two" (list 4 4 (c:remote-symbol "&body")) '("CL-USER"))
                                    '("my-progn" 0 ("CL-USER"))))
         (is = 1 (c:indentation-spec "then-when"))
         (is = 2 (c:indentation-spec "frob-two"))
         (is = 0 (c:indentation-spec "foo:my-progn"))
         (let ((s (c:make-lisp-syntax (c:make-string-text (format nil "(then-when ((ready) :timeout 3)~%x)")))))
           (is = 2 (c:lisp-indentation s 1) "the body of a learned macro"))
         ;; Redefined without &body: back to a call. Built-in specs stay.
         (c:learn-indentation '(("then-when" nil ("CADRE-SMOKE")) ("when" nil ("CL"))))
         (is eq nil (c:indentation-spec "then-when"))
         (is = 1 (c:indentation-spec "when")))
    (c:learn-indentation '(("then-when" nil ()) ("frob-two" nil ()) ("my-progn" nil ())))))

(define-test fuzzy :parent cadre-tests
  (true (c:fuzzy-match "sb" "save-buffer"))
  (false (c:fuzzy-match "xyz" "save-buffer"))
  (true (c:fuzzy-match "" "anything"))
  (is equal '(0 5) (nth-value 1 (c:fuzzy-match "sb" "save-buffer")))
  (is equal '("save-buffer" "set-mark-buffer")
      (c:fuzzy-filter "sb" '("scroll-down-page" "set-mark-buffer" "save-buffer")))
  (is equal '("src/ui/window.lisp")
      (c:fuzzy-filter "win" '("src/core/text.lisp" "src/ui/window.lisp") :limit 1)))

(define-test fold-ranges :parent cadre-tests
  (let ((syntax (c:make-lisp-syntax (c:make-string-text (format nil "(defun f (x)~%  (let ((y 1))~%    (+ x~%       y)))~%(g 1)~%(h~% 2)~%")))))
    ;; One range per line, the longest: the DEFUN's, not (x)'s; (g 1) is on one line.
    (is equal '((0 3) (1 3) (2 3) (5 6)) (c:lisp-fold-ranges syntax))
    (is equal '(2 3) (c:fold-range-at (c:lisp-fold-ranges syntax) 3))
    (is equal '(0 3) (c:fold-range-at (c:lisp-fold-ranges syntax) 0))
    (is eq nil (c:fold-range-at (c:lisp-fold-ranges syntax) 4)))
  ;; An unclosed list folds nothing; a paren in a string or comment doesn't count.
  (is equal '() (c:lisp-fold-ranges (c:make-lisp-syntax (c:make-string-text (format nil "(a~%b")))))
  (is equal '((0 1)) (c:lisp-fold-ranges (c:make-lisp-syntax (c:make-string-text (format nil "(a \"(\" ; (~%b)~%")))))
  (is equal '((0 5) (2 5) (3 5) (7 10) (9 10))
      (c:markdown-fold-ranges (format nil "# Title~%intro~%## A~%```~%# not a heading~%```~%~%# Next~%~%## B~%text~%~%"))))

(define-test tree-sitter-pure :parent cadre-tests
  (is eq :code-function (c:capture-face "function.method"))
  (is eq :code-function (c:capture-face "function"))
  (is eq :builtin (c:capture-face "function.builtin"))
  (is eq :code-property (c:capture-face "string.special.key"))
  (is eq nil (c:capture-face "variable"))
  (is eq nil (c:capture-face "punctuation.bracket"))
  (flet ((text (i) (aref #("FOO" "foo") i)))
    (true (c:predicate-holds-p '("match?" (:capture 0) "^[A-Z]+$") #'text))
    (false (c:predicate-holds-p '("match?" (:capture 1) "^[A-Z]+$") #'text))
    (true (c:predicate-holds-p '("not-match?" (:capture 1) "^[A-Z]+$") #'text))
    (true (c:predicate-holds-p '("eq?" (:capture 1) "foo") #'text))
    (false (c:predicate-holds-p '("not-eq?" (:capture 1) "foo") #'text))
    (true (c:predicate-holds-p '("any-of?" (:capture 0) "BAR" "FOO") #'text))
    (true (c:predicate-holds-p '("is-not?" "local") #'text)))
  (is string= "javascript" (c:tree-sitter-language-for-file #p"a/b.mjs"))
  (is string= "tsx" (c:tree-sitter-language-for-file #p"x.tsx"))
  (is eq nil (c:tree-sitter-language-for-file #p"x.lisp")))

(define-test tree-sitter-languages :parent cadre-tests
  ;; Needs libtree-sitter and the grammars (M-x install-language-grammar); skipped without them.
  (if (not (and (c:tree-sitter-prefix) (every #'c:tree-sitter-language-installed-p '("javascript" "typescript" "json"))))
      (skip "tree-sitter grammars aren't installed" (true t))
      (flet ((faces (language text)
               (let ((document (c:make-ts-document (c:load-ts-language language))))
                 (c:ts-parse document text)
                 (values (mapcar (lambda (s) (list (third s) (subseq text (first s) (second s))))
                                 (c:ts-highlight-spans document 0 (length text)))
                         document))))
        (multiple-value-bind (spans document)
            (faces "javascript" (format nil "// hi~%const MAX = 1;~%function greet(name) {~%  console.log(\"é\", name);~%}~%"))
          (true (member '(:comment "// hi") spans :test #'equal))
          (true (member '(:constant "MAX") spans :test #'equal))
          (true (member '(:code-function "greet") spans :test #'equal))
          (true (member '(:builtin "console") spans :test #'equal))
          ;; Non-ASCII text keeps char offsets right.
          (true (member '(:string "\"é\"") spans :test #'equal))
          (is equal '((2 4)) (c:ts-fold-ranges document))
          (is equal '(("greet" :function 2 0)) (c:ts-outline document))
          (false (c:ts-has-error-p document)))
        (true (member '(:code-type "Shape") (faces "typescript" "interface Shape { area(): number }") :test #'equal))
        (true (member '(:code-property "\"k\"") (faces "json" "{\"k\": [1, true]}") :test #'equal))
        ;; Only part of the text.
        (let ((text "let a = 1; let b = \"x\";"))
          (let ((document (c:make-ts-document (c:load-ts-language "javascript"))))
            (c:ts-parse document text)
            (is equal '((11 14 :keyword) (19 22 :string)) (c:ts-highlight-spans document 11 (length text))))))))
