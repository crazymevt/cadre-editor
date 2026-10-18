;;;; Renaming: local bindings and symbols across packages (lisp/rename.lisp).
(in-package #:cadre-tests)

(defparameter *rename-source* "(defpackage #:shop (:use #:cl) (:export #:total :price) (:import-from #:util #:total))
(in-package #:shop)
(defun total (items &key (tax 0))
  (let ((sum 0))
    (dolist (item items)
      (let ((sum (+ sum (price item))))
        (print sum)))
    (let* ((total (* sum tax)) (x total)) (list x total))
    (total items)))
(defun mixed () (let ((total 1)) (total total)) (flet ((helper (x) (* x 2))) (list (helper 1) #'helper (helper total))))
(defun other (sum) (loop for item in sum collect (util:total item) collect (shop::total item) collect \"total\" collect :total))
")

(defun place-at (marker &optional (skip 0))
  "The offset just inside MARKER's first symbol, in *rename-source*."
  (let ((at (search marker *rename-source*)))
    (+ at skip (or (position-if (lambda (c) (char/= c #\()) marker) 0))))

(defun place-texts (places)
  (mapcar (lambda (p) (list (first p) (subseq *rename-source* (first p) (second p)))) places))

(defun lexical-places (marker &optional (skip 0))
  (multiple-value-list (c::lexical-rename-places *rename-source* (place-at marker skip))))

(define-test rename-local-bindings :parent cadre-tests
  ;; The outer SUM: its LET, the inner LET's value (LET is parallel) and the
  ;; LET* value; not the inner SUM, which shadows it.
  (destructuring-bind (places others name certain) (lexical-places "(sum 0)")
    (is string= "sum" name)
    (true certain)
    (is equal '(150 203 266) (mapcar #'first places))
    (is equal '(196 238) (mapcar #'first others)))
  ;; The inner SUM: its binding and its use only.
  (is equal '(196 238) (mapcar #'first (first (lexical-places "(sum (+"))))
  ;; LET* sees the earlier binding in a later value.
  (is equal '(257 279 295) (mapcar #'first (first (lexical-places "(total (*"))))
  ;; A call to TOTAL is the function, not the variable TOTAL bound around it.
  (destructuring-bind (places others name certain) (lexical-places "((total 1")
    (declare (ignore name certain))
    (is equal '((346 "total") (363 "total")) (place-texts places))
    (is equal '(357 434) (mapcar #'first others)))
  ;; A local function: its calls and #' references.
  (is equal '(379 407 419 427) (mapcar #'first (first (lexical-places "((helper"))))
  (is equal '(387 393) (mapcar #'first (first (lexical-places "(x) (*"))))
  ;; A global function isn't a local binding.
  (false (first (lexical-places "(defun total" 6)))
  ;; WITH- macros are guessed to bind.
  (multiple-value-bind (places others name certain)
      (c::lexical-rename-places "(with-open-file (in path) (read in))" 17)
    (declare (ignore others name))
    (is = 2 (length places))
    (false certain)))

(define-test rename-classifies-occurrences :parent cadre-tests
  (let* ((contexts (c::symbol-contexts *rename-source* "total"))
         (resolve (lambda (package) (cond ((string= package "SHOP") "SHOP") ((string= package "UTIL") "UTIL"))))
         (kinds (mapcar (lambda (context) (list (c::sc-line context) (c::sc-prefix context)
                                                (c::classify-symbol-context context "SHOP" resolve)))
                        contexts)))
    ;; Strings and the keyword :TOTAL outside DEFPACKAGE aren't occurrences.
    (is equal '((0 "#:" :export) (0 "#:" :other) (2 "" :same)
                (7 "" :local) (7 "" :local) (7 "" :local) (8 "" :same)
                ;; In MIXED: the local TOTAL, the call (the function), the
                ;; local again, and a free TOTAL, which is SHOP::TOTAL.
                (9 "" :local) (9 "" :same) (9 "" :local) (9 "" :same)
                (10 "util:" :other) (10 "shop::" :same))
        kinds)
    (is equal '("UTIL" "SHOP") (c::context-packages contexts))
    ;; Unresolved packages are left for the user to judge.
    (is eq :unknown (c::classify-symbol-context (third contexts) "SHOP" (constantly nil)))
    (is eq :unknown (c::classify-symbol-context (third contexts) nil resolve))
    ;; The keyword in an export list names the symbol, as does the call.
    (is equal '(:export :same) (mapcar (lambda (c) (c::classify-symbol-context c "SHOP" resolve))
                                 (c::symbol-contexts *rename-source* "price")))))

(define-test rename-file-without-in-package :parent cadre-tests
  ;; Code before any IN-PACKAGE is read in CL-USER.
  (let ((contexts (c::symbol-contexts "(defun area (w h) (* w h))
(area 1 2)" "area")))
    (is equal '("COMMON-LISP-USER" "COMMON-LISP-USER") (mapcar #'c::sc-package contexts))
    (is equal '(:same :same)
        (mapcar (lambda (c) (c::classify-symbol-context c "COMMON-LISP-USER"
                                                        (lambda (p) (and (string= p "COMMON-LISP-USER") p))))
                contexts))))

(defun renamed-and-left (text marker)
  "The texts of the places renaming the local at MARKER changes, and of those it leaves."
  (multiple-value-bind (places others) (c::lexical-rename-places text (1+ (search marker text)))
    (flet ((texts (list) (mapcar (lambda (p) (list (first p) (subseq text (max 0 (- (first p) 1)) (second p)))) list)))
      (list (texts places) (texts others)))))

(define-test rename-leaves-quoted-data :parent cadre-tests
  ;; 'x, '(x) and `(x …) are data; ,x in a backquote is the variable.
  (is equal '(((7 "(x") (22 " x") (34 ",x")) ((20 "'x") (26 "(x") (31 "(x")))
      (renamed-and-left "(let ((x 1)) (list 'x x '(x) `(x ,x)))" "(x 1"))
  (is equal '(((7 "(x") (29 " x")) ((20 " x")))
      (renamed-and-left "(let ((x 1)) (quote x) (list x))" "(x 1")))

(define-test rename-sees-tests-and-keys :parent cadre-tests
  ;; A COND clause starts with a test, which uses the variable.
  (is equal '(((7 "(x") (20 "(x") (34 " x")) ())
      (renamed-and-left "(let ((x 1)) (cond (x 'yes) ((not x) 'no)))" "(x 1"))
  ;; A CASE clause starts with keys, which are data.
  (is equal '(((7 "(x") (24 " x")) ((22 "(x")))
      (renamed-and-left "(let ((x 1)) (case y (x x)))" "(x 1"))
  ;; DO's end clause starts with a test too.
  (is equal '((6 "(done") (20 "(done")) (first (renamed-and-left "(do ((done nil t)) (done 'ok))" "(done nil"))))

(define-test rename-sees-lambda-list-defaults :parent cadre-tests
  (let ((text "(defun f (a &optional (b a b-p) &key (c (+ a b))) (list a b c b-p))"))
    ;; A parameter is seen by later parameters' defaults.
    (is equal '((10 "(a") (25 " a") (43 " a") (56 " a")) (first (renamed-and-left text "(a &")))
    (is equal '((23 "(b") (45 " b") (58 " b")) (first (renamed-and-left text "(b a")))
    ;; A supplied-p variable is a binding of its own.
    (is equal '((27 " b-p") (62 " b-p")) (first (renamed-and-left text "b-p)")))))

(define-test rename-skips-package-names :parent cadre-tests
  ;; A package named like the symbol isn't an occurrence of it: not in
  ;; IN-PACKAGE, DEFPACKAGE's name, :use, or :import-from's package.
  (is equal '((":export" "#:"))
      (mapcar (lambda (c) (list (c::sc-clause c) (c::sc-prefix c)))
              (c::symbol-contexts "(defpackage total (:use #:cl #:total) (:import-from #:total #:x) (:export #:total))
(in-package total)" "total"))))
