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
