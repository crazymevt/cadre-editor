(in-package #:cadre-tests)

;;; The test explorer (core swank/test-runner.lisp): tests found in source,
;;; and the helper running them in a real SBCL.

(defparameter *parachute-source* "(defpackage #:shop-tests (:use #:cl #:parachute))
(in-package #:shop-tests)

(define-test shop)

(define-test prices :parent shop
  (is = 3 (+ 1 2)))

(parachute:define-test \"a string name\"
  (true t))

(define-test outer
  (define-test inner
    (false t)))
")

(defparameter *fiveam-source* "(defpackage :cart-tests (:use :cl :fiveam))
(in-package :cart-tests)
(def-suite cart)
(in-suite cart)

(test adds (is (= 2 (+ 1 1))))
(test (removes :suite cart) (is (= 3 (+ 1 1)) \"one and one is ~a\" 3))
")

(define-test test-sources :parent cadre-tests
  (let ((tests (c:source-tests *parachute-source*)))
    (is equal '("shop" "prices" "\"a string name\"" "outer" "inner") (mapcar #'c:source-test-name tests))
    (is equal '("SHOP" "PRICES" "a string name" "OUTER" "INNER") (mapcar #'c:source-test-key tests))
    (is equal '(nil "SHOP" nil nil "OUTER") (mapcar #'c:source-test-parent tests))
    (true (every (lambda (test) (eq :parachute (c:source-test-framework test))) tests))
    (true (every (lambda (test) (equal "shop-tests" (c:source-test-package test))) tests))
    (is equal '(3 5 8 11 12) (mapcar #'c:source-test-line tests))
    (is equal '("shop" "\"a string name\"" "outer") (mapcar #'c:source-test-name (c:test-roots tests)))
    (is equal '(("parachute" "shop-tests" "shop" "\"a string name\"" "outer"))
        (c:test-run-groups (c:test-roots tests)))
    (let ((updated (c:apply-test-results tests '(("SHOP" :passed ()) ("PRICES" :failed ("off by one")) ("NOPE" :passed ()))
                                         "shop-tests")))
      (is = 2 (length updated))
      (is eq :failed (c:source-test-status (second tests)))
      (is equal '("off by one") (c:source-test-messages (second tests)))))
  (let ((tests (c:source-tests *fiveam-source*)))
    (is equal '("adds" "removes") (mapcar #'c:source-test-name tests))
    (true (every (lambda (test) (eq :fiveam (c:source-test-framework test))) tests)))
  (is string= "FOO" (c:test-key "pkg::foo"))
  (is equal '(("A" :passed ()) ("B" :failed ("x")) ("C" :skipped ()))
      (c:parse-test-results '(("A" "passed" ()) ("B" "failed" ("x")) ("C" "skipped" ())))))

(define-test test-runner-helper :parent cadre-tests
  (let ((conn (start-test-lisp)))
    (unwind-protect
         (flet ((load-source (text)
                  (swank conn "swank:interactive-eval-region" text 3 120))
                (run (framework package names)
                  (c:parse-test-results
                   (c:swank-eval-sync conn (list (c:remote-symbol "cadre-test-runner:run") framework package
                                                 (list 'quote names))
                                      :timeout 60))))
           (load-source c:+test-runner-helper+)
           (swank conn "swank:interactive-eval" "(progn (require :asdf) (load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname))) (funcall (find-symbol \"QUICKLOAD\" \"QL\") '(:parachute :fiveam) :silent t) t)" 3 120)
           (load-source *parachute-source*)
           (let ((results (run "parachute" "shop-tests" '("shop" "outer" "\"a string name\""))))
             (is equal '(("SHOP" :passed) ("PRICES" :passed) ("OUTER" :failed) ("INNER" :failed) ("a string name" :passed))
                 (mapcar (lambda (r) (subseq r 0 2)) results))
             ;; The failed check, as parachute describes it
             (true (search "evaluated to" (or (first (third (fourth results))) "")) "~s" (third (fourth results))))
           (let ((results (run "parachute" "shop-tests" '("nothing"))))
             (is eq :failed (second (first results)))
             (true (search "No test" (first (third (first results))))))
           (load-source *fiveam-source*)
           (let ((results (run "fiveam" "cart-tests" '("cart"))))
             (is equal '(("ADDS" :passed) ("REMOVES" :failed)) (mapcar (lambda (r) (subseq r 0 2)) results))
             (is equal '("one and one is 3") (third (second results)))))
      (c:swank-disconnect conn)
      (c:kill-inferior-lisp (c:connection-process conn)))))

(defparameter *clojure-tests-source* "(ns shop.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [shop.core :as core]))

(deftest adds
  (is (= 2 (core/add 1 1))))

(deftest ^:slow broken
  (testing \"[brackets] and (parens) in a string\"
    (is (= 3 (core/add 1 1)))))
")

(defparameter *racket-tests-source* "#lang racket/base
(define (add a b) (+ a b))
(module+ test
  (require rackunit)
  (check-equal? (add 1 1) 2)
  (test-case \"adds\"
    (check-equal? (add 2 2) 4))
  (define-values (x y) (values [list 1] #\\())
  (let ()
    (test-case \"broken\"
      (check-equal? (add 1 1) 3 \"one and one\"))))
")

(define-test other-language-tests :parent cadre-tests
  (let ((tests (c:clojure-source-tests *clojure-tests-source*)))
    (is equal '("adds" "broken") (mapcar #'c:source-test-name tests))
    (is equal '("shop.core-test" "shop.core-test") (mapcar #'c:source-test-package tests))
    (is equal '(4 7) (mapcar #'c:source-test-line tests))
    (is equal '(5 9) (mapcar #'c:source-test-end-line tests))
    (true (every (lambda (test) (eq :clojure (c:source-test-framework test))) tests)))
  (let ((tests (c:racket-source-tests *racket-tests-source*)))
    (is equal '("module+ test" "adds" "broken") (mapcar #'c:source-test-name tests))
    (is equal '(nil "module+ test" "module+ test") (mapcar #'c:source-test-parent tests))
    (is equal '(2 5 9) (mapcar #'c:source-test-line tests))
    ;; raco test's own words, for a file whose test-case "broken" fails
    (let* ((output "raco test: (submod (file \"t.rkt\") test)
--------------------
broken
FAILURE
name:       check-equal?
location:   /p/t.rkt:11:6
message:    \"one and one\"
actual:     2
expected:   3
--------------------
--------------------
FAILURE
name:       check-equal?
location:   /p/t.rkt:5:2
actual:     1
expected:   2
--------------------
2/3 test failures
")
           (failures (c:parse-raco-test-output output)))
      (is equal '("broken" nil) (mapcar #'first failures))
      (is equal '(10 4) (mapcar #'second failures))
      (true (search "one and one" (third (first failures))))
      (c:apply-raco-results tests failures 1 output)
      ;; By line: the innermost test around it; the bare check is the block's.
      (is equal '(:failed :passed :failed) (mapcar #'c:source-test-status tests))
      (true (search "actual:     1" (first (c:source-test-messages (first tests)))))
      (true (search "one and one" (first (c:source-test-messages (third tests))))))
    ;; It didn't compile: every test failed, with the output.
    (c:apply-raco-results tests '() 1 "t.rkt:3:0: read: unexpected `)`")
    (true (every (lambda (test) (eq :failed (c:source-test-status test))) tests))
    (c:apply-raco-results tests '() 0 "4 tests passed")
    (true (every (lambda (test) (eq :passed (c:source-test-status test))) tests)))
  (is equal '("adds") (mapcar #'c:source-test-name (c:mode-source-tests "(deftest adds (is true))" (find-symbol "CLOJURE-MODE" :cadre)))))

(define-test raco-test-run :parent cadre-tests
  ;; The real raco test, where Racket is installed
  (let ((raco (ignore-errors (string-trim '(#\Newline) (uiop:run-program '("which" "raco") :output :string)))))
    (when (and raco (plusp (length raco)))
      (let ((file (merge-pathnames (format nil "cadre-raco-~d/t.rkt" (random 1000000)) (uiop:temporary-directory))))
        (ensure-directories-exist file)
        (unwind-protect
             (progn
               (with-open-file (o file :direction :output) (write-string *racket-tests-source* o))
               (multiple-value-bind (output error code)
                   (uiop:run-program (list raco "test" (uiop:native-namestring file))
                                     :output :string :error-output :output :ignore-error-status t)
                 (declare (ignore error))
                 (let ((tests (c:racket-source-tests *racket-tests-source*)))
                   (c:apply-raco-results tests (c:parse-raco-test-output output) code output)
                   (is equal '(:passed :passed :failed) (mapcar #'c:source-test-status tests)
                       "~a" output))))
          (uiop:delete-directory-tree (uiop:pathname-directory-pathname file) :validate t :if-does-not-exist :ignore))))))
