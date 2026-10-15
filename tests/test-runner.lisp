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
