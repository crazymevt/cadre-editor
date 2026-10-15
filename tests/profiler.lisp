(in-package #:cadre-tests)

;;; The profiler (core swank/profiler.lisp): its report, and the helper in a
;;; real SBCL.

(defparameter *report*
  '(:samples 10 :mode "cpu" :running nil
    :functions (("FIB" "FIB" 6 10 (("FIB" . 8) ("WORK" . 10)) (("FIB" . 8) ("+" . 2)))
                ("WORK" "WORK" 0 10 () (("FIB" . 10)))
                ("(LAMBDA () :IN WORK)" "WORK" 4 4 (("WORK" . 4)) ()))
    :tree ("all" 10 ("WORK" 10 ("FIB" 6 ("FIB" 5)) ("(LAMBDA () :IN WORK)" 4)))))

(define-test profiler-report :parent cadre-tests
  (let ((profile (c:parse-profile-report *report*)))
    (is = 10 (c:profile-samples profile))
    (is equal nil (c:profile-running profile))
    (is equal '("FIB" "WORK" "(LAMBDA () :IN WORK)") (mapcar #'c:profile-function-name (c:profile-functions profile)))
    (let ((fib (c:find-profile-function profile "FIB")))
      (is = 6 (c:profile-function-self fib))
      (is equal '(("FIB" . 8) ("WORK" . 10)) (c:profile-function-callers fib)))
    (is string= "WORK" (c:profile-function-definition (c:find-profile-function profile "(LAMBDA () :IN WORK)")))
    (is equal '("FIB" "(LAMBDA () :IN WORK)" "WORK")
        (mapcar #'c:profile-function-name (c:sort-profile-functions (c:profile-functions profile) :self)))
    (is equal '("(LAMBDA () :IN WORK)" "FIB" "WORK")
        (mapcar #'c:profile-function-name (c:sort-profile-functions (c:profile-functions profile) :name)))
    (is equal '("WORK" "(LAMBDA () :IN WORK)")
        (mapcar #'c:profile-function-name (c:sort-profile-functions (c:profile-functions profile) :total :filter "work")))
    (is string= "60.0%" (c:profile-percent 6 10))
    (is string= "–" (c:profile-percent 0 0))
    (is string= "allocation" (c:profile-mode-title "alloc"))
    ;; The flame graph
    (let* ((tree (c:profile-tree profile))
           (boxes (c:flame-boxes tree)))
      (is equal '("all" "WORK" "FIB" "FIB" "(LAMBDA () :IN WORK)") (mapcar (lambda (b) (first (first b))) boxes))
      (is equal '(0 1 2 3 2) (mapcar #'second boxes))
      (is = 4 (c:flame-depth boxes))
      (let ((lambda-box (fifth boxes)))
        (true (< (abs (- 0.6 (third lambda-box))) 1e-6))
        (true (< (abs (- 0.4 (fourth lambda-box))) 1e-6)))
      ;; Zoomed into the outer FIB: the way down stays, FIB takes the width.
      (let ((zoomed (c:flame-boxes tree :path '("WORK" "FIB"))))
        (is equal '("all" "WORK" "FIB" "FIB") (mapcar (lambda (b) (first (first b))) zoomed))
        (is equal '(1.0 1.0 1.0) (mapcar #'fourth (subseq zoomed 0 3)))
        (true (< (abs (- 5/6 (fourth (fourth zoomed)))) 1e-6)))
      (let ((inner (c:flame-node-at tree '("WORK" "FIB" "FIB"))))
        (is = 5 (second inner))
        (is equal '("WORK" "FIB" "FIB") (c:flame-path-to tree inner)))
      (is equal nil (c:flame-node-at tree '("nowhere"))))))

(define-test profiler-helper :parent cadre-tests
  (let ((conn (start-test-lisp)))
    (unwind-protect
         (progn
           (swank conn "swank:interactive-eval-region" c:+profiler-helper+ 3 120)
           (swank conn "swank:interactive-eval-region"
                  "(defun cadre-fib (n) (if (< n 2) n (+ (cadre-fib (- n 1)) (cadre-fib (- n 2)))))
(defun cadre-work () (loop repeat 2000 sum (cadre-fib 20)))" 3 120)
           (true (search "=>" (swank conn "swank:interactive-eval"
                                     "(cadre-profiler:run (lambda () (cadre-work)) :mode :cpu)" 3 120)))
           (let ((profile (c:parse-profile-report
                           (c:swank-eval-sync conn (list (c:remote-symbol "cadre-profiler:report"))))))
             (true (< 10 (c:profile-samples profile)) "~d samples" (c:profile-samples profile))
             (let ((fib (c:find-profile-function profile "CADRE-FIB")))
               (true fib)
               (true (<= (* 0.8 (c:profile-samples profile)) (c:profile-function-total fib)))
               (is string= "CADRE-FIB" (c:profile-function-definition fib)))
             ;; Only the form's stacks: the tree starts at the form's code.
             (is equal '("CADRE-WORK") (mapcar #'first (cddr (c:profile-tree profile))))
             (false (c:profile-running profile)))
           ;; Profiling everything, until stopped
           (c:swank-eval-sync conn (list (c:remote-symbol "cadre-profiler:start") :mode :cpu))
           (true (c:profile-running (c:parse-profile-report
                                     (c:swank-eval-sync conn (list (c:remote-symbol "cadre-profiler:report"))))))
           (swank conn "swank:interactive-eval" "(loop repeat 3 sum (cadre-work))" 3 120)
           (c:swank-eval-sync conn (list (c:remote-symbol "cadre-profiler:stop")))
           (let ((profile (c:parse-profile-report
                           (c:swank-eval-sync conn (list (c:remote-symbol "cadre-profiler:report"))))))
             (false (c:profile-running profile))
             (true (c:find-profile-function profile "CADRE-FIB"))))
      (c:swank-disconnect conn)
      (c:kill-inferior-lisp (c:connection-process conn)))))
