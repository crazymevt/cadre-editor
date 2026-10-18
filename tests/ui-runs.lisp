;;;; Run history is independent of the window and the currently open project.
(in-package #:cadre-ui-tests)

(define-test runs)

(defmacro with-runs-fixture (&body body)
  `(let ((ui::*project-runs* nil) (ui::*runs-page* nil) (ui::*build* nil))
     ,@body))

(define-test run-history-and-output :parent runs
  (with-runs-fixture
    (let* ((ui::*project-run-limit* 2)
           (first (ui::begin-project-run #P"/tmp/a/" '(:program "make" :arguments ("test")) "Tests"))
           (second (ui::begin-project-run #P"/tmp/b/" '(:program "make" :arguments ("build")) "Build")))
      (ui::append-project-run-output first "first output")
      (ui::append-project-run-output second "second output")
      (is equal (format nil "first output~%") (ui::project-run-output-text first))
      (is equal (format nil "second output~%") (ui::project-run-output-text second))
      (ui::finish-project-run first 2)
      (is eq :failed (ui::pr-status first))
      (is = 2 (ui::pr-exit-code first))
      (let ((end (ui::pr-end first)))
        (ui::finish-project-run first 0)
        (is = end (ui::pr-end first))
        (is eq :failed (ui::pr-status first)))
      (ui::finish-project-run second 0 :stopped t)
      (is eq :stopped (ui::pr-status second))
      (let ((third (ui::begin-project-run #P"/tmp/c/" nil "Third")))
        (is equal (list third second) ui::*project-runs*)
        (is equal (format nil "first output~%") (ui::project-run-output-text first))))))

(define-test bounded-and-filtered-run-output :parent runs
  (with-runs-fixture
    (let* ((ui::*project-run-output-limit* 12)
           (run (ui::begin-project-run #P"/tmp/" nil "Test")))
      (ui::append-project-run-output run "FAIL one")
      (ui::append-project-run-output run "next long line")
      (true (ui::pr-truncated run))
      (is = 12 (length (ui::pr-output run)))
      (ui::append-project-run-output run "more ignored output")
      (is = 12 (length (ui::pr-output run)))
      (let ((filtered (ui::project-run-output-text run "fail")))
        (true (search "FAIL one" filtered))
        (false (search "nex" filtered))
        (true (search "limit reached" filtered))))))

(define-test run-diagnostic-locations :parent runs
  (is equal '(#P"/tmp/project/src/main.lisp" 12 3)
      (ui::project-run-location "src/main.lisp:12:3: error" #P"/tmp/project/"))
  (is equal '(#P"/tmp/a file.lisp" 2 1)
      (ui::project-run-location "/tmp/a file.lisp:2: failed" #P"/other/"))
  (false (ui::project-run-location "Tests passed" #P"/tmp/"))
  (false (ui::project-run-location "src/main.lisp:0: error" #P"/tmp/")))

(define-test rerun-keeps-original-project-and-plan :parent runs
  (with-runs-fixture
    (let* ((root (uiop:temporary-directory))
           (plan (list :program "make" :arguments (list "test") :description "make test"))
           (run (ui::begin-project-run root plan "Tests"))
           (saved nil) (launched nil) (continuation nil))
      (setf (first (getf plan :arguments)) "build")
      (ui::finish-project-run run 0)
      (with-assistant-functions
          ((ui::save-project-then (lambda (directory then)
                                   (setf saved directory continuation then)))
           (ui::run-project-process (lambda (directory command &key what)
                                      (setf launched (list directory command what)))))
        (ui::rerun-project-run run)
        (is equal root saved)
        (false launched)
        (funcall continuation)
        (is equal root (first launched))
        (is equal '("test") (getf (second launched) :arguments))
        (is equal "Tests" (third launched))))))

(define-test stopped-process-does-not-report-success :parent runs
  (with-runs-fixture
    (let* ((run (ui::begin-project-run #P"/tmp/" nil "Stopped test"))
           (success nil) (ended nil)
           (ui::*window* nil)
           (ui::*build* (list :run run :what "Stopped test" :stopped t
                              :start (get-internal-real-time)
                              :on-end (lambda (code) (setf ended (list code)))
                              :on-success (lambda (seconds) (declare (ignore seconds)) (setf success t)))))
      (with-assistant-functions
          ((ui::window-panel (lambda (window) (declare (ignore window)) nil))
           (ui::panel-log (lambda (&rest args) (declare (ignore args))))
           (ui::message (lambda (&rest args) (declare (ignore args)))))
        (ui::build-finished 0))
      (false success)
      (is equal '(0) ended)
      (is eq :stopped (ui::pr-status run))
      (false ui::*build*))))
