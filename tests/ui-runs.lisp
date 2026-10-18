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

(define-test managed-run-controls-target-the-selected-run :parent runs
  (with-runs-fixture
    (let* ((stops nil) (reruns nil) (available t)
           (run (ui::begin-project-run
                 (uiop:temporary-directory) '(:description "REPL tests") "Tests"
                 :available (lambda () available)
                 :rerun (lambda () (incf reruns))
                 :stop (lambda () (push :tests stops)))))
      (setf reruns 0 ui::*build* (list :what "Unrelated build"))
      (false (ui::project-run-rerunnable-p run))
      (ui::stop-project-run run)
      (is equal '(:tests) stops)
      (true ui::*build*)
      (false (ui::project-run-stoppable-p run))
      (ui::finish-project-run run 0)
      (is eq :stopped (ui::pr-status run))
      (with-assistant-functions ((ui::save-project-then (lambda (root then) (declare (ignore root)) (funcall then))))
        (ui::rerun-project-run run))
      (is = 1 reruns)
      (setf available nil)
      (false (ui::project-run-rerunnable-p run)))))

(define-test connection-output-is-chunked-and-isolated :parent runs
  (with-runs-fixture
    (let* ((connection (list :connection))
           (run (ui::begin-project-run #P"/tmp/" nil "App" :connection connection))
           (other (ui::begin-project-run #P"/tmp/" nil "Other" :connection (list :other))))
      (ui::capture-project-run-output connection "hel")
      (ui::capture-project-run-output connection "lo")
      (is equal "hello" (ui::project-run-output-text run))
      (is equal "" (ui::project-run-output-text other))
      (ui::disconnect-project-runs connection "Connection lost")
      (is eq :failed (ui::pr-status run))
      (is eq :running (ui::pr-status other))
      (ui::capture-project-run-output connection "late output")
      (false (search "late output" (ui::project-run-output-text run)))
      (true (search "Connection lost" (ui::project-run-output-text run))))))

(define-test gtk-app-runs-do-not-take-repl-output :parent runs
  ;; A GTK app's output comes from its Lisp's stream; what the user
  ;; evaluates in the REPL meanwhile isn't the app's.
  (with-runs-fixture
    (let* ((connection (list :connection))
           (app (ui::begin-project-run #P"/tmp/" nil "GTK app" :connection connection :capture-output nil))
           (tests (ui::begin-project-run #P"/tmp/" nil "REPL tests" :connection connection)))
      (ui::capture-project-run-output connection "(+ 1 2)")
      (is equal "" (ui::project-run-output-text app))
      (is equal "(+ 1 2)" (ui::project-run-output-text tests))
      ;; It still ends when the connection goes.
      (ui::disconnect-project-runs connection "Connection lost")
      (is eq :failed (ui::pr-status app)))))

(define-test run-locations-keep-each-place :parent runs
  (with-runs-fixture
    (let* ((root (uiop:ensure-directory-pathname
                  (merge-pathnames (format nil "cadre-runs-~a/" (c:make-uuid)) (uiop:temporary-directory))))
           (run (ui::begin-project-run root nil "Build")))
      (ensure-directories-exist root)
      (with-open-file (out (merge-pathnames "a.lisp" root) :direction :output) (write-line "x" out))
      (unwind-protect
           (progn
             (dolist (line '("a.lisp:3: first" "a.lisp:9:2: second in the same file" "a.lisp:3: first again"
                             "missing.lisp:1: no such file" "a.lisp:12: a warning"))
               (ui::append-project-run-output run line))
             (is equal '(("a.lisp" 3 1) ("a.lisp" 9 2) ("a.lisp" 12 1))
                 (mapcar (lambda (entry) (destructuring-bind (file row column) (second entry)
                                           (list (file-namestring file) row column)))
                         (ui::run-output-locations run "")))
             (is = 1 (length (ui::run-output-locations run "warning")))
             (is = 2 (length (ui::run-output-locations run "" :limit 2))))
        (uiop:delete-directory-tree root :validate t)))))

(define-test active-runs-are-not-evicted :parent runs
  (with-runs-fixture
    (let* ((ui::*project-run-limit* 1)
           (connection (list :connection))
           (active (ui::begin-project-run #P"/tmp/" nil "App" :connection connection)))
      (dotimes (n 3)
        (ui::finish-project-run (ui::begin-project-run #P"/tmp/" nil (princ-to-string n)) 0))
      (true (member active ui::*project-runs*))
      (ui::capture-project-run-output connection "still captured")
      (true (search "still captured" (ui::project-run-output-text active))))))

(define-test explorer-summary-only-counts-selected-tests :parent runs
  (with-runs-fixture
    (let* ((passed (c::make-source-test :name "current" :line 2 :file #P"/tmp/tests.lisp" :status :passed))
           (old (c::make-source-test :name "old failure" :status :failed))
           (run (ui::begin-project-run #P"/tmp/" nil "Selected tests"))
           (page (ui::make-tests-page :run run :running t :tests (list passed old) :selected-tests (list passed))))
      (with-assistant-functions
          ((ui::fill-tests-list (lambda (page) (declare (ignore page))))
           (ui::message (lambda (&rest args) (declare (ignore args)))))
        (ui::finish-test-run page))
      (is eq :passed (ui::pr-status run))
      (true (search "/tmp/tests.lisp:3:" (ui::project-run-output-text run)))
      (false (search "old failure" (ui::project-run-output-text run))))))

(define-test stopped-explorer-callbacks-cannot-touch-a-new-run :parent runs
  (with-runs-fixture
    (let* ((old (ui::begin-project-run #P"/tmp/" nil "Old"))
           (page (ui::make-tests-page :run old :running t))
           (called nil)
           (callback (ui::test-run-callback page (value) (setf called value))))
      (with-assistant-functions
          ((ui::fill-tests-list (lambda (page) (declare (ignore page))))
           (ui::message (lambda (&rest args) (declare (ignore args)))))
        (ui::stop-explorer-test-run page old))
      (funcall callback :after-stop)
      (false called)
      (setf (ui::tsp-run page) (ui::begin-project-run #P"/tmp/" nil "New")
            (ui::tsp-running page) t)
      (funcall callback :after-new-run)
      (false called)
      (is eq :stopped (ui::pr-status old)))))

(define-test nested-test-selection-includes-descendants :parent runs
  (let* ((parent (c::make-source-test :name "parent" :key "PARENT" :package "TESTS" :file #P"/tmp/tests.lisp"))
         (child (c::make-source-test :name "child" :key "CHILD" :parent "PARENT" :package "TESTS" :file #P"/tmp/tests.lisp"))
         (other (c::make-source-test :name "other" :key "OTHER" :file #P"/tmp/other.lisp"))
         (all (list parent child other)))
    (true (ui::selected-test-p child (list parent) all))
    (false (ui::selected-test-p other (list parent) all))))

(define-test late-language-app-completion-is-ignored :parent runs
  (let* ((ui::*language-app* (list :title "Old"))
         (called nil)
         (callback (ui::language-app-callback () (setf called t))))
    (setf ui::*language-app* (list :title "New"))
    (funcall callback)
    (false called)))

(define-test gtk-warning-timestamps-are-not-file-locations :parent runs
  (false (ui::project-run-location
          "(<unknown>:30505): Gdk-WARNING **: 12:45:10.200: warning" #P"/tmp/")))

(define-test stopped-test-run-does-not-start-after-connection-arrives :parent runs
  (with-runs-fixture
    (let* ((run (ui::begin-project-run #P"/tmp/" nil "Waiting"))
           (page (ui::make-tests-page :run run :running t))
           (pending nil) (called nil))
      (with-assistant-functions
          ((ui::call-with-language-connection
            (lambda (language then) (declare (ignore language)) (setf pending then))))
        (ui::with-test-run-connection (connection page)
          (declare (ignore connection))
          (setf called t)))
      (setf (ui::tsp-running page) nil)
      (funcall pending :connection)
      (false called))))

(define-test explorer-start-and-failure-use-the-same-run-lifecycle :parent runs
  (with-runs-fixture
    (let* ((test (c::make-source-test :name "selected" :key "SELECTED" :framework :parachute
                                     :line 0 :file #P"/tmp/tests.lisp"))
           (ui::*tests-page* (ui::make-tests-page :root #P"/tmp/" :tests (list test))))
      (with-assistant-functions
          ((ui::save-project-then (lambda (root then) (declare (ignore root)) (funcall then)))
           (ui::show-tests-page (lambda ()))
           (ui::fill-tests-list (lambda (page) (declare (ignore page))))
           (ui::message (lambda (&rest args) (declare (ignore args))))
           (ui::run-lisp-tests (lambda (&rest args) (declare (ignore args)) (error "Failed to start tests"))))
        (ui::run-tests-of (list test)))
      (false (ui::tsp-running ui::*tests-page*))
      (is eq :failed (ui::pr-status (ui::tsp-run ui::*tests-page*)))
      (true (search "Failed to start tests" (ui::project-run-output-text (ui::tsp-run ui::*tests-page*)))))))

(define-test showing-tests-cannot-replace-an-original-project-selection :parent runs
  (with-runs-fixture
    (let* ((test (c::make-source-test :name "original" :key "ORIGINAL" :framework :parachute
                                     :line 0 :file #P"/tmp/tests.lisp"))
           (ui::*tests-page* (ui::make-tests-page :root #P"/tmp/" :tests (list test))))
      (with-assistant-functions
          ((ui::save-project-then (lambda (root then) (declare (ignore root)) (funcall then)))
           (ui::show-tests-page
            (lambda ()
              (unless (ui::tsp-running ui::*tests-page*) (setf (ui::tsp-tests ui::*tests-page*) nil))))
           (ui::fill-tests-list (lambda (page) (declare (ignore page))))
           (ui::message (lambda (&rest args) (declare (ignore args))))
           (ui::run-lisp-tests
            (lambda (page root tests then)
              (declare (ignore page root tests))
              (setf (c:source-test-status test) :passed)
              (funcall then))))
        (ui::run-tests-of (list test)))
      (is equal (list test) (ui::tsp-selected-tests ui::*tests-page*))
      (true (search "original" (ui::project-run-output-text (ui::tsp-run ui::*tests-page*)))))))

(define-test stopping-clojure-project-tests-cancels-file-loading :parent runs
  (with-runs-fixture
    (let ((ui::*clojure-project-test-run* nil) (interrupted nil) (continued nil))
      (with-assistant-functions
          ((ui::save-project-then (lambda (root then) (declare (ignore root)) (funcall then)))
           (ui::call-with-language-connection
            (lambda (language then) (declare (ignore language)) (funcall then :connection)))
           (ui::nrepl-op-p (lambda (&rest args) (declare (ignore args)) t))
           (ui::clojure-project-namespaces (lambda (root) (declare (ignore root)) nil))
           (ui::message (lambda (&rest args) (declare (ignore args))))
           (ui::backend-interrupt (lambda (connection) (declare (ignore connection)) (setf interrupted t)))
           (ui::nrepl-run-tests (lambda (&rest args) (declare (ignore args)) (setf continued t)))
           (ui::load-clojure-files
            (lambda (connection files then &key continue-p)
              (declare (ignore connection files))
              (true (funcall continue-p))
              (ui::stop-project-run ui::*clojure-project-test-run*)
              (false (funcall continue-p))
              (funcall then nil))))
        (ui::run-clojure-project-tests #P"/tmp/"))
      (true interrupted)
      (false continued)
      (is eq :stopped (ui::pr-status ui::*clojure-project-test-run*)))))

;;; Runs kept across sessions, and the project filter.

(defmacro with-saved-runs-fixture (&body body)
  `(let ((state (merge-pathnames (format nil "cadre-runs-state-~a/" (c:make-uuid)) (uiop:temporary-directory)))
         (ui::*project-runs-loaded* t))
     (ensure-directories-exist state)
     (unwind-protect
          (with-assistant-functions ((ui::state-directory (lambda () state)))
            (with-runs-fixture ,@body))
       (uiop:delete-directory-tree state :validate t :if-does-not-exist :ignore))))

(define-test runs-are-saved-for-later-sessions :parent runs
  (with-saved-runs-fixture
    (let* ((ui::*project-run-saved-output-limit* 10)
           (build (ui::begin-project-run #P"/tmp/alpha/" '(:program "make" :arguments ("build") :description "make build")
                                         "Build"))
           (tests (ui::begin-project-run #P"/tmp/beta/" '(:description "asdf:test-system beta (REPL)") "Tests"
                                         :rerun (lambda ()) :stop (lambda ())))
           (running (ui::begin-project-run #P"/tmp/alpha/" '(:program "sleep") "Still running")))
      (declare (ignore running))
      (ui::append-project-run-output build "0123456789abcdef")
      (ui::finish-project-run build 2)
      (ui::append-project-run-output tests "all passed")
      (ui::finish-project-run tests 0)
      (let ((restored (ui::read-project-runs)))
        ;; Finished runs only, newest first.
        (is equal '("Tests" "Build") (mapcar #'ui::pr-title restored))
        (destructuring-bind (tests-again build-again) restored
          (is eq :failed (ui::pr-status build-again))
          (is eql 2 (ui::pr-exit-code build-again))
          (is equal '(:program "make" :arguments ("build") :description "make build") (ui::pr-plan build-again))
          (is equal #P"/tmp/alpha/" (ui::pr-root build-again))
          (true (ui::pr-restored build-again))
          ;; The end of a long output, marked as cut.
          (is equal (format nil "[Earlier output wasn't kept.]~%789abcdef~%") (ui::project-run-output-text build-again))
          (is eq :passed (ui::pr-status tests-again))
          ;; A restored process can run again; a REPL run, without its rerun function, can't.
          (true (ui::project-run-rerunnable-p build-again))
          (false (ui::project-run-rerunnable-p tests-again))
          (false (ui::project-run-stoppable-p build-again)))))))

(define-test runs-history-is-read-before-it-is-written :parent runs
  (with-saved-runs-fixture
    (let ((ui::*project-run-limit* 3))
      (ui::finish-project-run (ui::begin-project-run #P"/tmp/a/" '(:program "true") "Earlier one") 0)
      (ui::finish-project-run (ui::begin-project-run #P"/tmp/a/" '(:program "true") "Earlier two") 0)
      ;; A new session: nothing is saved until the earlier runs are read.
      (let ((ui::*project-runs* nil) (ui::*project-runs-loaded* nil))
        (ui::finish-project-run (ui::begin-project-run #P"/tmp/b/" '(:program "true") "Before loading") 0)
        (is equal '("Earlier two" "Earlier one") (mapcar #'ui::pr-title (ui::read-project-runs)))
        (ui::load-project-runs)
        (is equal '("Before loading" "Earlier two" "Earlier one") (mapcar #'ui::pr-title ui::*project-runs*))
        (ui::load-project-runs)
        (is = 3 (length ui::*project-runs*))
        (ui::finish-project-run (ui::begin-project-run #P"/tmp/b/" '(:program "true") "After loading") 0)
        ;; The limit counts every kept run.
        (is equal '("After loading" "Before loading" "Earlier two")
            (mapcar #'ui::pr-title (ui::read-project-runs)))))))

(define-test bad-runs-history-is-skipped :parent runs
  (with-saved-runs-fixture
    (with-open-file (out (ui::project-runs-file) :direction :output :if-exists :supersede)
      (prin1 '(:version 1 :runs ((:title "Good" :root "/tmp/a/" :plan (:program "true") :started 3900000000
                                  :seconds 1.5 :exit-code 0 :status :passed :output "ok")
                                 (:title "No status" :root "/tmp/a/" :plan nil :started 3900000000
                                  :seconds 1 :exit-code 0 :status :running :output "")
                                 (:title 7 :root "/tmp/a/")))
             out))
    (is equal '("Good") (mapcar #'ui::pr-title (ui::read-project-runs)))
    (is = 1.5 (ui::project-run-seconds (first (ui::read-project-runs))))
    ;; A file that would evaluate code when read is ignored whole.
    (with-open-file (out (ui::project-runs-file) :direction :output :if-exists :supersede)
      (write-string "(:version 1 :runs #.(error \"evaluated\"))" out))
    (false (ui::read-project-runs))))

(define-test runs-filter-by-project :parent runs
  (with-runs-fixture
    (let ((a1 (ui::begin-project-run #P"/tmp/one/app/" nil "A1"))
          (b (ui::begin-project-run #P"/tmp/two/lib/" nil "B"))
          (a2 (ui::begin-project-run #P"/tmp/one/app" nil "A2"))
          (c (ui::begin-project-run #P"/tmp/three/app/" nil "C")))
      (is equal (list c a2 b a1) (ui::visible-project-runs nil))
      (is equal (list a2 a1) (ui::visible-project-runs #P"/tmp/one/app/"))
      (is equal '(#P"/tmp/three/app/" #P"/tmp/one/app/" #P"/tmp/two/lib/") (ui::project-run-roots))
      ;; Folders with the same name are told apart by their paths.
      (let ((roots (ui::project-run-roots)))
        (is equal "/tmp/three/app/" (ui::project-root-label #P"/tmp/three/app/" roots))
        (is equal "lib" (ui::project-root-label #P"/tmp/two/lib/" roots))))))

(define-test clearing-runs :parent runs
  (with-saved-runs-fixture
    (let ((a-old (ui::begin-project-run #P"/tmp/a/" '(:program "true") "A old"))
          (b (ui::begin-project-run #P"/tmp/b/" '(:program "true") "B"))
          (a-new (ui::begin-project-run #P"/tmp/a/" '(:program "true") "A new"))
          (a-running (ui::begin-project-run #P"/tmp/a/" '(:program "sleep") "A running")))
      (declare (ignore a-running))
      (dolist (run (list a-old b a-new)) (ui::finish-project-run run 0))
      (is equal '("A new" "A old") (mapcar #'ui::pr-title (ui::clearable-project-runs #P"/tmp/a/")))
      ;; One project's finished runs go, here and in the saved history;
      ;; a run still going stays.
      (is = 2 (ui::clear-project-runs #P"/tmp/a/"))
      (is equal '("A running" "B") (mapcar #'ui::pr-title ui::*project-runs*))
      (is equal '("B") (mapcar #'ui::pr-title (ui::read-project-runs)))
      (is = 0 (ui::clear-project-runs #P"/tmp/a/"))
      ;; All projects.
      (is = 1 (ui::clear-project-runs))
      (is equal '("A running") (mapcar #'ui::pr-title ui::*project-runs*))
      (false (ui::read-project-runs)))))
