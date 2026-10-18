;;;; test-explorer.lisp — the Tests page: a project's tests, run one at a
;;;; time or all, each passed or failed
;;;;
;;;; The page lists the tests in the project's files, found in the source
;;;; (nothing need be loaded), by file: Common Lisp's parachute and FiveAM
;;;; tests, Clojure's deftests (run through cider-nrepl) and Racket's
;;;; rackunit test-cases (run with raco test, a file at a time). Run All
;;;; runs them, Run Failed the ones that failed, ▶ one (with its children);
;;;; each first saves the project's files and loads its systems again, so a
;;;; rerun tests the code as it is now. Results come from the Lisp without
;;;; the debugger: a failed test shows its failed checks, and clicking a
;;;; test goes to it in the source.

(in-package #:cadre-ui)

(defstruct (tests-page (:conc-name tsp-))
  widget list title root
  (tests '())                           ; source-tests, with FILE set
  (running nil) run selected-tests connection process)

(defvar *tests-page* nil)
(defvar *test-rows* (make-hash-table :test 'eq) "Test row → source-test.")
(defvar *test-runner-connection* nil "The connection the runner helper was sent to.")

(defun make-tests-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (title (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t :css-classes '("heading")
                                          :label "Tests"))
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :spacing 4 :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4
                       title
                       (gtk:button :label "Run All" :css-classes '("flat")
                                   :tooltip-text "Save, load the project's systems, and run every test"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'run-all-tests)))
                       (gtk:button :label "Run Failed" :css-classes '("flat")
                                   :tooltip-text "Run again the tests that failed"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'run-failed-tests)))
                       (gtk:button :icon-name "view-refresh-symbolic" :css-classes '("flat")
                                   :tooltip-text "Look for tests in the project's files again"
                                   :on-clicked (lambda (b) (declare (ignore b)) (scan-project-tests *tests-page*))))
                     (gtk:separator)
                     (gtk:scrolled-window :vexpand t :child list)))))
    (setf *tests-page* (make-tests-page :widget widget :list list :title title))
    (gtk:list-box-set-activate-on-single-click list t)
    (gobject:connect list :row-activated
                     (lambda (lb row)
                       (declare (ignore lb))
                       (let ((test (gethash row *test-rows*)))
                         (when test (go-to-test test)))))
    (fill-tests-list *tests-page*)
    widget))

;;; Finding the tests

(defun scan-project-tests (page &optional (root (current-project-root)))
  "Read the tests in ROOT's Lisp files, keeping the results of those seen before."
  (when (tsp-running page) (return-from scan-project-tests))
  (let ((old (tsp-tests page))
        (tests '()))
    (when root
      (dolist (path (project-files root :hidden-names *explorer-hidden-names* :hidden-types *explorer-hidden-types*))
        (let ((mode (major-mode-for-file path)))
          (when (member mode '(lisp-mode clojure-mode racket-mode))
          (let* ((buffer (find-file-buffer path))
                 (text (if buffer (buffer-string buffer) (read-text-file path))))
            (when (and text (case mode
                              (lisp-mode (or (search "define-test" text :test #'char-equal) (fiveam-source-p text)))
                              (clojure-mode (search "deftest" text))
                              (racket-mode (or (search "test-case" text) (search "rackunit" text)))))
              (dolist (test (ignore-errors (mode-source-tests text mode)))
                (setf (source-test-file test) path)
                (let ((before (find-if (lambda (o) (and (equal (source-test-key o) (source-test-key test))
                                                        (equal (source-test-package o) (source-test-package test))
                                                        (equal (source-test-file o) path)))
                                       old)))
                  (when before
                    (setf (source-test-status test) (source-test-status before)
                          (source-test-messages test) (source-test-messages before))))
                (push test tests))))))))
    (setf (tsp-root page) root
          (tsp-tests page) (nreverse tests))
    (fill-tests-list page)))

;;; Showing them

(defun test-status-mark (status)
  (case status
    (:passed (values "✓" "success"))
    (:failed (values "✗" "error"))
    (:skipped (values "–" "dim-label"))
    (:running (values "⋯" "dim-label"))
    (t (values "○" "dim-label"))))

(defun test-depth (test tests)
  (loop for depth from 0 below 8
        for parent = (source-test-parent test) then (source-test-parent found)
        for found = (and parent (find-if (lambda (o) (and (equal (source-test-key o) parent)
                                                          (equal (source-test-package o) (source-test-package test))))
                                         tests))
        while found
        finally (return depth)))

(defun test-row (test depth)
  (let* ((box (make-instance 'gtk:box :orientation :vertical :margin-start (+ 6 (* 16 depth)) :margin-end 6))
         (line (make-instance 'gtk:box :spacing 8))
         (row (make-instance 'gtk:list-box-row :child box)))
    (multiple-value-bind (mark class) (test-status-mark (source-test-status test))
      (gtk:box-append line (label mark :width-chars 2 :css-classes (list class "heading"))))
    (gtk:box-append line (label (source-test-name test) :hexpand t :ellipsize :end :css-classes '("monospace")))
    (gtk:box-append line (label (case (source-test-framework test)
                                  (:clojure "clojure.test")
                                  (t (string-downcase (symbol-name (source-test-framework test)))))
                                :css-classes '("dim-label")))
    (let ((run (make-instance 'gtk:button :icon-name "cadre-play-symbolic" :css-classes '("flat")
                                          :tooltip-text (case (source-test-framework test)
                                                          (:rackunit (format nil "Run ~a's tests (raco test runs a whole file)"
                                                                             (file-namestring (source-test-file test))))
                                                          (:parachute (format nil "Run ~a and the tests inside it" (source-test-name test)))
                                                          (t (format nil "Run ~a" (source-test-name test)))))))
      (gobject:connect run :clicked (lambda (b) (declare (ignore b)) (run-tests-of (list test))))
      (gtk:box-append line run))
    (gtk:box-append box line)
    (when (eq (source-test-status test) :failed)
      (loop for message in (source-test-messages test)
            repeat 4
            do (gtk:box-append box (label message :wrap t :wrap-mode :word-char :selectable t :margin-start 30
                                                  :margin-bottom 4 :css-classes '("monospace" "dim-label")))))
    (gtk:widget-set-tooltip-text row (format nil "~a, line ~d: click to go to it"
                                             (file-namestring (source-test-file test)) (1+ (source-test-line test))))
    (setf (gethash row *test-rows*) test)
    row))

(defun fill-tests-list (page)
  (let* ((list (tsp-list page))
         (tests (tsp-tests page))
         (passed (count :passed tests :key #'source-test-status))
         (failed (count :failed tests :key #'source-test-status)))
    (clrhash *test-rows*)
    (gtk:list-box-remove-all list)
    (gtk:label-set-text (tsp-title page)
                        (cond ((tsp-running page) "Running tests…")
                              ((null tests) "Tests")
                              ((zerop (+ passed failed)) (format nil "~d test~:p" (length tests)))
                              (t (format nil "~d test~:p: ~d passed~[~:;, ~:*~d failed~]" (length tests) passed failed))))
    (if (null tests)
        (gtk:list-box-append list (label (if (tsp-root page)
                                             "No tests in this project: parachute, FiveAM, clojure.test or rackunit"
                                             "Open a project to see its tests")
                                         :margin-start 8 :wrap t :css-classes '("dim-label")))
        (let ((file nil))
          (dolist (test tests)
            (unless (equal file (source-test-file test))
              (setf file (source-test-file test))
              (let ((header (make-instance 'gtk:list-box-row :activatable nil :selectable nil
                                                             :child (label (enough-namestring file (tsp-root page))
                                                                           :margin-start 6 :margin-top 6
                                                                           :css-classes '("caption-heading" "dim-label")))))
                (gtk:list-box-append list header)))
            (gtk:list-box-append list (test-row test (test-depth test tests))))))
    (when *window*
      (panel-set-title (window-panel *window*) "tests"
                       (if (plusp failed) (format nil "Tests (~d ✗)" failed) "Tests")))))

(defun go-to-test (test)
  (open-file-path (source-test-file test)
                  :then (lambda (view)
                          (goto-line-column view (source-test-line test) 0)
                          (focus-view view))))

;;; Running

(defun test-systems-form (root)
  "Source that loads ROOT's systems (with Quicklisp if the Lisp has it, which
fetches what they need), so the tests and the code are current."
  (let ((systems (project-systems root)))
    (asdf-form (format nil "(dolist (s (quote ~s)) (if (find-package \"QL\") (funcall (find-symbol \"QUICKLOAD\" \"QL\") s :silent t) (funcall (find-symbol \"LOAD-SYSTEM\" \"ASDF\") s)))"
                       systems)
               root)))

(defun ensure-test-runner (connection then &optional on-abort)
  (if (eq *test-runner-connection* connection)
      (funcall then)
      (rex connection (swank-call "swank:interactive-eval-region" +test-runner-helper+ 3 120)
           :package "COMMON-LISP-USER"
           :on-ok (lambda (reply)
                    (declare (ignore reply))
                    (setf *test-runner-connection* connection)
                    (funcall then))
           :on-abort on-abort)))

(defun lisp-test-p (test) (member (source-test-framework test) '(:parachute :fiveam)))

(defun no-result (tests &optional (message "No result: is the test's code loaded?"))
  "Mark TESTS that are still running as failed with MESSAGE."
  (dolist (test tests)
    (when (eq (source-test-status test) :running)
      (setf (source-test-status test) :failed
            (source-test-messages test) (list message)))))

(defmacro test-run-callback (page arguments &body body)
  "Ignore asynchronous replies from a stopped or replaced test run."
  (let ((token (gensym "RUN")) (holder (gensym "PAGE")))
    `(let* ((,holder ,page) (,token (tsp-run ,holder)))
       (lambda ,arguments
         (when (and (eq ,token (tsp-run ,holder)) (tsp-running ,holder))
           (locally ,@body))))))

(defmacro with-test-run-connection ((connection page &key (language :common-lisp)) &body body)
  (let ((token (gensym "RUN")) (holder (gensym "PAGE")))
    `(let* ((,holder ,page) (,token (tsp-run ,holder)))
       (with-connection (,connection :language ,language)
         (when (and (eq ,token (tsp-run ,holder)) (tsp-running ,holder))
           (locally ,@body))))))

(defun test-run-connection (page connection)
  (setf (tsp-connection page) connection
        (pr-connection (tsp-run page)) connection))

(defun finish-test-run (page &key stopped)
  (when (tsp-running page)
    (let* ((run (tsp-run page)) (tests (tsp-selected-tests page))
           (failed (count :failed tests :key #'source-test-status)))
      (dolist (test tests)
        (append-project-run-output
         run (format nil "~a:~d: ~a ~a~{~%  ~a~}"
                     (source-test-file test) (1+ (source-test-line test))
                     (source-test-status test) (source-test-name test) (source-test-messages test)))
        (when (eq (source-test-status test) :running) (setf (source-test-status test) nil)))
      (setf (tsp-running page) nil (tsp-connection page) nil (tsp-process page) nil)
      (finish-project-run run (if (zerop failed) 0 1) :stopped stopped)
      (fill-tests-list page)
      (message (cond (stopped "Tests stopped") ((plusp failed) "~d test~:p failed") (t "Tests passed")) failed))))

(defun stop-explorer-test-run (page run)
  (when (and (tsp-running page) (eq run (tsp-run page)))
    (let ((connection (tsp-connection page)))
      (when (tsp-process page) (sb-ext:process-kill (tsp-process page) 15))
      (finish-test-run page :stopped t)
      (when connection (backend-interrupt connection)))))

(defun source-test-run-key (test)
  (list (source-test-file test) (source-test-framework test)
        (source-test-package test) (source-test-name test)))

(defun rerun-explorer-tests (root keys)
  (when (tsp-running *tests-page*) (editor-error "Tests are already running"))
  (scan-project-tests *tests-page* root)
  (let ((tests (remove-if-not (lambda (test) (member (source-test-run-key test) keys :test #'equal))
                              (tsp-tests *tests-page*))))
    (unless tests (editor-error "The original tests are no longer present"))
    (run-tests-of tests)))

;;; Common Lisp: parachute and FiveAM, in the connected Lisp

(defun run-test-groups (page connection groups then)
  "Run GROUPS ((framework package names…) …) one after another, showing
results as they come; then call THEN."
  (if (null groups)
      (funcall then)
      (destructuring-bind ((framework package &rest names) &rest more) groups
        (rex connection (list (remote-symbol "cadre-test-runner:run") framework package (list 'quote names))
             :on-ok (test-run-callback page (reply)
                      (let* ((results (parse-test-results reply))
                             (updated (apply-test-results (tsp-tests page) results package)))
                        ;; A test that got no result (its package isn't loaded, say)
                        ;; failed, with whatever the runner said.
                        (no-result (remove-if-not (lambda (test) (and (lisp-test-p test)
                                                                      (equal (source-test-package test) package)
                                                                      (not (member test updated))))
                                                  (tsp-tests page))
                                   (or (first (remove-duplicates (loop for (nil status messages) in results
                                                                       when (eq status :failed) append messages)
                                                                 :test #'string=))
                                       "No result: is the test's system loaded?")))
                      (fill-tests-list page)
                      (run-test-groups page connection more then))
             :on-abort (test-run-callback page (reason)
                         (no-result (tsp-selected-tests page) (or reason "Tests aborted"))
                         (message "Running the tests was abandoned~@[: ~a~]" reason)
                         (funcall then))))))

(defun run-lisp-tests (page root tests then)
  (if (null tests)
      (funcall then)
      (with-test-run-connection (connection page)
        (test-run-connection page connection)
        (rex connection (swank-call "swank:interactive-eval" (test-systems-form root) 3 120)
             :package "COMMON-LISP-USER"
             :on-ok (test-run-callback page (reply)
                      (declare (ignore reply))
                      (ensure-test-runner connection
                                          (test-run-callback page ()
                                            (run-test-groups page connection (test-run-groups (test-roots tests)) then))
                                          (test-run-callback page (reason)
                                            (no-result tests (or reason "Test runner did not load"))
                                            (funcall then))))
             :on-abort (test-run-callback page (reason)
                         (message "Loading the project failed~@[: ~a~]; see the REPL and Problems" reason)
                         (no-result tests "The project didn't load: see the REPL and Problems")
                         (funcall then))))))

;;; Clojure: clojure.test, through cider-nrepl

(defun run-clojure-tests (page tests then)
  (if (null tests)
      (funcall then)
      (with-test-run-connection (connection page :language :clojure)
        (test-run-connection page connection)
        (if (not (nrepl-op-p connection "test-var-query"))
            (progn
              (no-result tests "Running tests needs cider-nrepl in the Clojure REPL")
              (funcall then))
            (let ((run (tsp-run page)))
              (load-clojure-files
               connection (remove-duplicates (mapcar #'uiop:native-namestring (mapcar #'source-test-file tests)) :test #'equal)
               (test-run-callback page (failed)
                 (dolist (test tests)
                   (when (member (uiop:native-namestring (source-test-file test)) failed :test #'equal)
                     (setf (source-test-status test) :failed
                           (source-test-messages test) (list "The file didn't load: see the Problems page"))))
                 (nrepl-test-vars
                  connection
                  (mapcar (lambda (test) (format nil "~a/~a" (source-test-package test) (source-test-name test))) tests)
                  :on-done
                  (test-run-callback page (results)
                    (loop for (ns var status messages) in results
                          for test = (find-if (lambda (test) (and (equal (source-test-package test) ns)
                                                                  (equal (source-test-name test) var))) tests)
                          when (and test (not (member (uiop:native-namestring (source-test-file test)) failed :test #'equal)))
                            do (setf (source-test-status test) status (source-test-messages test) messages))
                    (no-result tests)
                    (fill-tests-list page)
                    (funcall then))
                  :on-abort (test-run-callback page (reason)
                              (no-result tests (format nil "The tests didn't run: ~a" reason))
                              (funcall then))))
               :continue-p (lambda () (and (eq run (tsp-run page)) (tsp-running page)))))))))

;;; Racket: raco test, a file at a time

(defun run-racket-files (page files then)
  (if (null files)
      (funcall then)
      (let* ((file (first files))
             (run (tsp-run page))
             (process (handler-case
                          (sb-ext:run-program *raco-command* (list "test" (uiop:native-namestring file))
                                              :search t :wait nil :input nil :output :stream :error :output
                                              :directory (uiop:pathname-directory-pathname file)
                                              :external-format :utf-8)
                        (error (e)
                          (no-result (tsp-selected-tests page) (princ-to-string e))
                          (funcall then)
                          (return-from run-racket-files)))))
        (setf (tsp-process page) process (tsp-connection page) nil (pr-connection run) nil)
        (sb-thread:make-thread
         (lambda ()
           (let ((output (with-output-to-string (out)
                           (loop for line = (read-line (sb-ext:process-output process) nil)
                                 while line do (write-line line out)
                                   (let ((line line))
                                     (deliver-to-gui
                                      (lambda ()
                                        (when (and (eq run (tsp-run page)) (tsp-running page))
                                          (append-project-run-output run line)))))))))
             (sb-ext:process-wait process)
             (deliver-to-gui
              (test-run-callback page ()
                (when (eq run (tsp-run page))
                  (setf (tsp-process page) nil)
                  (let ((tests (remove-if-not (lambda (test) (and (eq (source-test-framework test) :rackunit)
                                                                                (equal (source-test-file test) file)))
                                              (tsp-tests page))))
                    (apply-raco-results tests (parse-raco-test-output output)
                                        (or (sb-ext:process-exit-code process) 1) output))
                  (fill-tests-list page)
                  (run-racket-files page (rest files) then))))))
         :name "raco test"))))

(defun run-racket-tests (page tests then)
  (run-racket-files page (remove-duplicates (mapcar #'source-test-file tests) :test #'equal) then))

(defun selected-test-p (test selected all)
  (or (member test selected)
      (and (eq (source-test-framework test) :rackunit)
           (some (lambda (other) (equal (source-test-file other) (source-test-file test))) selected))
      (loop with seen = nil
            for current = test then parent
            for parent = (find-if (lambda (other)
                                    (and (equal (source-test-parent current) (source-test-key other))
                                         (equal (source-test-package current) (source-test-package other))
                                         (equal (source-test-file current) (source-test-file other)))) all)
            while (and parent (not (member parent seen)))
            thereis (member parent selected)
            do (push parent seen))))

(defun run-tests-of (tests)
  "Save, then run the selection, recording results in Tests and Runs."
  (let* ((page *tests-page*) (root (or (tsp-root page) (project-root-or-error))))
    (when (tsp-running page) (editor-error "Tests are running already"))
    (unless tests (return-from run-tests-of (message "No tests to run")))
    (save-project-then
     root
     (lambda ()
       (when (tsp-running page) (editor-error "Tests are running already"))
       (let* ((selected (remove-if-not (lambda (test) (selected-test-p test tests (tsp-tests page)))
                                      (tsp-tests page)))
              (keys (mapcar #'source-test-run-key tests))
              (run (begin-project-run root '(:description "Test Explorer selection") "Tests"
                                      :available (lambda () (not (tsp-running page)))
                                      :rerun (lambda () (rerun-explorer-tests root keys)))))
         (setf (tsp-run page) run (tsp-selected-tests page) selected (tsp-running page) t
               (pr-stop run) (lambda () (stop-explorer-test-run page run)))
         ;; Showing Tests can rescan for the current editor project. Lock this
         ;; selection first so a run from another project keeps its own tests.
         (show-tests-page)
         (dolist (test selected) (setf (source-test-status test) :running (source-test-messages test) nil))
         (fill-tests-list page)
         (flet ((of (framework-p) (remove-if-not framework-p tests)))
           (handler-case
               (run-lisp-tests
                page root (of #'lisp-test-p)
                (test-run-callback page ()
                  (run-clojure-tests
                   page (of (lambda (test) (eq (source-test-framework test) :clojure)))
                   (test-run-callback page ()
                     (run-racket-tests page (of (lambda (test) (eq (source-test-framework test) :rackunit)))
                                       (test-run-callback page () (finish-test-run page)))))))
             (error (e)
               (no-result selected (princ-to-string e))
               (finish-test-run page)
               (message "~a" e)))))))))

(defun show-tests-page ()
  (panel-show (window-panel *window*) "tests")
  (set-panel-visible *window* t))

(defun tests-page-shown ()
  "The Tests tab was chosen: look for tests if the project changed."
  (let ((root (current-project-root)))
    (unless (and (tsp-root *tests-page*) root (equal (namestring root) (namestring (tsp-root *tests-page*))))
      (scan-project-tests *tests-page* root))))

(defun tests-disconnected (&optional (connection *connection*))
  (setf *test-runner-connection* nil)
  (when (and *tests-page* (tsp-running *tests-page*)
             (eq connection (tsp-connection *tests-page*)))
    (no-result (tsp-selected-tests *tests-page*) "Test connection disconnected")
    (finish-test-run *tests-page*)))

(defun test-project (root otherwise)
  "Run ROOT's tests on the Tests page; if it has none Cadre finds,
call OTHERWISE (which runs them some other way)."
  (let ((page *tests-page*))
    (scan-project-tests page root)
    (if (tsp-tests page)
        (run-tests-of (tsp-tests page))
        (funcall otherwise))))

(defun add-tests-tab (root)
  "Put the Tests tab in the panel if ROOT has tests, without showing it."
  (when (and *window* root)
    (let ((page *tests-page*))
      (unless (and (tsp-root page) (equal (namestring (tsp-root page)) (namestring root)))
        (scan-project-tests page root))
      (when (tsp-tests page)
        (panel-add-tab (window-panel *window*) "tests")))))

;;; Commands

(define-command show-test-explorer ()
  "Show the Tests page: the project's tests, to run all, the failed ones, or one."
  (scan-project-tests *tests-page*)
  (show-tests-page))

(define-command run-all-tests ()
  "Run every test of the project, showing each one's result on the Tests page."
  (scan-project-tests *tests-page* (or (tsp-root *tests-page*) (current-project-root)))
  (run-tests-of (tsp-tests *tests-page*)))

(define-command run-failed-tests ()
  "Run again the tests that failed last time."
  (let ((failed (remove :failed (tsp-tests *tests-page*) :key #'source-test-status :test-not #'eq)))
    (if failed
        (run-tests-of failed)
        (message "No failed tests"))))

(define-command run-test-at-cursor ()
  "Run the test the cursor is in (and the tests inside it)."
  (:modes lisp-mode clojure-mode racket-mode)
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (line (cursor-line-column view))
         (tests (mode-source-tests (buffer-string buffer) (buffer-major-mode buffer)))
         (test (find-if (lambda (test) (<= (source-test-line test) line)) tests :from-end t)))
    (unless test (editor-error "No test at or above the cursor"))
    (unless (buffer-file buffer) (editor-error "Save the file first"))
    (scan-project-tests *tests-page* (or (tsp-root *tests-page*) (current-project-root)))
    (let ((listed (find-if (lambda (o) (and (equal (source-test-key o) (source-test-key test))
                                            (equal (source-test-package o) (source-test-package test))
                                            (same-file-p (source-test-file o) (buffer-file buffer))))
                           (tsp-tests *tests-page*))))
      (run-tests-of (list (or listed (progn (setf (source-test-file test) (buffer-file buffer)) test)))))))
