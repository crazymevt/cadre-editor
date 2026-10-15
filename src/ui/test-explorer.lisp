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
  (running nil))

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

(defun ensure-test-runner (connection then)
  (if (eq *test-runner-connection* connection)
      (funcall then)
      (rex connection (swank-call "swank:interactive-eval-region" +test-runner-helper+ 3 120)
           :package "COMMON-LISP-USER"
           :on-ok (lambda (reply)
                    (declare (ignore reply))
                    (setf *test-runner-connection* connection)
                    (funcall then)))))

(defun lisp-test-p (test) (member (source-test-framework test) '(:parachute :fiveam)))

(defun no-result (tests &optional (message "No result: is the test's code loaded?"))
  "Mark TESTS that are still running as failed with MESSAGE."
  (dolist (test tests)
    (when (eq (source-test-status test) :running)
      (setf (source-test-status test) :failed
            (source-test-messages test) (list message)))))

(defun finish-test-run (page)
  (setf (tsp-running page) nil)
  (dolist (test (tsp-tests page))
    (when (eq (source-test-status test) :running) (setf (source-test-status test) nil)))
  (fill-tests-list page)
  (let ((failed (count :failed (tsp-tests page) :key #'source-test-status)))
    (message (if (plusp failed) "~d test~:p failed" "Tests passed") failed)))

;;; Common Lisp: parachute and FiveAM, in the connected Lisp

(defun run-test-groups (page connection groups then)
  "Run GROUPS ((framework package names…) …) one after another, showing
results as they come; then call THEN."
  (if (null groups)
      (funcall then)
      (destructuring-bind ((framework package &rest names) &rest more) groups
        (rex connection (list (remote-symbol "cadre-test-runner:run") framework package (list 'quote names))
             :on-ok (lambda (reply)
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
             :on-abort (lambda (reason)
                         (message "Running the tests was abandoned~@[: ~a~]" reason)
                         (funcall then))))))

(defun run-lisp-tests (page root tests then)
  (if (null tests)
      (funcall then)
      (with-connection (connection)
        (rex connection (swank-call "swank:interactive-eval" (test-systems-form root) 3 120)
             :package "COMMON-LISP-USER"
             :on-ok (lambda (reply)
                      (declare (ignore reply))
                      (ensure-test-runner connection
                                          (lambda ()
                                            (run-test-groups page connection (test-run-groups (test-roots tests)) then))))
             :on-abort (lambda (reason)
                         (message "Loading the project failed~@[: ~a~]; see the REPL and Problems" reason)
                         (no-result tests "The project didn't load: see the REPL and Problems")
                         (funcall then))))))

;;; Clojure: clojure.test, through cider-nrepl

(defun run-clojure-tests (page tests then)
  (if (null tests)
      (funcall then)
      (with-connection (connection :language :clojure)
        (unless (nrepl-op-p connection "test-var-query")
          (no-result tests "Running tests needs cider-nrepl in the Clojure REPL")
          (funcall then)
          (return-from run-clojure-tests))
        ;; LOAD-CLOJURE-FILES (projects.lisp) puts load errors on the Problems page.
        (load-clojure-files
         connection (remove-duplicates (mapcar #'uiop:native-namestring (mapcar #'source-test-file tests)) :test #'equal)
         (lambda (failed)
           (dolist (test tests)
             (when (member (uiop:native-namestring (source-test-file test)) failed :test #'equal)
               (setf (source-test-status test) :failed
                     (source-test-messages test) (list "The file didn't load: see the Problems page"))))
           (nrepl-test-vars connection
                            (mapcar (lambda (test) (format nil "~a/~a" (source-test-package test) (source-test-name test)))
                                    tests)
                            :on-done (lambda (results)
                                       (loop for (ns var status messages) in results
                                             for test = (find-if (lambda (test) (and (equal (source-test-package test) ns)
                                                                                     (equal (source-test-name test) var)))
                                                                 tests)
                                             when test
                                               do (setf (source-test-status test) status
                                                        (source-test-messages test) messages))
                                       (no-result tests)
                                       (fill-tests-list page)
                                       (funcall then))
                            :on-abort (lambda (reason)
                                        (no-result tests (format nil "The tests didn't run: ~a" reason))
                                        (funcall then))))))))

;;; Racket: raco test, a file at a time

(defun run-racket-files (page files then)
  (if (null files)
      (funcall then)
      (let ((file (first files)))
        (sb-thread:make-thread
         (lambda ()
           (multiple-value-bind (output error-output code)
               (ignore-errors
                (uiop:run-program (list *raco-command* "test" (uiop:native-namestring file))
                                  :output :string :error-output :output :ignore-error-status t
                                  :directory (uiop:pathname-directory-pathname file)))
             (declare (ignore error-output))
             (deliver-to-gui
              (lambda ()
                (let ((tests (remove-if-not (lambda (test) (and (eq (source-test-framework test) :rackunit)
                                                                (equal (source-test-file test) file)))
                                            (tsp-tests page))))
                  (apply-raco-results tests (parse-raco-test-output (or output "")) (or code 1)
                                      (or output (format nil "~a didn't run" *raco-command*))))
                (fill-tests-list page)
                (run-racket-files page (rest files) then)))))
         :name "raco test"))))

(defun run-racket-tests (page tests then)
  (run-racket-files page (remove-duplicates (mapcar #'source-test-file tests) :test #'equal) then))

(defun run-tests-of (tests)
  "Save, then run TESTS (their children too): Common Lisp ones in the Lisp
\(loading the project first), Clojure ones through nREPL, Racket ones with
raco test, a file at a time."
  (let ((page *tests-page*))
    (cond ((tsp-running page) (message "Tests are running already"))
          ((null tests) (message "No tests to run"))
          (t
           (let ((root (or (tsp-root page) (project-root-or-error))))
             (show-tests-page)
             (setf (tsp-running page) t)
             (dolist (test tests) (setf (source-test-status test) :running))
             ;; A Racket file runs all its tests.
             (dolist (test (tsp-tests page))
               (when (and (eq (source-test-framework test) :rackunit)
                          (find-if (lambda (o) (and (eq (source-test-framework o) :rackunit)
                                                    (equal (source-test-file o) (source-test-file test))))
                                   tests))
                 (setf (source-test-status test) :running)))
             (fill-tests-list page)
             (flet ((of (framework-p) (remove-if-not framework-p tests)))
               (save-project-then
                root
                (lambda ()
                  (handler-case
                      (run-lisp-tests
                       page root (of #'lisp-test-p)
                       (lambda ()
                         (run-clojure-tests
                          page (of (lambda (test) (eq (source-test-framework test) :clojure)))
                          (lambda ()
                            (run-racket-tests
                             page (of (lambda (test) (eq (source-test-framework test) :rackunit)))
                             (lambda () (finish-test-run page)))))))
                    (error (e)
                      (finish-test-run page)
                      (message "~a" e)))))))))))

(defun show-tests-page ()
  (panel-show (window-panel *window*) "tests")
  (set-panel-visible *window* t))

(defun tests-page-shown ()
  "The Tests tab was chosen: look for tests if the project changed."
  (let ((root (current-project-root)))
    (unless (and (tsp-root *tests-page*) root (equal (namestring root) (namestring (tsp-root *tests-page*))))
      (scan-project-tests *tests-page* root))))

(defun tests-disconnected ()
  (setf *test-runner-connection* nil)
  (when (and *tests-page* (tsp-running *tests-page*))
    (setf (tsp-running *tests-page*) nil)
    (fill-tests-list *tests-page*)))

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
