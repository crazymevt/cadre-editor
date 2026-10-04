;;;; build.lisp — Build Project and Run Tests
;;;;
;;;; Build Project saves the project's changed files, then runs the build the
;;;; core plans (project-build-plan: make build, or ASDF's make in a new
;;;; Lisp) in the project's folder. Run Tests in a New Lisp does the same
;;;; with project-test-plan (make test, or asdf:test-system in a new Lisp).
;;;; Their output streams to the Output page; when one ends, the status bar
;;;; says how it went. One at a time; Stop Build ends it.
;;;;
;;;; Run Tests runs asdf:test-system in the connected Lisp's REPL instead:
;;;; quicker, and a failing test's error opens the debugger.

(in-package #:cadre-ui)

(defvar *build* nil
  "The running build or test run, as a plist (:process :plan :start :root
:what :on-success), or nil.")

(defun build-log (line)
  (panel-log-raw (window-panel *window*) line))

(defun build-finished (code)
  (let* ((build *build*)
         (what (getf build :what))
         (seconds (/ (- (get-internal-real-time) (getf build :start)) internal-time-units-per-second)))
    (setf *build* nil)
    (cond ((eql code 0)
           (panel-log (window-panel *window*) (format nil "~a finished in ~,1f s" what seconds))
           (funcall (getf build :on-success) seconds))
          ((getf build :stopped)
           (panel-log (window-panel *window*) (format nil "~a stopped" what))
           (message "~a stopped" what))
          (t
           (panel-log (window-panel *window*) (format nil "~a failed (exit ~a)" what code))
           (message "~a failed (exit ~a): see the Output page" what code)
           (call-command 'show-output)))))

(defun run-project-process (root plan &key (what "Build") on-success)
  "Run PLAN (from project-build-plan or project-test-plan) in ROOT, with its
output on the Output page. WHAT names it in messages; ON-SUCCESS is called
with the seconds taken if it exits 0."
  (panel-log (window-panel *window*)
             (format nil "~a in ~a: ~a" what (uiop:native-namestring root) (getf plan :description)))
  (call-command 'show-output)
  (let ((process (handler-case
                     (sb-ext:run-program (getf plan :program) (getf plan :arguments)
                                         :search t :wait nil :directory (uiop:native-namestring root)
                                         :input nil :output :stream :error :output
                                         :external-format :utf-8)
                   (error (e) (editor-error "Could not run ~a: ~a" (getf plan :program) e)))))
    (setf *build* (list :process process :plan plan :root root :what what
                        :on-success (or on-success (lambda (seconds) (message "~a finished in ~,1f s" what seconds)))
                        :start (get-internal-real-time)))
    (message "~a: ~a…" what (getf plan :description))
    (sb-thread:make-thread
     (lambda ()
       (let ((stream (sb-ext:process-output process)))
         (loop for line = (ignore-errors (read-line stream nil))
               while line
               do (let ((line line)) (glib:call-in-main-thread (lambda () (build-log line)))))
         (sb-ext:process-wait process)
         (let ((code (sb-ext:process-exit-code process)))
           (glib:call-in-main-thread (lambda () (build-finished code))))))
     :name "cadre build")))

(defun project-root-or-error ()
  (or (and *window* (window-project *window*)) (editor-error "Open a project first")))

(defun save-project-then (root continuation)
  "Save the changed files under ROOT, then call CONTINUATION."
  (let ((modified (remove-if-not (lambda (b)
                                   (and (buffer-needs-saving-p b) (buffer-file b)
                                        (uiop:subpathp (real-path (buffer-file b)) (real-path root))))
                                 (buffer-list))))
    (if modified
        (save-buffers *window* modified (lambda (ok) (if ok (funcall continuation) (message "A file couldn't be saved"))))
        (funcall continuation))))

;;; Building

(define-command build-project ()
  "Build the project's program: make build, or, for a system with a
:build-operation, ASDF's make in a new Lisp. Saves the project's files first."
  (let ((root (project-root-or-error)))
    (when *build* (editor-error "~a is already running (Stop Build ends it)" (getf *build* :what)))
    (let ((plan (or (project-build-plan root)
                    (editor-error "Nothing to build: no build target in the Makefile, and no system with a :build-operation"))))
      (save-project-then root
                         (lambda ()
                           (run-project-process
                            root plan :what "Build"
                            :on-success (lambda (seconds)
                                          (let* ((output (getf plan :output))
                                                 (size (and output (ignore-errors (with-open-file (in output :element-type '(unsigned-byte 8))
                                                                                    (file-length in))))))
                                            (message "Built ~:[the project~;~:*~a~]~@[ (~,1f MB)~] in ~,1f s"
                                                     output (and size (/ size 1048576.0)) seconds)))))))))

(define-command stop-build ()
  "Stop the running build or test run."
  (unless *build* (editor-error "Nothing is running"))
  (setf (getf *build* :stopped) t)
  ;; The whole process group: make's children (the Lisp doing the work) too.
  (let ((process (getf *build* :process)))
    (unless (ignore-errors (sb-ext:process-kill process 15 :process-group))
      (sb-ext:process-kill process 15))))

;;; Tests

(defun call-with-test-system (root function)
  "Call FUNCTION with the system to test in ROOT: the only main system (one
without a / in its name, whose test-op runs its NAME/tests), or the one
chosen (the last one tested first)."
  (let* ((all (or (project-systems root) (editor-error "No .asd file in ~a" (uiop:native-namestring root))))
         (main (remove-if (lambda (s) (find #\/ s)) all))
         (systems (if (= 1 (length main)) main all))
         (last (setting :last-test-system)))
    (flet ((choose (system)
             (setf (setting :last-test-system) system)
             (funcall function system)))
      (if (null (rest systems))
          (choose (first systems))
          (open-picker (window-picker *window*)
                       :items (if (member last systems :test #'string=)
                                  (cons last (remove last systems :test #'string=))
                                  systems)
                       :placeholder "Test which system?"
                       :on-choose #'choose)))))

(defun run-tests-form (root system)
  "Source that runs SYSTEM's tests, loading it and its NAME/... systems (its
tests) with Quicklisp first when the Lisp has it, so their dependencies are
fetched: asdf:test-system only finds what is already installed."
  (let ((systems (cons system (remove-if-not (lambda (s) (and (> (length s) (length system))
                                                             (string= (concatenate 'string system "/")
                                                                      s :end2 (1+ (length system)))))
                                             (project-systems root)))))
    (asdf-form (format nil "(progn (when (find-package \"QL\") (funcall (find-symbol \"QUICKLOAD\" \"QL\") (quote ~s))) (funcall (find-symbol \"TEST-SYSTEM\" \"ASDF\") ~a))"
                       systems (cadre::lisp-string system))
               root)))

(define-command run-tests ()
  "Run the project's tests (asdf:test-system) in the connected Lisp's REPL,
after saving the project's files. A failing test's error opens the debugger."
  (let ((root (project-root-or-error)))
    (call-with-test-system
     root
     (lambda (system)
       (save-project-then root
                          (lambda ()
                            (run-in-repl (format nil "Testing system ~a" system)
                                         (run-tests-form root system))))))))

(define-command run-tests-in-new-lisp ()
  "Run the project's tests in a fresh Lisp (make test, or asdf:test-system),
so nothing left in your session can make them pass. Output goes to the
Output page."
  (let ((root (project-root-or-error)))
    (when *build* (editor-error "~a is already running (Stop Build ends it)" (getf *build* :what)))
    (flet ((run (system)
             (let ((plan (project-test-plan root system)))
               (save-project-then root
                                  (lambda ()
                                    (run-project-process
                                     root plan :what "Test run"
                                     :on-success (lambda (seconds)
                                                   ;; make test (as new projects write it) fails when a test does;
                                                   ;; asdf:test-system doesn't say.
                                                   (if (string= (getf plan :program) "make")
                                                       (message "Tests passed (~,1f s)" seconds)
                                                       (message "Test run finished in ~,1f s: the results are on the Output page" seconds)))))))))
      ;; make test needs no system; otherwise ask which.
      (if (makefile-has-target-p (merge-pathnames "Makefile" root) "test")
          (run nil)
          (call-with-test-system root #'run)))))
