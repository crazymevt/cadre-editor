;;;; build.lisp — Build Project: make the project's program
;;;;
;;;; Saves the project's changed files, then runs the build the core plans
;;;; (project-build-plan: make build, or ASDF's make in a new Lisp) in the
;;;; project's folder. Its output streams to the Output page; when it ends,
;;;; the status bar says how it went. One build at a time; Stop Build ends it.

(in-package #:cadre-ui)

(defvar *build* nil "The running build, as a plist (:process :plan :start :root), or nil.")

(defun build-log (line)
  (panel-log-raw (window-panel *window*) line))

(defun build-finished (code)
  (let* ((build *build*)
         (plan (getf build :plan))
         (seconds (/ (- (get-internal-real-time) (getf build :start)) internal-time-units-per-second))
         (output (getf plan :output)))
    (setf *build* nil)
    (cond ((eql code 0)
           (let ((size (and output (ignore-errors (with-open-file (in output :element-type '(unsigned-byte 8))
                                                    (file-length in))))))
             (panel-log (window-panel *window*) (format nil "Build finished in ~,1f s" seconds))
             (message "Built ~:[the project~;~:*~a~]~@[ (~,1f MB)~] in ~,1f s"
                      output (and size (/ size 1048576.0)) seconds)))
          ((getf build :stopped)
           (panel-log (window-panel *window*) "Build stopped")
           (message "Build stopped"))
          (t
           (panel-log (window-panel *window*) (format nil "Build failed (exit ~a)" code))
           (message "Build failed (exit ~a): see the Output page" code)
           (call-command 'show-output)))))

(defun start-build (root plan)
  (panel-log (window-panel *window*)
             (format nil "Building ~a: ~a" (uiop:native-namestring root) (getf plan :description)))
  (call-command 'show-output)
  (let ((process (handler-case
                     (sb-ext:run-program (getf plan :program) (getf plan :arguments)
                                         :search t :wait nil :directory (uiop:native-namestring root)
                                         :input nil :output :stream :error :output
                                         :external-format :utf-8)
                   (error (e) (editor-error "Could not run ~a: ~a" (getf plan :program) e)))))
    (setf *build* (list :process process :plan plan :root root :start (get-internal-real-time)))
    (message "Building: ~a…" (getf plan :description))
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

(define-command build-project ()
  "Build the project's program: make build, or, for a system with a
:build-operation, ASDF's make in a new Lisp. Saves the project's files first."
  (let ((root (or (and *window* (window-project *window*)) (editor-error "Open a project first"))))
    (when *build* (editor-error "A build is already running (Stop Build ends it)"))
    (let ((plan (or (project-build-plan root)
                    (editor-error "Nothing to build: no build target in the Makefile, and no system with a :build-operation"))))
      (let ((modified (remove-if-not (lambda (b)
                                       (and (buffer-needs-saving-p b) (buffer-file b)
                                            (uiop:subpathp (real-path (buffer-file b)) (real-path root))))
                                     (buffer-list))))
        (if modified
            (save-buffers *window* modified (lambda (ok) (if ok (start-build root plan) (message "Not built: a file couldn't be saved"))))
            (start-build root plan))))))

(define-command stop-build ()
  "Stop the running build."
  (unless *build* (editor-error "No build is running"))
  (setf (getf *build* :stopped) t)
  ;; The whole process group: make's children (the Lisp doing the build) too.
  (let ((process (getf *build* :process)))
    (unless (ignore-errors (sb-ext:process-kill process 15 :process-group))
      (sb-ext:process-kill process 15))))
