;;;; gtk-app.lisp — Run GTK App: a gtk4 program and a REPL at the same time
;;;;
;;;; On macOS, GTK must run on a process's first thread, while Swank runs
;;;; the REPL on others. In a Lisp Cadre started, the first thread sits in
;;;; the Lisp's own REPL reading standard input, which Cadre holds, so Run GTK
;;;; App sends it a form there: load the project's system and call its entry
;;;; point. GTK takes that thread; Swank, and so Cadre, keep working.
;;;;
;;;; While the app runs:
;;;;  - what is evaluated from the REPL or a file is wrapped in
;;;;    (glib:in-main-thread (:wait t) …), so GTK calls happen on the GTK
;;;;    thread (Evaluate on the GTK Thread turns this off; IN-PACKAGE is left
;;;;    alone, as it must run in the REPL's thread);
;;;;  - an error in a GTK callback opens the debugger, with an ABORT restart
;;;;    that returns from the callback (never unwinding through GTK's C);
;;;;  - when the entry point returns (the last window closed), the form
;;;;    prints a marker, and Cadre notices.

(in-package #:cadre-ui)

(defvar *gtk-app* nil
  "The GTK app started with Run GTK App, as a plist (:system :entry :running
:gtk-thread), or nil.")

(defparameter *gtk-app-ended-marker* ";; cadre: GTK app ended")

(defun gtk-app-running-p () (and *gtk-app* (getf *gtk-app* :running)))

(defun gtk-thread-evaluation-p ()
  "True if evaluations should run on the GTK thread now."
  (and (gtk-app-running-p) (getf *gtk-app* :gtk-thread)))

(defun gtk-thread-source (source)
  "SOURCE (forms to evaluate), to run on the GTK thread while a GTK app runs."
  (if (and (gtk-thread-evaluation-p)
           (not (let ((trimmed (string-left-trim '(#\Space #\Tab #\Newline) source)))
                  (and (>= (length trimmed) 11) (string-equal "(in-package" trimmed :end2 11)))))
      ;; The newline keeps a trailing comment from swallowing the paren.
      (format nil "(glib:in-main-thread (:wait t) ~a~%)" source)
      source))

(defun gtk-entry-point (root system)
  "The project's entry point: the :entry-point of its system, or the one
given before for this project."
  (or (let* ((asd (find-if (lambda (f) (string-equal (pathname-name f) system))
                           (uiop:directory-files root "*.asd")))
             (text (and asd (read-text-file asd))))
        (and text (multiple-value-bind (match groups) (cl-ppcre:scan-to-strings ":entry-point\\s+\"([^\"]+)\"" text)
                    (and match (aref groups 0)))))
      (cdr (assoc (uiop:native-namestring root) (setting :gtk-entry-points) :test #'string=))))

(defun remember-gtk-entry-point (root entry)
  (setf (setting :gtk-entry-points)
        (cons (cons (uiop:native-namestring root) entry)
              (remove (uiop:native-namestring root) (setting :gtk-entry-points) :key #'car :test #'string=))))

(defun gtk-app-launch-form (root system entry)
  "Source for the Lisp's first thread: load SYSTEM, have GTK callback errors
open the debugger, call ENTRY, then print the marker."
  (format nil "(progn (require \"ASDF\") (pushnew ~a (symbol-value (find-symbol \"*CENTRAL-REGISTRY*\" \"ASDF\")) :test (function equal)) (if (find-package \"QL\") (funcall (find-symbol \"QUICKLOAD\" \"QL\") ~a) (funcall (find-symbol \"LOAD-SYSTEM\" \"ASDF\") ~a)) (let ((handler (find-symbol \"*CALLBACK-ERROR-HANDLER*\" \"GTK4.RUNTIME\"))) (when handler (setf (symbol-value handler) (lambda (condition where) (with-simple-restart (abort \"Return from the GTK callback (~~a)\" where) (invoke-debugger condition)))))) (unwind-protect (funcall (read-from-string ~a)) (format t \"~~&~a~~%\") (finish-output)))"
          (cadre::lisp-string (uiop:native-namestring root))
          (cadre::lisp-string system) (cadre::lisp-string system)
          (cadre::lisp-string entry)
          *gtk-app-ended-marker*))

(defun gtk-app-output (line)
  "See each line the started Lisp prints: notice when the GTK app ends."
  (when (and (gtk-app-running-p) (search *gtk-app-ended-marker* line))
    (setf (getf *gtk-app* :running) nil)
    (update-connection-status)
    (message "The GTK app ended; Run GTK App starts it again")))

(defun send-to-first-thread (source)
  "Have the started Lisp's first thread evaluate SOURCE, through its standard input."
  (let ((process (and *inferior* (inferior-alive-p *inferior*) (inferior-process *inferior*))))
    (unless process
      (editor-error "Run GTK App needs a Lisp that Cadre started (M-x lisp), not one it connected to"))
    (let ((in (sb-ext:process-input process)))
      (write-line source in)
      (finish-output in))))

(defun launch-gtk-app (root system entry)
  (with-connection (connection)
    (declare (ignore connection))
    (send-to-first-thread (gtk-app-launch-form root system entry))
    (setf *gtk-app* (list :system system :entry entry :root root :running t :gtk-thread t))
    (update-connection-status)
    (message "Starting ~a on the main thread; the REPL evaluates on the GTK thread while it runs" entry)))

(define-command run-gtk-app ()
  "Run the project's gtk4 program on the Lisp's first thread (as macOS
requires), keeping the REPL: load its system, call its entry point. While it
runs, evaluations happen on the GTK thread and callback errors open the
debugger."
  (let ((root (project-root-or-error)))
    (when (gtk-app-running-p) (editor-error "The GTK app is running (Stop GTK App quits it)"))
    (call-with-test-system
     root
     (lambda (system)
       (let ((entry (gtk-entry-point root system)))
         (if entry
             (launch-gtk-app root system entry)
             (open-picker (window-picker *window*)
                          :placeholder "The function that runs the app (it runs on the main thread)"
                          :text (format nil "~a:main" system)
                          :on-choose (lambda (text)
                                       (let ((entry (string-trim " " text)))
                                         (unless (string= entry "")
                                           (remember-gtk-entry-point root entry)
                                           (launch-gtk-app root system entry)))))))))))

(define-command stop-gtk-app ()
  "Quit the GTK app started with Run GTK App (the Lisp keeps running)."
  (unless (gtk-app-running-p) (editor-error "No GTK app is running"))
  (with-connection (connection)
    (rex connection
         (swank-call "swank:interactive-eval"
                     "(glib:in-main-thread () (let ((app (gio:application-get-default))) (when app (gio:application-quit app))))"
                     3 120)
         :on-ok (lambda (v) (declare (ignore v)) (message "Quitting the GTK app…")))))

(define-command toggle-gtk-thread-evaluation ()
  "While a GTK app runs: evaluate on the GTK thread (the default), or in the REPL's own thread."
  (unless (gtk-app-running-p) (editor-error "No GTK app is running"))
  (setf (getf *gtk-app* :gtk-thread) (not (getf *gtk-app* :gtk-thread)))
  (update-connection-status)
  (message "Evaluating ~:[in the REPL's thread~;on the GTK thread~]" (getf *gtk-app* :gtk-thread)))

(defun gtk-app-disconnected ()
  (setf *gtk-app* nil))
