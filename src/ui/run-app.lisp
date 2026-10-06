;;;; run-app.lisp — Run App: the ▶ button in the header bar
;;;;
;;;; A project whose .asd has an :entry-point is an app; the button runs it
;;;; and, while it runs, stops it. Saving the project's files comes first,
;;;; and loading the system recompiles what changed, so each run has the
;;;; latest code.
;;;;  - A system that depends on gtk4 runs as Run GTK App runs it, on the
;;;;    Lisp's first thread (gtk-app.lisp).
;;;;  - Any other runs in the REPL, as if typed: its output goes there, and
;;;;    when it reads *standard-input* (read-line), the REPL asks for a line
;;;;    and RET sends it. Stopping it aborts the REPL's evaluation.
;;;;  - One that depends on raylib starts its game with rl:run, which hands
;;;;    it to the Lisp's first thread and returns at once, leaving the REPL
;;;;    free. The app runs until rl:running-p says the game ended, and
;;;;    stopping it calls rl:stop.

(in-package #:cadre-ui)

(defvar *console-app* nil
  "The program started in the REPL with Run App, as a plist (:system :entry
:raylib :game), or nil. :game is true once a raylib app's entry point has
returned and its game runs on.")

(defparameter *raylib-poll-ms* 500
  "How often Cadre asks whether a raylib game still runs.")

(defun app-running-p () (or (gtk-app-running-p) (and *console-app* t)))

(defun console-app-form (root system entry)
  "Source that loads SYSTEM, notes the thread it runs on (so Stop App can
abort it), and calls ENTRY."
  (asdf-form (format nil "(progn (if (find-package \"QL\") (funcall (find-symbol \"QUICKLOAD\" \"QL\") ~a) (funcall (find-symbol \"LOAD-SYSTEM\" \"ASDF\") ~a)) (setf (symbol-value (intern \"*CADRE-APP-THREAD*\" \"CL-USER\")) (funcall (find-symbol \"CURRENT-THREAD\" \"SWANK/BACKEND\"))) (funcall (read-from-string ~a)))"
                     (cadre::lisp-string system) (cadre::lisp-string system) (cadre::lisp-string entry))
             root))

(defun console-app-ended ()
  (setf *console-app* nil)
  (update-run-button))

(defun console-app-returned ()
  "The entry point returned: the app ended, unless it started a raylib game."
  (if (getf *console-app* :raylib)
      (progn (setf (getf *console-app* :game) t)
             (watch-raylib-game *console-app*))
      (console-app-ended)))

(defun watch-raylib-game (app)
  "Ask the Lisp, every *RAYLIB-POLL-MS*, whether APP's game still runs; when it
doesn't, the app has ended."
  (glib:timeout-add glib:+priority-default+ *raylib-poll-ms*
                    (lambda ()
                      (when (and (eq app *console-app*) *connection*)
                        (rex *connection*
                             (swank-call "swank:eval-and-grab-output"
                                         "(let ((running-p (find-symbol \"RUNNING-P\" \"RAYLIB\"))) (and running-p (funcall running-p) t))")
                             :on-ok (lambda (result)
                                      (when (eq app *console-app*)
                                        (if (string-equal "NIL" (second result))
                                            (progn (console-app-ended)
                                                   (message "The game ended; ▶ starts it again"))
                                            (watch-raylib-game app))))
                             :on-abort (lambda (reason)
                                         (declare (ignore reason))
                                         (when (eq app *console-app*) (console-app-ended)))))
                      nil)))

(defun run-console-app (root system entry &key raylib)
  (with-connection (connection)
    (declare (ignore connection))
    (when (repl-busy *repl*) (editor-error "The REPL is busy"))
    (show-repl-page :focus t)
    (repl-fresh-line)
    (repl-insert (format nil "; Running ~a (input it reads is typed here, then RET)~%" entry) "cadre-repl-note")
    (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*)
                               (gtk:text-buffer-get-end-iter (repl-gtk-buffer)))
    (setf *console-app* (list :system system :entry entry :raylib raylib))
    (update-run-button)
    (repl-eval (console-app-form root system entry) :on-done #'console-app-returned)))

(define-command run-app ()
  "Run the project's program, the :entry-point in its .asd, after saving the
project's files: a gtk4 one as Run GTK App does, any other in the REPL,
which takes the input it reads."
  (let* ((root (project-root-or-error))
         (app (or (project-app root) (editor-error "No .asd file in the project has an :entry-point"))))
    (when (app-running-p) (editor-error "The app is running (Stop App ends it)"))
    (save-project-then root
                       (lambda ()
                         (if (getf app :gtk)
                             (launch-gtk-app root (getf app :system) (getf app :entry))
                             (run-console-app root (getf app :system) (getf app :entry)
                                              :raylib (getf app :raylib)))))))

(define-command stop-app ()
  "Stop the program started with Run App."
  (cond ((gtk-app-running-p) (stop-gtk-app))
        ((getf *console-app* :game)
         (with-connection (connection)
           (rex connection
                (swank-call "swank:interactive-eval"
                            "(let ((stop (find-symbol \"STOP\" \"RAYLIB\"))) (when stop (funcall stop)) nil)"
                            3 120)
                :on-ok (lambda (v) (declare (ignore v)) (message "Stopping the game…")))))
        (*console-app*
         (let ((entry (getf *console-app* :entry)))
           (with-connection (connection)
             (rex connection
                  (swank-call "swank:interactive-eval"
                              "(let ((thread (symbol-value (find-symbol \"*CADRE-APP-THREAD*\" \"CL-USER\")))) (funcall (find-symbol \"INTERRUPT-THREAD\" \"SWANK/BACKEND\") thread (lambda () (abort))) nil)"
                              3 120)
                  :on-ok (lambda (v) (declare (ignore v)) (message "Stopped ~a" entry))))))
        (t (editor-error "No app is running"))))

(defun update-run-button ()
  "Show the ▶ button when the project has an app, as ■ while it runs."
  (let ((button (gethash :run-button *named-widgets*)))
    (when (and button *window*)
      (let* ((root (window-project *window*))
             (app (and root (project-app root)))
             (running (app-running-p)))
        (gtk:widget-set-visible button (or running (and app t)))
        (gtk:button-set-icon-name button (if running "media-playback-stop-symbolic" "media-playback-start-symbolic"))
        (gtk:widget-set-tooltip-text button
                                     (cond (running "Stop the app")
                                           (app (format nil "Run ~a~:[ (in the REPL)~;~]" (getf app :entry) (getf app :gtk)))
                                           (t "")))))))

;; Adding or removing an :entry-point shows or hides the button.
(add-hook '*after-save-hook* (lambda (buffer)
                               (let ((file (buffer-file buffer)))
                                 (when (and file (string-equal (pathname-type file) "asd"))
                                   (update-run-button)))))
