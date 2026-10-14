;;;; racket.lisp — Racket, through Cadre's own back end (core/racket/)
;;;;
;;;; A Racket buffer's commands (evaluate, compile, describe, go to
;;;; definition…) use the Racket connection, which this file starts when the
;;;; first one needs it: Racket runs vendor/cadre-racket/main.rkt in the
;;;; file's package folder (the nearest with an info.rkt), and its output
;;;; goes to the Output page. Code from a file is evaluated in its module,
;;;; and Run File runs the module and puts the REPL in it, as DrRacket's Run
;;;; does. The REPL is a tab, *racket-repl* (language-repl.lisp).
;;;;
;;;; Saving a Racket file checks it with check-syntax (when Racket is
;;;; running): its errors and unused requires go to the Problems page. raco
;;;; make and raco test run like Build Project, with the places they report
;;;; going to the Problems page too.

(in-package #:cadre-ui)

(define-major-mode racket-repl-mode (:title "Racket REPL")
  "The REPL of the running Racket.")

(pushnew '(racket-repl-mode . :racket) cadre::*mode-dialects* :test #'equal)

(setf (cadre::keymap-parent (major-mode-keymap (find-major-mode 'racket-repl-mode)))
      (major-mode-keymap (find-major-mode 'repl-mode)))

(define-language-repl :racket :mode 'racket-repl-mode :buffer-name "*racket-repl*" :title "Racket"
                              :prompt-format "~a> ")

(defvar *racket-process* nil "The Racket Cadre started (an inferior-lisp), or nil.")
(defvar *racket-waiting* '() "Functions waiting for the Racket connection to be ready.")

(defun racket-connection () (language-connection :racket))

(defun show-racket-repl (&key (focus t)) (show-language-repl :racket :focus focus))

;;; Starting

(defun racket-start-directory ()
  "Where Racket starts: the current file's package folder, else its folder,
else the open project's."
  (let* ((view (current-view))
         (file (and view (buffer-file (view-buffer view))))
         (start (or file (window-project *window*) (user-homedir-pathname))))
    (or (racket-project start) (uiop:pathname-directory-pathname start))))

(defun racket-ready (connection)
  (setf (gethash :racket *connections*) connection
        *racket-starting* nil)
  (language-repl-connected :racket connection)
  (update-connection-status)
  (message "Connected to ~a" (connection-implementation connection))
  (let ((waiting (reverse *racket-waiting*)))
    (setf *racket-waiting* '())
    (dolist (f waiting)
      (handler-case (funcall f connection)
        (editor-error (e) (message "~a" (editor-error-message e)))))))

(defun racket-failed (why)
  (setf *racket-starting* nil *racket-waiting* '())
  (update-connection-status)
  (message "~a" why))

(defun start-racket-connection (then)
  "Start Racket in the current file's package, then call THEN with the connection."
  (when then (push then *racket-waiting*))
  (unless *racket-starting*
    (let ((dir (racket-start-directory)))
      (setf *racket-starting* t)
      (update-connection-status)
      (message "Starting Racket in ~a…" (uiop:native-namestring dir))
      (panel-log (window-panel *window*) (format nil "Starting ~a in ~a" *racket-command* (uiop:native-namestring dir)))
      (handler-case
          (setf *racket-process*
                (start-racket dir
                              :deliver #'deliver-to-gui
                              :on-output (lambda (line) (panel-log-raw (window-panel *window*) line))
                              :on-port (lambda (port)
                                         (handler-case
                                             (racket-connect "localhost" port
                                                             :deliver #'deliver-to-gui
                                                             :handler (lambda (connection event)
                                                                        (handle-language-event :racket connection event))
                                                             :on-ready 'racket-ready
                                                             :on-failure (lambda (reason)
                                                                           (racket-failed (format nil "Racket failed: ~a" reason))))
                                           (editor-error (e) (racket-failed (editor-error-message e)))))
                              :on-exit #'racket-failed))
        (error (e) (racket-failed (format nil "Could not start Racket: ~a" e)))))))

(setf (gethash :racket *connection-starters*) 'start-racket-connection)

(defun stop-racket-connection ()
  (let ((connection (racket-connection)))
    (when connection (backend-disconnect connection)))
  (when *racket-process*
    (kill-inferior-lisp *racket-process*)
    (setf *racket-process* nil)))

;;; Checking files when they are saved

(defun show-racket-problems (buffer notes)
  (show-notes notes :buffer buffer :replace-lines :all)
  (when notes
    (message "~a: ~d problem~:p" (buffer-name buffer) (length notes))))

(defun check-racket-buffer (buffer &key (connection (racket-connection)) quiet)
  "Check BUFFER's Racket code with check-syntax, without running it."
  (when (and connection (buffer-file buffer))
    (request #'racket-problems connection (uiop:native-namestring (buffer-file buffer))
             (text-string (buffer-text buffer))
             :on-ok (lambda (notes)
                      (show-racket-problems buffer notes)
                      (unless (or notes quiet) (message "No problems in ~a" (buffer-name buffer))))
             :on-abort (lambda (reason) (declare (ignore reason))))))

(add-hook '*after-save-hook* (lambda (buffer)
                               (when (eq (buffer-major-mode buffer) 'racket-mode)
                                 (check-racket-buffer buffer :quiet t))))

;;; Commands

(define-command racket-start ()
  "Start Racket (with Cadre's back end) in the current file's package."
  (:title "Racket: Start")
  (cond ((racket-connection) (message "Already connected to ~a" (connection-implementation (racket-connection))))
        (*racket-starting* (message "Racket is starting…"))
        (t (start-racket-connection (lambda (c) (declare (ignore c)) (show-racket-repl :focus nil))))))

(define-command racket-repl-tab ()
  "Show the Racket REPL, starting Racket if it isn't running."
  (:title "Racket: REPL")
  (show-racket-repl)
  (unless (or (racket-connection) *racket-starting*)
    (start-racket-connection nil)))

(define-command racket-stop ()
  "Stop the Racket Cadre started."
  (:title "Racket: Stop")
  (if (or (racket-connection) *racket-process*)
      (stop-racket-connection)
      (message "Racket isn't running")))

(define-command racket-restart ()
  "Stop Racket and start a fresh one."
  (:title "Racket: Restart")
  (stop-racket-connection)
  (start-racket-connection nil))

(define-command racket-run ()
  "Save the file, run its module and put the REPL in it, as DrRacket's Run does."
  (:modes racket-mode)
  (:title "Racket: Run File")
  (let ((buffer (view-buffer (current-view))))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (flet ((run-it ()
             (with-connection (connection :language :racket)
               (show-racket-repl :focus nil)
               (request #'backend-compile-file connection (uiop:native-namestring (buffer-file buffer))
                        :on-ok (lambda (result)
                                 (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                                   (show-notes notes :buffer buffer :replace-lines :all)
                                   (compilation-message notes successp duration
                                                        (format nil "Ran ~a" (buffer-name buffer)))))))))
      (if (buffer-modified-p buffer)
          (write-buffer buffer (buffer-file buffer) (lambda (ok) (when ok (run-it))))
          (run-it)))))

(define-command racket-enter-module ()
  "Put the Racket REPL in the current file's module (running it if it hasn't run)."
  (:modes racket-mode)
  (:title "Racket: Switch the REPL to This Module")
  (let ((file (buffer-file (view-buffer (current-view)))))
    (unless file (editor-error "Save the buffer to a file first"))
    (with-connection (connection :language :racket)
      (request #'backend-set-package connection (uiop:native-namestring file)
               :on-ok (lambda (reply)
                        (update-connection-status)
                        (message "Racket REPL in ~a" (second reply)))))))

(define-command racket-documentation ()
  "Open the documentation of the identifier at the cursor in the installed Racket docs."
  (:modes racket-mode racket-repl-mode)
  (:title "Racket: Documentation")
  (let* ((view (current-view))
         (syntax (buffer-sexp-syntax (view-buffer view)))
         (name (and syntax (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c)))))
    (unless name (editor-error "No identifier at the cursor"))
    (with-connection (connection :language :racket)
      (apply #'request #'racket-documentation-url connection name
             :package (view-package view)
             :on-ok (lambda (url)
                      (if url
                          (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new url) (window-gtk-window *window*))
                                     (lambda (ok) (declare (ignore ok)))
                                     :error (lambda (e) (message "Couldn't open the documentation: ~a" e)))
                          (message "No documentation for ~a" name)))
             (and (buffer-file (view-buffer view)) (racket-place-keys view))))))

(define-command racket-check-syntax ()
  "Check the file with check-syntax (without running it): errors and unused
requires go to the Problems page."
  (:modes racket-mode)
  (:title "Racket: Check Syntax")
  (let ((buffer (view-buffer (current-view))))
    (unless (buffer-file buffer) (editor-error "Save the buffer to a file first"))
    (with-connection (connection :language :racket)
      (check-racket-buffer buffer :connection connection))))

;;; raco

(defun raco-places (line)
  "A compiler note for a place LINE (raco's output) names, \"/p/f.rkt:3:4: …\"
or rackunit's \"location: /p/f.rkt:3:4\", or nil."
  (multiple-value-bind (match groups) (ppcre:scan-to-strings "(/[^:\\s]+\\.(?:rkt|scrbl|rktl)):(\\d+):(\\d+)" line)
    (when match
      (make-compiler-note (if (search "location:" line) :warning :error)
                          (let ((text (string-trim " " line)))
                            (if (search "location:" text) "Test failure (see the Output page)" text))
                          (list :file (aref groups 0)
                                :line (max 0 (1- (parse-integer (aref groups 1))))
                                :column (parse-integer (aref groups 2)))))))

(defun run-raco (root arguments what)
  (when *build* (editor-error "~a is already running (Stop Build ends it)" (getf *build* :what)))
  (let ((notes '()))
    (save-project-then
     root
     (lambda ()
       (run-project-process root (list :program *raco-command* :arguments arguments
                                       :description (format nil "raco ~{~a~^ ~}" arguments))
                            :what what
                            :on-line (lambda (line)
                                       (let ((note (raco-places line)))
                                         (when note (push note notes))))
                            :on-end (lambda (code)
                                      (declare (ignore code))
                                      (show-notes (reverse notes)))
                            :on-success (lambda (seconds)
                                          (message "~a passed in ~,1f s" what seconds)))))))

(defun raco-target ()
  "The current Racket file's package folder (or the file, if it has none),
and the folder to run raco in."
  (let* ((buffer (view-buffer (current-view)))
         (file (or (buffer-file buffer) (editor-error "Save the buffer to a file first")))
         (package (racket-project file)))
    (if package
        (values (uiop:native-namestring package) package)
        (values (uiop:native-namestring file) (uiop:pathname-directory-pathname file)))))

(define-command raco-make ()
  "Compile the current file (and what it requires) with raco make."
  (:modes racket-mode)
  (:title "Racket: raco make")
  (let ((file (buffer-file (view-buffer (current-view)))))
    (unless file (editor-error "Save the buffer to a file first"))
    (run-raco (uiop:pathname-directory-pathname file) (list "make" "-v" (uiop:native-namestring file)) "raco make")))

(define-command raco-test ()
  "Run the tests of the current file's package (or the file) with raco test."
  (:modes racket-mode)
  (:title "Racket: raco test")
  (multiple-value-bind (target root) (raco-target)
    (run-raco root (list "test" target) "raco test")))
