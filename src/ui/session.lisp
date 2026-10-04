;;;; session.lisp — the connection to the Lisp being edited
;;;;
;;;; There is one connection at a time (for now). Commands that need the
;;;; Lisp use WITH-CONNECTION: if there is none, Cadre starts one with
;;;; *lisp-command* and runs the command once it is ready. Swank messages
;;;; arrive on the reader thread and are handled on the GTK thread.

(in-package #:cadre-ui)

(defvar *connection* nil "The connection to the Lisp being edited, or nil.")
(defvar *inferior* nil "The Lisp Cadre started, or nil.")
(defvar *connecting* nil "True while a Lisp is starting or a connection is being set up.")
(defvar *when-connected* '() "Functions to call with the connection once it is ready.")

(defun deliver-to-gui (thunk)
  (glib:call-in-main-thread thunk))

(defun connected-p ()
  (connection-open-p *connection*))

(defun call-with-connection (function)
  "Call FUNCTION with the connection, first starting a Lisp if there is none."
  (if (connected-p)
      (funcall function *connection*)
      (progn
        (setf *when-connected* (append *when-connected* (list function)))
        (unless *connecting* (start-lisp)))))

(defmacro with-connection ((var) &body body)
  `(call-with-connection (lambda (,var) ,@body)))

(defun rex (connection form &rest options &key on-ok on-abort package thread)
  "SWANK-REX with errors in ON-OK reported as messages."
  (declare (ignore package thread))
  (apply #'swank-rex connection form
         :on-ok (lambda (value)
                  (handler-case (when on-ok (funcall on-ok value))
                    (editor-error (e) (message "~a" (editor-error-message e)))
                    (error (e) (message "Error: ~a" e))))
         :on-abort (lambda (reason)
                     (if on-abort
                         (funcall on-abort reason)
                         (message "Evaluation aborted~@[: ~a~]" reason)))
         (loop for (k v) on options by #'cddr unless (member k '(:on-ok :on-abort)) append (list k v))))

;;; Starting and connecting

(defun start-lisp (&optional (command *lisp-command*))
  "Start a Lisp with Swank and connect to it."
  (setf *connecting* t)
  (update-connection-status)
  (message "Starting ~{~a~^ ~}…" command)
  (panel-log (window-panel *window*) (format nil "Starting ~{~a~^ ~}" command))
  (setf *inferior*
        (start-inferior-lisp :command command :deliver #'deliver-to-gui
                             :on-output (lambda (line) (panel-log-raw (window-panel *window*) line))
                             :on-port (lambda (port) (connect-to "localhost" port :inferior *inferior*))
                             :on-exit (lambda (why)
                                        (setf *connecting* nil *when-connected* '())
                                        (update-connection-status)
                                        (message "~a" why)))))

(defun connect-to (host port &key inferior)
  (setf *connecting* t)
  (update-connection-status)
  (handler-case
      (let ((connection (swank-connect host port :deliver #'deliver-to-gui :handler 'handle-swank-event)))
        (setf (connection-process connection) inferior)
        (swank-start-session connection
                             :on-ready 'session-ready
                             :on-failure (lambda (reason)
                                           (setf *connecting* nil)
                                           (update-connection-status)
                                           (message "Could not set up the Lisp: ~a" reason))))
    (editor-error (e)
      (setf *connecting* nil)
      (update-connection-status)
      (message "~a" (editor-error-message e)))))

(defun session-ready (connection)
  (setf *connection* connection *connecting* nil)
  (update-connection-status)
  (repl-connected connection)
  (image-changed)
  (refresh-systems)
  (message "Connected to ~a" (connection-implementation connection))
  (let ((pending *when-connected*))
    (setf *when-connected* '())
    (dolist (f pending)
      (handler-case (funcall f connection)
        (editor-error (e) (message "~a" (editor-error-message e)))))))

(defun session-closed (connection reason)
  (when (eq connection *connection*)
    (setf *connection* nil)
    (repl-disconnected reason)
    (debugger-clear)
    (image-changed)
    (refresh-systems)
    (update-connection-status)
    (message "Lisp disconnected: ~a" reason)))

(defun disconnect-lisp (&key kill)
  (let ((connection *connection*)
        (inferior *inferior*))
    (when connection (swank-disconnect connection))
    (when (and kill inferior)
      (kill-inferior-lisp inferior)
      (setf *inferior* nil))))

;;; Events from the Lisp

(defun handle-swank-event (connection event)
  (destructuring-bind (kind &rest args) event
    (case kind
      (:write-string (destructuring-bind (string &optional target thread) args
                       (declare (ignore thread))
                       (repl-output string target)))
      (:presentation-start (presentation-start (first args)))
      (:presentation-end (presentation-end (first args)))
      (:new-package (destructuring-bind (package prompt) args
                      (setf (connection-package connection) package
                            (connection-prompt connection) prompt)
                      (update-connection-status)))
      (:debug (apply #'debugger-enter connection args))
      (:debug-activate nil)
      (:debug-return (destructuring-bind (thread level &rest more) args
                       (declare (ignore more))
                       (debugger-return thread level)))
      (:read-string (destructuring-bind (thread tag) args
                      (repl-read-string connection thread tag)))
      (:read-aborted (repl-read-aborted))
      (:y-or-n-p (destructuring-bind (thread tag question) args
                   (ask-y-or-n connection thread tag question)))
      (:read-from-minibuffer (destructuring-bind (thread tag prompt &optional initial) args
                               (open-picker (window-picker *window*)
                                            :placeholder prompt :text (or initial "")
                                            :on-choose (lambda (text)
                                                         (swank-send connection (list :emacs-return thread tag text))))))
      (:inspect (destructuring-bind (what &optional thread tag) args
                  (show-inspection what)
                  (when tag (swank-send connection (list :emacs-return thread tag nil)))))
      (:indentation-update (learn-indentation (first args)))
      (:background-message (message "~a" (first args)))
      (:new-features nil)
      (:disconnected (session-closed connection (first args)))
      (t (panel-log (window-panel *window*) (format nil "Unhandled message from the Lisp: ~a" kind))))))

(defun ask-y-or-n (connection thread tag question)
  (let ((dialog (adw:alert-dialog-new "The Lisp asks" question)))
    (adw:alert-dialog-add-response dialog "no" "_No")
    (adw:alert-dialog-add-response dialog "yes" "_Yes")
    (adw:alert-dialog-set-close-response dialog "no")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (swank-send connection (list :emacs-return thread tag (string= response "yes")))))))

(defun learn-indentation (updates)
  "Use the indentation the Lisp reports for macros with &body (swank-indentation)."
  (dolist (update updates)
    (when (and (consp update) (stringp (car update)) (integerp (cdr update)))
      (setf (gethash (string-downcase (car update)) cadre::*indentation-specs*) (cdr update)))))

;;; The status bar

(defun update-connection-status ()
  (when *window*
    (let ((label (window-status-connection *window*)))
      (gtk:button-set-label
       label
       (cond ((connected-p)
              (format nil "● ~a  ~a" (connection-implementation *connection*)
                      (connection-prompt *connection*)))
             (*connecting* "◌ Starting Lisp…")
             (t "○ No Lisp")))
      (gtk:widget-set-tooltip-text
       label
       (cond ((connected-p) (format nil "Connected to ~a:~d. Click to show the REPL."
                                    (connection-host *connection*) (connection-port *connection*)))
             (*connecting* "Starting…")
             (t "Click to start a Lisp"))))))

;;; Commands

(define-command lisp ()
  "Start a Lisp (with *lisp-command*) and connect to it."
  (cond ((connected-p) (message "Already connected to ~a" (connection-implementation *connection*)))
        (*connecting* (message "A Lisp is starting…"))
        (t (start-lisp))))

(define-command connect ()
  "Connect to a running Swank server, given as host:port."
  (open-picker (window-picker *window*)
               :placeholder "host:port, such as localhost:4005" :text "localhost:4005"
               :on-choose (lambda (text)
                            (let* ((colon (position #\: text :from-end t))
                                   (host (if colon (subseq text 0 colon) "localhost"))
                                   (port (ignore-errors (parse-integer text :start (if colon (1+ colon) 0)))))
                              (unless port (editor-error "Not host:port: ~a" text))
                              (when (connected-p) (disconnect-lisp))
                              (connect-to host port)))))

(define-command disconnect ()
  "Disconnect from the Lisp (a Lisp Cadre started is stopped)."
  (if (connected-p)
      (disconnect-lisp :kill t)
      (message "Not connected")))

(define-command restart-lisp ()
  "Stop the Lisp Cadre started and start a fresh one."
  (disconnect-lisp :kill t)
  (start-lisp))

(define-command interrupt-lisp ()
  "Interrupt the evaluation running in the REPL."
  (if (connected-p)
      (progn (swank-interrupt *connection* :repl-thread) (message "Interrupted"))
      (message "Not connected")))
