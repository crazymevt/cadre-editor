;;;; session.lisp — the connections to the Lisps being edited
;;;;
;;;; *CONNECTION* is the Common Lisp one, through Swank. Commands that need
;;;; it use WITH-CONNECTION: if there is none, Cadre starts one with
;;;; *lisp-command* and runs the command once it is ready. Swank messages
;;;; arrive on the reader thread and are handled on the GTK thread.
;;;;
;;;; Other Lisps (Clojure, Racket) get one connection each, kept in
;;;; *CONNECTIONS* and started by the function their back end registers in
;;;; *CONNECTION-STARTERS*. Requests go through REQUEST and the BACKEND-…
;;;; generic functions (core/backend.lisp), so the same commands work with
;;;; any of them.

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

(defvar *clojure-starting* nil "True while a Clojure REPL starts (clojure.lisp).")
(defvar *racket-starting* nil "True while Racket starts (racket.lisp).")
(defvar *connections* (make-hash-table) "Language (:clojure, :racket) → its open connection.")
(defvar *connection-starters* (make-hash-table)
  "Language → a function of a callback that starts a connection for it and
calls the callback with the connection once it is ready.")

(defun language-connection (language)
  "The open connection for LANGUAGE (a dialect name), or nil."
  (if (eq language :common-lisp)
      (and (connected-p) *connection*)
      (let ((connection (gethash language *connections*)))
        (and (connection-open-p connection) connection))))

(defun call-with-language-connection (language function)
  "Call FUNCTION with LANGUAGE's connection, starting one if there is none."
  (cond ((eq language :common-lisp) (call-with-connection function))
        ((language-connection language) (funcall function (language-connection language)))
        ((gethash language *connection-starters*)
         (funcall (gethash language *connection-starters*)
                  (lambda (connection)
                    (setf (gethash language *connections*) connection)
                    (funcall function connection))))
        (t (editor-error "Cadre can't run ~a code yet"
                         (dialect-title (or (find-dialect language) (error "No language ~s" language)))))))

(defmacro with-connection ((var &key (language :common-lisp)) &body body)
  "Run BODY with VAR bound to LANGUAGE's connection (the Common Lisp one by
default), starting one first if there is none."
  `(call-with-language-connection ,language (lambda (,var) ,@body)))

(defun buffer-language (buffer)
  "The language BUFFER's code is in, for choosing its connection: its
mode's dialect, or Common Lisp (the REPL, *Help*, other text)."
  (or (and buffer (mode-dialect-name (buffer-major-mode buffer))) :common-lisp))

(defun view-language (view)
  (buffer-language (and view (view-buffer view))))

(defun view-connection (view)
  "The open connection for VIEW's language, or nil."
  (language-connection (view-language view)))

(defun evaluable-source (connection string)
  "STRING as CONNECTION should evaluate it: while a GTK app runs in the
Common Lisp, on its GTK thread."
  (if (eq (connection-language connection) :common-lisp) (gtk-thread-source string) string))

(defun follow-package (connection package)
  "After code from a file in PACKAGE was evaluated, switch the REPL to it
(Common Lisp's REPL; a Clojure REPL keeps its own namespace, as CIDER's does)."
  (when (eq (connection-language connection) :common-lisp)
    (repl-follow-package package)))

(defun reporting-on-ok (on-ok)
  "ON-OK, with errors it signals shown as messages."
  (lambda (value)
    (handler-case (when on-ok (funcall on-ok value))
      (editor-error (e) (message "~a" (editor-error-message e)))
      (error (e) (message "Error: ~a" e)))))

(defun reporting-on-abort (on-abort)
  (lambda (reason)
    (if on-abort
        (funcall on-abort reason)
        (message "Evaluation aborted~@[: ~a~]" reason))))

(defun request (function connection &rest arguments)
  "Call FUNCTION, one of the BACKEND-… requests, on CONNECTION with
ARGUMENTS (its own, then keywords), as REX does: errors in :on-ok are shown
as messages, and an abort without :on-abort says so."
  (let* ((keys (or (position-if #'keywordp arguments) (length arguments)))
         (positional (subseq arguments 0 keys))
         (options (subseq arguments keys)))
    (apply function connection
           (append positional
                   (list :on-ok (reporting-on-ok (getf options :on-ok))
                         :on-abort (reporting-on-abort (getf options :on-abort)))
                   (loop for (k v) on options by #'cddr
                         unless (member k '(:on-ok :on-abort)) append (list k v))))))

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
                             :on-output (lambda (line)
                                          (panel-log-raw (window-panel *window*) line)
                                          (gtk-app-output line))
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
    (traces-disconnected)
    (profiler-disconnected)
    (gtk-app-disconnected)
    (image-changed)
    (refresh-systems)
    (update-connection-status)
    (message "Lisp disconnected: ~a" reason)))

(defun disconnect-lisp (&key kill)
  (let ((connection *connection*)
        (inferior *inferior*))
    (when connection (backend-disconnect connection))
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
      (:debug-return (destructuring-bind (thread level &optional stepping &rest more) args
                       (declare (ignore more))
                       (debugger-return thread level stepping)))
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

;;; The status bar

(defvar *status-language* :common-lisp
  "Whose connection the status bar shows: the selected tab's language's.")

(defun status-language ()
  (let ((language (and *window* (selected-view *window*) (view-language (selected-view *window*)))))
    (if (member language '(:clojure :racket)) language :common-lisp)))

(defun follow-status-language ()
  "Show the selected tab's language's connection, if that changed, and its
REPL on the REPL page (when the tab is Lisp code of some kind)."
  (unless (eq (status-language) *status-language*)
    (update-connection-status))
  (let* ((view (and *window* (selected-view *window*)))
         (language (and view (mode-dialect-name (buffer-major-mode (view-buffer view))))))
    (when language (show-repl-language language)))
  (follow-project-root))

(defun language-starting-p (language)
  (case language
    (:clojure *clojure-starting*)
    (:racket *racket-starting*)
    (t *connecting*)))

(defparameter *language-status-texts*
  '((:clojure "○ No Clojure REPL" "◌ Starting Clojure…" "Click to start the project's Clojure REPL" "nREPL")
    (:racket "○ No Racket" "◌ Starting Racket…" "Click to start Racket" "Racket"))
  "Language → the status bar's text when it isn't running and while it
starts, the tooltip offering to start it, and what Cadre connects to.")

(defun update-language-status (label language)
  (destructuring-bind (none starting offer server) (rest (assoc language *language-status-texts*))
    (let ((connection (language-connection language)))
      (gtk:button-set-label label
                            (cond (connection (format nil "● ~a  ~a" (connection-implementation connection)
                                                      (connection-prompt connection)))
                                  ((language-starting-p language) starting)
                                  (t none)))
      (gtk:widget-set-tooltip-text label
                                   (cond (connection (format nil "Connected to ~a at ~a:~d. Click to show the REPL."
                                                             server (connection-host connection) (connection-port connection)))
                                         ((language-starting-p language) "Starting…")
                                         (t offer))))))

(defun update-connection-status ()
  (update-run-button)
  (setf *status-language* (status-language))
  (when (and *window* (not (eq *status-language* :common-lisp)))
    (return-from update-connection-status
      (update-language-status (window-status-connection *window*) *status-language*)))
  (when *window*
    (let ((label (window-status-connection *window*)))
      (gtk:button-set-label
       label
       (cond ((connected-p)
              (format nil "● ~a  ~a~:[~; · GTK~]" (connection-implementation *connection*)
                      (connection-prompt *connection*) (gtk-thread-evaluation-p)))
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
  "Interrupt the evaluation running in the REPL (in a Clojure or Racket
buffer or REPL, that language's)."
  (let ((connection (language-connection (view-language (current-view)))))
    (if connection
        (progn (backend-interrupt connection) (message "Interrupted"))
        (message "Not connected"))))
