;;;; session-log.lisp — what Cadre was doing, for finding a crash
;;;;
;;;; With *session-log* on, calls to the functions in *session-log-functions*
;;;; (commands, keys, popups, Lisp requests, the debugger, git, parsing) are
;;;; written to ~/.local/state/cadre/session.log, one line each with the time
;;;; and the thread, flushed as they happen: when Cadre dies, the last lines
;;;; say what it was in the middle of. The previous run's log is kept as
;;;; session.log.1.

(in-package #:cadre-ui)

(define-option *session-log* t boolean
  "Write what Cadre does (commands, keys, popups, Lisp requests) to
~/.local/state/cadre/session.log, to find what led to a crash. Read when
Cadre starts."
  :category "Other")

(defparameter *session-log-functions*
  '(run-command process-key
    show-completions close-completion fill-completions
    swank-rex handle-swank-event repl-eval
    debugger-enter debugger-return debugger-render
    panel-show git-changed refresh-source-control
    background-parse-done cadre::ts-install-state
    open-file-path show-buffer write-buffer render-image
    send-to-claude handle-claude-event chat-show-approval cadre::call-mcp-tool)
  "The functions whose calls the session log records.")

(defparameter *session-log-returns* '(cadre::call-mcp-tool)
  "Of those, the functions whose returns are logged too: slow ones, to see if they finished.")

(defparameter *session-log-limit* (* 50 1024 1024)
  "Bytes the session log grows to before it starts again (keeping the old one as session.log.1).")

(defvar *session-log-stream* nil)
(defvar *session-log-lock* (sb-thread:make-mutex :name "session log"))
(defvar *session-log-depth* 0 "Calls being logged on this thread, to indent nested ones.")

(defun session-log-file () (merge-pathnames "session.log" (state-directory)))

(defun open-session-log ()
  (let* ((log (session-log-file))
         (previous (make-pathname :name "session.log" :type "1" :defaults log)))
    (ensure-directories-exist log)
    (when (probe-file log) (uiop:rename-file-overwriting-target log previous))
    (setf *session-log-stream*
          (open log :direction :output :if-exists :append :if-does-not-exist :create
                    :external-format :utf-8))))

(defun log-argument (x)
  "X, briefly, on one line: strings and lists cut short, GTK and other objects by their type."
  (substitute #\Space #\Newline (log-argument-1 x)))

(defun log-argument-1 (x)
  (typecase x
    (cadre::claude-event
     (format nil "#<claude-event ~(~a~)~@[ ~a~]~@[ ~s~]>" (cadre::event-kind x) (cadre::event-name x)
             (let ((text (cadre::event-text x)))
               (and (stringp text) (if (> (length text) 40) (concatenate 'string (subseq text 0 40) "…") text)))))
    (string (prin1-to-string (if (> (length x) 60) (concatenate 'string (subseq x 0 60) "…") x)))
    ((or number symbol character) (prin1-to-string x))
    (cons (let ((*print-length* 4) (*print-level* 2))
            (let ((s (handler-case (prin1-to-string x) (error () "(…)"))))
              (if (> (length s) 100) (concatenate 'string (subseq s 0 100) "…") s))))
    (t (format nil "#<~(~a~)>" (type-of x)))))

(defun session-log (format &rest arguments)
  "Write a line to the session log, if it's open."
  (let ((stream *session-log-stream*))
    (when stream
      (handler-case
          (let ((line (apply #'format nil format arguments))
                (thread (sb-thread:thread-name sb-thread:*current-thread*)))
            (sb-thread:with-mutex (*session-log-lock*)
              (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
                (multiple-value-bind (s m h) (decode-universal-time (+ seconds (encode-universal-time 0 0 0 1 1 1970 0)))
                  (format stream "~2,'0d:~2,'0d:~2,'0d.~3,'0d ~@[[~a] ~]~a~a~%"
                          h m s (floor microseconds 1000)
                          (and (not (eq sb-thread:*current-thread* (sb-thread:main-thread))) thread)
                          (make-string (* 2 *session-log-depth*) :initial-element #\Space) line)))
              (finish-output stream)
              (when (> (file-position stream) *session-log-limit*)
                (close stream)
                (open-session-log))))
        (error () nil)))))

(defun log-calls (name)
  (when (fboundp name)
    (sb-int:unencapsulate name 'session-log)
    (sb-int:encapsulate name 'session-log
                        (lambda (function &rest arguments)
                          (session-log "~(~a~)~{ ~a~}" name (mapcar #'log-argument arguments))
                          (let ((*session-log-depth* (1+ *session-log-depth*)))
                            (if (member name *session-log-returns*)
                                (let ((values (multiple-value-list (apply function arguments))))
                                  (session-log "~(~a~) returned~{ ~a~}" name (mapcar #'log-argument values))
                                  (values-list values))
                                (apply function arguments)))))))

(defun start-session-log ()
  "Open the session log and start recording, if *session-log* is on."
  (when (and *session-log* (null *session-log-stream*))
    (handler-case
        (progn
          (open-session-log)
          (session-log ";; Cadre started, pid ~d" (sb-posix:getpid))
          ;; gtk4 catches an object freed under Lisp's reference (the crash
          ;; this log is for) and puts its backtrace in cadre.log.
          (setf gtk4.runtime::*lost-reference-hook*
                (lambda (type address)
                  (session-log "!! gtk4: a ~a at #x~x was freed under Lisp's reference (backtrace in cadre.log)"
                               type address)))
          (mapc #'log-calls *session-log-functions*))
      (error (e) (format *error-output* "~&;; No session log: ~a~%" e)))))

(defun stop-session-log ()
  (dolist (name *session-log-functions*)
    (when (fboundp name) (sb-int:unencapsulate name 'session-log)))
  (when *session-log-stream*
    (session-log ";; Cadre exited normally")
    (close *session-log-stream*)
    (setf *session-log-stream* nil)))
