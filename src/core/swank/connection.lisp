;;;; connection.lisp — a connection to a Swank server
;;;;
;;;; Messages are a 6-digit hex length (of the UTF-8 octets) followed by an
;;;; s-expression. A reader thread reads messages and hands each to the
;;;; connection's DELIVER function, which decides where it is handled: the
;;;; GUI passes a function that queues it on the GTK thread.
;;;;
;;;; Requests are (:emacs-rex FORM PACKAGE THREAD ID); the answer comes back
;;;; as (:return (:ok VALUE) ID) or (:return (:abort REASON) ID) and goes to
;;;; the continuation registered for ID. Other messages (output, debugger
;;;; events, …) go to the connection's HANDLER.

(in-package #:cadre)

(defvar *swank-trace* nil
  "When a function, it is called with :send or :receive and the text of each
Swank message: for debugging the protocol.")

(defclass swank-connection ()
  ((socket :initarg :socket :reader connection-socket)
   (stream :initarg :stream :reader connection-stream)
   (write-lock :initform (sb-thread:make-mutex :name "swank write") :reader connection-write-lock)
   (next-id :initform 1 :accessor connection-next-id)
   (continuations :initform (make-hash-table) :reader connection-continuations)
   (reader-thread :initform nil :accessor connection-reader-thread)
   (deliver :initarg :deliver :initform #'funcall :reader connection-deliver
            :documentation "Called with a thunk to run each incoming message.")
   (handler :initarg :handler :initform nil :accessor connection-handler
            :documentation "Called with the connection and each message that is not a reply.")
   (info :initform nil :accessor connection-info
         :documentation "The plist from swank:connection-info.")
   (package :initform "COMMON-LISP-USER" :accessor connection-package)
   (prompt :initform "CL-USER" :accessor connection-prompt)
   (state :initform :open :accessor connection-state)
   (host :initarg :host :reader connection-host)
   (port :initarg :port :reader connection-port)
   (process :initform nil :accessor connection-process
            :documentation "The inferior Lisp, if Cadre started it.")))

(defmethod print-object ((c swank-connection) stream)
  (print-unreadable-object (c stream :type t)
    (format stream "~a:~d ~(~a~)" (connection-host c) (connection-port c) (connection-state c))))

(defun connection-open-p (connection)
  (and connection (eq (connection-state connection) :open)))

(defun connection-implementation (connection)
  "A short name for the other Lisp, such as \"SBCL 2.6.9\"."
  (let ((impl (getf (connection-info connection) :lisp-implementation)))
    (if impl
        (format nil "~a ~a" (getf impl :type) (getf impl :version))
        "Lisp")))

;;; Framing

(defun encode-message (sexp)
  (let* ((octets (sb-ext:string-to-octets (sexp-to-string sexp) :external-format :utf-8))
         (header (sb-ext:string-to-octets (format nil "~6,'0x" (length octets)) :external-format :ascii)))
    (concatenate '(vector (unsigned-byte 8)) header octets)))

(defun read-octets (stream count)
  (let* ((buffer (make-array count :element-type '(unsigned-byte 8)))
         (got (read-sequence buffer stream)))
    (unless (= got count) (error 'end-of-file :stream stream))
    buffer))

(defun read-message (stream)
  "Read one message from STREAM (of octets)."
  (let* ((length (parse-integer (sb-ext:octets-to-string (read-octets stream 6) :external-format :ascii)
                                :radix 16))
         (text (sb-ext:octets-to-string (read-octets stream length) :external-format :utf-8)))
    (when *swank-trace* (funcall *swank-trace* :receive text))
    (values (read-sexp text) text)))

(defun swank-send (connection sexp)
  "Send SEXP to the server."
  (unless (connection-open-p connection) (editor-error "Not connected to a Lisp."))
  (let ((octets (encode-message sexp)))
    (when *swank-trace* (funcall *swank-trace* :send (sexp-to-string sexp)))
    (sb-thread:with-mutex ((connection-write-lock connection))
      (write-sequence octets (connection-stream connection))
      (finish-output (connection-stream connection)))))

;;; Connecting

(defun resolve-host (host)
  (if (member host '("localhost" "127.0.0.1") :test #'string-equal)
      #(127 0 0 1)
      (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host))))

(defun swank-connect (host port &key (deliver #'funcall) handler)
  "Connect to the Swank server at HOST:PORT and start reading messages."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (handler-case (sb-bsd-sockets:socket-connect socket (resolve-host host) port)
      (error (e)
        (sb-bsd-sockets:socket-close socket)
        (editor-error "Could not connect to ~a:~d: ~a" host port e)))
    (let ((connection (make-instance 'swank-connection
                                     :socket socket :host host :port port
                                     :stream (sb-bsd-sockets:socket-make-stream
                                              socket :input t :output t
                                                     :element-type '(unsigned-byte 8)
                                                     :buffering :full)
                                     :deliver deliver :handler handler)))
      (setf (connection-reader-thread connection)
            (sb-thread:make-thread (lambda () (reader-loop connection))
                                   :name (format nil "swank reader ~a:~d" host port)))
      connection)))

(defun reader-loop (connection)
  (let ((stream (connection-stream connection)))
    (loop
      (multiple-value-bind (message reason)
          (handler-case (values (read-message stream) nil)
            (end-of-file () (values nil "The Lisp closed the connection."))
            (error (e) (values nil (princ-to-string e))))
        (cond (reason
               (funcall (connection-deliver connection)
                        (lambda () (connection-closed connection reason)))
               (return))
              (t (funcall (connection-deliver connection)
                          (lambda ()
                            ;; A bad message must not stop the reader.
                            (handler-case (dispatch-message connection message)
                              (error (e)
                                (message "Error handling ~a from the Lisp: ~a"
                                         (if (consp message) (first message) message) e)))))))))))

(defun connection-closed (connection reason)
  (unless (eq (connection-state connection) :closed)
    (setf (connection-state connection) :closed)
    (ignore-errors (sb-bsd-sockets:socket-close (connection-socket connection)))
    ;; Requests still waiting get an abort.
    (let ((pending (loop for k being the hash-values of (connection-continuations connection) collect k)))
      (clrhash (connection-continuations connection))
      (dolist (k pending) (funcall (cdr k) reason)))
    (when (connection-handler connection)
      (funcall (connection-handler connection) connection (list :disconnected reason)))))

(defun swank-disconnect (connection)
  "Close CONNECTION."
  (when (connection-open-p connection)
    (ignore-errors (close (connection-stream connection)))
    (connection-closed connection "Disconnected.")))

;;; Requests

(defun default-abort (reason)
  (message "Evaluation aborted~@[: ~a~]" (and reason (princ-to-string reason))))

(defun swank-rex (connection form &key (on-ok #'identity) (on-abort #'default-abort)
                                       (package (connection-package connection)) (thread t))
  "Ask the server to evaluate FORM. Calls ON-OK with the value, or ON-ABORT
with the reason, when the answer arrives. Returns the request's id."
  (let ((id (connection-next-id connection)))
    (incf (connection-next-id connection))
    (setf (gethash id (connection-continuations connection)) (cons on-ok on-abort))
    (swank-send connection (list :emacs-rex form package thread id))
    id))

(defun swank-interrupt (connection &optional (thread :repl-thread))
  "Interrupt THREAD in the other Lisp."
  (swank-send connection (list :emacs-interrupt thread)))

(defun dispatch-message (connection message)
  (destructuring-bind (kind &rest args) message
    (case kind
      (:return
       (destructuring-bind (result id) args
         (let ((k (gethash id (connection-continuations connection))))
           (remhash id (connection-continuations connection))
           (when k
             (ecase (first result)
               (:ok (funcall (car k) (second result)))
               (:abort (funcall (cdr k) (second result))))))))
      (:invalid-rpc
       (destructuring-bind (id reason) args
         (let ((k (gethash id (connection-continuations connection))))
           (remhash id (connection-continuations connection))
           (if k (funcall (cdr k) reason) (message "Invalid request: ~a" reason)))))
      (:ping (destructuring-bind (thread tag) args
               (swank-send connection (list :emacs-pong thread tag))))
      (:eval (destructuring-bind (thread tag &rest form) args
               (declare (ignore form))
               (swank-send connection (list :emacs-return thread tag
                                            (list :abort "Cadre does not evaluate Emacs Lisp.")))))
      (:eval-no-wait nil)
      (:write-string
       ;; (:write-string string target thread): user output from THREAD,
       ;; which waits until we say it has been shown.
       (unwind-protect
            (when (connection-handler connection)
              (funcall (connection-handler connection) connection message))
         (let ((thread (third args)))
           (when thread
             (swank-send connection (list :write-done thread))))))
      (t (when (connection-handler connection)
           (funcall (connection-handler connection) connection message))))))

(defun swank-eval-sync (connection form &key (timeout 30) package (thread t))
  "Evaluate FORM in the other Lisp and wait for its value. For tests and
scripts only: never call it on the thread that handles messages."
  (let ((done (sb-thread:make-semaphore)) (value nil) (failed nil))
    (swank-rex connection form
               :package (or package (connection-package connection)) :thread thread
               :on-ok (lambda (v) (setf value v) (sb-thread:signal-semaphore done))
               :on-abort (lambda (r) (setf failed (or r "aborted")) (sb-thread:signal-semaphore done)))
    (unless (sb-thread:wait-on-semaphore done :timeout timeout)
      (error "Timed out waiting for ~a" (sexp-to-string form)))
    (when failed (error "Swank aborted ~a: ~a" (sexp-to-string form) failed))
    value))

(defun swank-call (name &rest arguments)
  "The form calling the Swank function NAME (a string such as
\"swank:autodoc\") with ARGUMENTS as data: lists and symbols are quoted,
since the other Lisp evaluates the form."
  (cons (remote-symbol name)
        (mapcar (lambda (a) (if (or (consp a) (remote-symbol-p a)) (list 'quote a) a))
                arguments)))

;;; Setting a connection up

(defparameter *swank-contribs*
  '(:swank-repl :swank-arglists :swank-fuzzy :swank-c-p-c :swank-fancy-inspector
    :swank-package-fu :swank-trace-dialog :swank-macrostep :swank-indentation)
  "The contribs Cadre asks the other Lisp to load.")

(defun swank-start-session (connection &key on-ready (on-failure #'default-abort))
  "Ask for the server's details, load Cadre's contribs and create a REPL;
then call ON-READY with the connection."
  (swank-rex connection (list (remote-symbol "swank:connection-info"))
             :on-abort on-failure
             :on-ok (lambda (info)
                      (setf (connection-info connection) info)
                      (let ((package (getf info :package)))
                        (when package
                          (setf (connection-package connection) (getf package :name)
                                (connection-prompt connection) (getf package :prompt))))
                      (swank-rex connection
                                 (list (remote-symbol "swank:swank-require") (list 'quote *swank-contribs*))
                                 :on-abort on-failure
                                 :on-ok (lambda (modules)
                                          (declare (ignore modules))
                                          (swank-rex connection
                                                     (list (remote-symbol "swank-repl:create-repl") nil)
                                                     :on-abort on-failure
                                                     :on-ok (lambda (result)
                                                              (destructuring-bind (package prompt) result
                                                                (setf (connection-package connection) package
                                                                      (connection-prompt connection) prompt))
                                                              (when on-ready (funcall on-ready connection)))))))))
