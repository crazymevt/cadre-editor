;;;; app-server.lisp — Codex's local app-server protocol
;;;;
;;;; The app-server is a long-running JSON-RPC process over newline-delimited
;;;; stdin/stdout. Unlike the Claude CLI driver, this protocol is bidirectional:
;;;; Codex can request client-side dynamic tool calls while a turn is running.

(in-package #:cadre)

(define-option *codex-program* nil (or null string)
  "The Codex CLI. Nil: `codex` on PATH, then the bundled ChatGPT app copy."
  :category "Codex")

(defun find-codex-program ()
  "The Codex CLI executable, or nil."
  (if *codex-program*
      (and (probe-file *codex-program*) *codex-program*)
      (or (ignore-errors
            (let ((path (string-trim '(#\Newline #\Return #\Space #\Tab)
                                     (uiop:run-program '("/bin/sh" "-c" "command -v codex")
                                                       :output :string :error-output nil))))
              (and (plusp (length path)) path)))
          (let ((path (merge-pathnames "Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex"
                                        (user-homedir-pathname))))
            (and (probe-file path) (namestring path))))))

(defstruct (codex-status (:conc-name codex-status-))
  program version logged-in error)

(defun check-codex ()
  "Check whether Codex is installed and has a usable saved login."
  (let ((program (find-codex-program)))
    (if (null program)
        (make-codex-status :error "The Codex CLI (codex) was not found.")
        (handler-case
            (let* ((version (string-trim '(#\Newline #\Return #\Space)
                                         (uiop:run-program (list program "--version") :output :string
                                                           :error-output :output :ignore-error-status t)))
                   (login (uiop:run-program (list program "login" "status") :output :string
                                            :error-output :output :ignore-error-status t)))
              (make-codex-status :program program :version version
                                 :logged-in (and (search "logged in" login :test #'char-equal) t)
                                 :error (unless (search "logged in" login :test #'char-equal)
                                          (string-trim '(#\Newline #\Return #\Space) login))))
          (error (e) (make-codex-status :program program :error (princ-to-string e)))))))

(defstruct (codex-event (:conc-name codex-event-))
  "A normalized Codex app-server event for the Cadre UI."
  kind text name id input error thread-id turn-id data)

(defstruct (codex-pending (:constructor make-codex-pending ()))
  (waitqueue (sb-thread:make-waitqueue :name "codex request"))
  result error (done nil))

(defstruct (codex-app-server (:conc-name codex-))
  process program directory deliver on-event on-exit
  (thread-id nil) (turn-id nil) (stopped nil)
  (next-id 0) (pending (make-hash-table :test 'equal))
  (lock (sb-thread:make-mutex :name "codex app-server"))
  (write-lock (sb-thread:make-mutex :name "codex app-server stdin")))

(defun codex-send-json (server object)
  (sb-thread:with-mutex ((codex-write-lock server))
    (let ((stream (sb-ext:process-input (codex-process server))))
      (unless (and stream (open-stream-p stream))
        (error "Codex app-server input is closed"))
      (write-line (json-string object) stream)
      (force-output stream))))

(defun codex-notify (server method &optional params)
  (codex-send-json server (jobj "jsonrpc" "2.0" "method" method
                                "params" (or params (jobj)))))

(defun codex-request (server method &optional params)
  "Send METHOD and wait for its JSON-RPC result. Call from a worker thread."
  (let* ((id (sb-thread:with-mutex ((codex-lock server))
               (incf (codex-next-id server))))
         (pending (make-codex-pending)))
    (sb-thread:with-mutex ((codex-lock server))
      (setf (gethash id (codex-pending server)) pending))
    (handler-case
        (codex-send-json server (jobj "jsonrpc" "2.0" "id" id "method" method
                                      "params" (or params (jobj))))
      (error (e)
        (sb-thread:with-mutex ((codex-lock server))
          (remhash id (codex-pending server))
          (setf (codex-pending-error pending) e (codex-pending-done pending) t)
          (sb-thread:condition-notify (codex-pending-waitqueue pending)))))
    (sb-thread:with-mutex ((codex-lock server))
      (loop until (codex-pending-done pending)
            do (sb-thread:condition-wait (codex-pending-waitqueue pending) (codex-lock server)))
      (when (codex-pending-error pending) (error "Codex app-server: ~a" (codex-pending-error pending)))
      (when (jget (codex-pending-result pending) "error")
        (error "Codex app-server ~a failed: ~a" method
               (jget (codex-pending-result pending) "error" "message")))
      (jget (codex-pending-result pending) "result"))))

(defun codex-deliver-event (server event)
  (when (codex-on-event server)
    (funcall (codex-deliver server)
             (lambda () (funcall (codex-on-event server) event)))))

(defun codex-finish-pending (server id object)
  (sb-thread:with-mutex ((codex-lock server))
    (let ((pending (gethash id (codex-pending server))))
      (when pending
        (remhash id (codex-pending server))
        (setf (codex-pending-result pending) object (codex-pending-done pending) t)
        (sb-thread:condition-notify (codex-pending-waitqueue pending))))))

(defun codex-dynamic-tool-call (server request)
  (let* ((params (jget request "params"))
         (name (jget params "tool"))
         (arguments (jget params "arguments"))
         (id (jget request "id")))
    (codex-deliver-event server (make-codex-event :kind :tool-start :name name
                                                 :id (jget params "callId")
                                                 :input arguments
                                                 :thread-id (jget params "threadId")
                                                 :turn-id (jget params "turnId")))
    (multiple-value-bind (text error-p) (call-mcp-tool name arguments)
      (codex-send-json server
                       (jobj "jsonrpc" "2.0" "id" id
                             "result" (jobj "success" (if error-p :false t)
                                            "contentItems" (vector (jobj "type" "inputText" "text" text)))))
      (codex-deliver-event server (make-codex-event :kind :tool-result :name name
                                                   :id (jget params "callId") :text text :error error-p
                                                   :thread-id (jget params "threadId")
                                                   :turn-id (jget params "turnId"))))))

(defun codex-server-request (server object)
  (let ((method (jget object "method"))
        (params (jget object "params")))
    (cond
      ((equal method "item/tool/call")
       ;; Tool handlers may wait for inline edit review or runtime approval.
       ;; Keep the protocol reader free to process other responses/events.
       (sb-thread:make-thread
        (lambda ()
          (handler-case (codex-dynamic-tool-call server object)
            (error (e)
              (ignore-errors
                (codex-send-json server
                                 (jobj "jsonrpc" "2.0" "id" (jget object "id")
                                       "error" (jobj "code" -32603
                                                     "message" (princ-to-string e))))))))
        :name "Codex dynamic tool"))
      ;; Native shell and filesystem requests are intentionally denied in v1.
      ;; Codex uses Cadre's propose_edit and approved Lisp tools instead.
      ((member method '("item/commandExecution/requestApproval" "item/fileChange/requestApproval")
               :test #'equal)
       (codex-deliver-event server (make-codex-event :kind :permission-denied :name method :data params))
       (codex-send-json server (jobj "jsonrpc" "2.0" "id" (jget object "id")
                                     "result" (jobj "decision" "decline"))))
      (t
       (codex-send-json server (jobj "jsonrpc" "2.0" "id" (jget object "id")
                                     "error" (jobj "code" -32601 "message" (format nil "Unsupported Codex request: ~a" method))))))))

(defun codex-handle-notification (server object)
  (let* ((method (jget object "method"))
         (params (jget object "params"))
         (thread-id (jget params "threadId"))
         (turn-id (jget params "turnId")))
    (cond
      ((equal method "thread/started")
       (setf (codex-thread-id server) (jget params "thread" "id"))
       (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id (codex-thread-id server))))
      ((equal method "turn/started")
       (setf (codex-turn-id server) (jget params "turn" "id")))
      ((equal method "item/agentMessage/delta")
       (codex-deliver-event server (make-codex-event :kind :text-delta :text (jget params "delta")
                                                     :thread-id thread-id :turn-id turn-id)))
      ((equal method "item/started")
       (let* ((item (jget params "item"))
              (type (jget item "type")))
         (when (member type '("commandExecution" "fileChange" "mcpToolCall" "dynamicToolCall") :test #'equal)
           (codex-deliver-event server (make-codex-event :kind :activity :name type
                                                         :id (jget item "id") :data item
                                                         :thread-id thread-id :turn-id turn-id)))))
      ((equal method "turn/completed")
       (let* ((turn (jget params "turn"))
              (status (jget turn "status")))
         (codex-deliver-event server (make-codex-event :kind :result :error (not (equal status "completed"))
                                                       :text (jget turn "error" "message")
                                                       :turn-id (jget turn "id") :thread-id thread-id
                                                       :data turn))))
      (t nil))))

(defun codex-handle-line (server line)
  (let ((object (ignore-errors (parse-json line))))
    (when (jobject-p object)
      (cond ((jget object "method")
             (if (jget object "id")
                 (codex-server-request server object)
                 (codex-handle-notification server object)))
            ((jget object "id") (codex-finish-pending server (jget object "id") object))))))

(defun codex-fail-pending (server reason)
  (sb-thread:with-mutex ((codex-lock server))
    (maphash (lambda (id pending)
               (declare (ignore id))
               (setf (codex-pending-error pending) reason (codex-pending-done pending) t)
               (sb-thread:condition-notify (codex-pending-waitqueue pending)))
             (codex-pending server))
    (clrhash (codex-pending server))))

(defun start-codex-app-server (program directory &key (deliver #'funcall) on-event on-exit)
  "Start the local Codex app-server in DIRECTORY."
  (let* ((process (sb-ext:run-program program '("app-server" "--listen" "stdio://")
                                      :search t :wait nil :directory (and directory (namestring directory))
                                      :input :stream :output :stream :error :stream
                                      :external-format :utf-8))
         (server (make-codex-app-server :process process :program program :directory directory
                                        :deliver deliver :on-event on-event :on-exit on-exit)))
    (sb-thread:make-thread
     (lambda ()
       (let ((stream (sb-ext:process-output process)))
         (loop for line = (ignore-errors (read-line stream nil))
               while line
               do (handler-case (codex-handle-line server line)
                    (error (e) (codex-deliver-event server (make-codex-event :kind :protocol-error
                                                                            :text (princ-to-string e))))))
         (sb-ext:process-wait process)
         (codex-fail-pending server "the Codex app-server exited")
         (unless (codex-stopped server)
           (funcall deliver (lambda () (when on-exit (funcall on-exit (sb-ext:process-exit-code process))))))))
     :name "codex app-server output")
    (sb-thread:make-thread
     (lambda ()
       (let ((stream (sb-ext:process-error process)))
         (loop for line = (ignore-errors (read-line stream nil))
               while line
               do (funcall deliver (lambda () (when on-event
                                                (funcall on-event (make-codex-event :kind :stderr :text line))))))))
     :name "codex app-server stderr")
    server))

(defun codex-alive-p (server)
  (and server (not (codex-stopped server))
       (sb-ext:process-alive-p (codex-process server))
       (open-stream-p (sb-ext:process-input (codex-process server)))))

(defun codex-dynamic-tools ()
  "The registered Cadre tools as app-server client-side dynamic tools."
  (coerce (loop for tool in (reverse *mcp-tools*)
                unless (equal (mcp-tool-name tool) "approve")
                collect (jobj "type" "function" "name" (mcp-tool-name tool)
                              "description" (mcp-tool-description tool)
                              "inputSchema" (schema-json (mcp-tool-schema tool))))
          'vector))

(defun codex-models-from-response (response)
  "Visible model choices from a `model/list` response."
  (remove-if (lambda (model) (eq (jget model "hidden") t))
             (jlist (jget response "data"))))

(defun codex-list-models (program directory)
  "Return the models advertised by PROGRAM's app-server in DIRECTORY."
  (let ((server (start-codex-app-server program directory)))
    (unwind-protect
         (progn
           (codex-request server "initialize"
                          (jobj "clientInfo" (jobj "name" "cadre" "title" "Cadre" "version" "0.1")))
           (codex-notify server "initialized")
           (codex-models-from-response (codex-request server "model/list" (jobj))))
      (stop-codex-app-server server))))

(defun codex-start-thread (server &key model instructions (sandbox "read-only"))
  "Initialize app-server and start a thread configured with Cadre's tools."
  (codex-request server "initialize"
                 (jobj "clientInfo" (jobj "name" "cadre" "title" "Cadre" "version" "0.1")))
  (codex-notify server "initialized")
  (let* ((result (codex-request server "thread/start"
                                (jobj "cwd" (uiop:native-namestring (codex-directory server))
                                      "model" (or model :null)
                                      "sandbox" sandbox
                                      "approvalPolicy" "never"
                                      "baseInstructions" (or instructions :null)
                                      "dynamicTools" (codex-dynamic-tools))))
         (thread-id (jget result "thread" "id")))
    (unless thread-id (error "Codex app-server did not return a thread id"))
    (setf (codex-thread-id server) thread-id)
    (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id thread-id))
    thread-id))

(defun codex-resume-thread (server thread-id &key instructions (sandbox "read-only"))
  "Initialize app-server and resume THREAD-ID using its saved conversation."
  (codex-request server "initialize"
                 (jobj "clientInfo" (jobj "name" "cadre" "title" "Cadre" "version" "0.1")))
  (codex-notify server "initialized")
  (let ((result (codex-request server "thread/resume"
                               (jobj "threadId" thread-id
                                     "cwd" (uiop:native-namestring (codex-directory server))
                                     "sandbox" sandbox "approvalPolicy" "never"
                                     "baseInstructions" (or instructions :null)))))
    (declare (ignore result))
    (setf (codex-thread-id server) thread-id)
    (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id thread-id))
    thread-id))

(defun codex-start-turn (server text &key model effort)
  "Start a user turn on SERVER's active thread."
  (unless (codex-thread-id server) (error "Codex thread has not been started"))
  (codex-request server "turn/start"
                 (jobj "threadId" (codex-thread-id server)
                       "input" (vector (jobj "type" "text" "text" text))
                       "model" (or model :null)
                       "effort" (or effort :null))))

(defun codex-interrupt (server)
  "Interrupt the active Codex turn."
  (when (and (codex-thread-id server) (codex-turn-id server))
    (codex-request server "turn/interrupt"
                   (jobj "threadId" (codex-thread-id server) "turnId" (codex-turn-id server)))))

(defun stop-codex-app-server (server)
  "Stop SERVER and wake any waiting JSON-RPC requests."
  (when server
    (setf (codex-stopped server) t)
    (codex-fail-pending server "the Codex app-server was stopped")
    (ignore-errors (close (sb-ext:process-input (codex-process server))))
    (when (sb-ext:process-alive-p (codex-process server))
      (sb-ext:process-kill (codex-process server) 15))))
