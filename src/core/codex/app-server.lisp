;;;; app-server.lisp — Codex's local app-server protocol
;;;;
;;;; The app-server is a long-running JSON-RPC process over newline-delimited
;;;; stdin/stdout. Unlike the Claude CLI driver, this protocol is bidirectional:
;;;; Codex can request client-side dynamic tool calls while a turn is running.

(in-package #:cadre)

(define-option *codex-program* nil (or null string)
  "The Codex CLI. Nil: `codex` on PATH, then the bundled ChatGPT app copy."
  :category "Codex")

(define-option *codex-model* nil (or null string)
  "The default Codex model ID. Nil uses the model Codex marks as its default."
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
          (some (lambda (path)
                  (and (probe-file path) (namestring path)))
                (list #P"/Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex"
                      (merge-pathnames "Applications/ChatGPT.app/Contents/Resources/codex-cli/CodexCLI.app/Contents/MacOS/codex"
                                       (user-homedir-pathname)))))))

(defstruct (codex-status (:conc-name codex-status-))
  program version logged-in error)

(defun codex-login-output-logged-in-p (output)
  "Whether CODEX LOGIN STATUS output positively indicates an active login."
  (and (search "logged in" output :test #'char-equal)
       (not (some (lambda (phrase) (search phrase output :test #'char-equal))
                  '("not logged in" "not authenticated" "authentication required")))))

(defun check-codex ()
  "Check whether Codex is installed and has a usable saved login."
  (let ((program (find-codex-program)))
    (if (null program)
        (make-codex-status :error "The Codex CLI (codex) was not found.")
        (handler-case
            (let* ((version (string-trim '(#\Newline #\Return #\Space)
                                         (uiop:run-program (list program "--version") :output :string
                                                           :error-output :output :ignore-error-status t)))
                   (login nil)
                   (login-exit-code nil))
              (multiple-value-bind (output error-output exit-code)
                  (uiop:run-program (list program "login" "status") :output :string
                                    :error-output :output :ignore-error-status t)
                (declare (ignore error-output))
                (setf login output login-exit-code exit-code))
              (make-codex-status :program program :version version
                                 :logged-in (and (zerop login-exit-code)
                                                 (codex-login-output-logged-in-p login))
                                 :error (unless (and (zerop login-exit-code)
                                                     (codex-login-output-logged-in-p login))
                                          (string-trim '(#\Newline #\Return #\Space) login))))
          (error (e) (make-codex-status :program program :error (princ-to-string e)))))))

(defstruct (codex-event (:conc-name codex-event-))
  "A normalized Codex app-server event for the Cadre UI."
  kind text name id input error thread-id turn-id data)

(defstruct (codex-pending (:constructor make-codex-pending ()))
  (waitqueue (sb-thread:make-waitqueue :name "codex request"))
  result error (done nil))

(defparameter *codex-request-timeout* 60
  "Maximum seconds to wait for an app-server JSON-RPC response.")

(defun codex-request-deadline (timeout &optional (now (get-internal-real-time)))
  (+ now (round (* timeout internal-time-units-per-second))))

(defstruct (codex-app-server (:conc-name codex-))
  process program directory deliver on-event on-exit stdout-thread stderr-thread
  (thread-id nil) (turn-id nil) (stopped nil) (initialized nil)
  (tool-token (list t)) (tool-turns (make-hash-table :test 'equal))
  (next-id 0) (pending (make-hash-table :test 'equal))
  (lock (sb-thread:make-mutex :name "codex app-server"))
  (write-lock (sb-thread:make-mutex :name "codex app-server stdin")))

(defvar *tool-call-active-p* nil
  "Optional cancellation predicate for the current client-side tool call.")

(defun ensure-tool-call-active ()
  (when (and *tool-call-active-p* (not (funcall *tool-call-active-p*)))
    (tool-error "The assistant turn was cancelled.")))

(defun codex-cancel-tools (server)
  "Invalidate this turn's tool token, including callbacks already queued."
  (setf (car (codex-tool-token server)) nil))

(defun codex-prepare-tools (server)
  "Allocate a token before scheduling a new send, so Stop can cancel startup."
  (codex-cancel-tools server)
  (setf (codex-tool-token server) (list t)))

(defun codex-tool-request-token (server request)
  (or (gethash (jget request "params" "turnId") (codex-tool-turns server))
      (setf (gethash (jget request "params" "turnId") (codex-tool-turns server))
            (codex-tool-token server))))

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
  "Send METHOD and wait at most *CODEX-REQUEST-TIMEOUT* seconds for its result."
  (let* ((id (sb-thread:with-mutex ((codex-lock server))
               (incf (codex-next-id server))))
         (pending (make-codex-pending))
         (deadline (codex-request-deadline *codex-request-timeout*)))
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
    ;; Poll briefly instead of waiting on the condition queue with a timeout:
    ;; SBCL may return from a timed condition-wait without holding the mutex.
    (loop
      (when (sb-thread:with-mutex ((codex-lock server)) (codex-pending-done pending))
        (return))
      (when (>= (get-internal-real-time) deadline)
        (sb-thread:with-mutex ((codex-lock server))
          (unless (codex-pending-done pending)
            (remhash id (codex-pending server))
            (setf (codex-pending-error pending)
                  (format nil "request timed out after ~d seconds" *codex-request-timeout*)
                  (codex-pending-done pending) t)))
        (return))
      (sleep 0.01))
    (sb-thread:with-mutex ((codex-lock server))
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

(defun codex-dynamic-tool-call (server request &optional (token (codex-tool-request-token server request)))
  (let* ((params (jget request "params"))
         (*tool-call-active-p* (lambda () (and (car token) (not (codex-stopped server)))))
         (name (jget params "tool"))
         (arguments (jget params "arguments"))
         (id (jget request "id")))
    (codex-deliver-event server (make-codex-event :kind :tool-start :name name
                                                 :id (jget params "callId")
                                                 :input arguments
                                                 :thread-id (jget params "threadId")
                                                 :turn-id (jget params "turnId")))
    (handler-case
        (multiple-value-bind (text error-p)
            (progn (ensure-tool-call-active) (call-mcp-tool name arguments))
          (codex-send-json server
                           (jobj "jsonrpc" "2.0" "id" id
                                 "result" (jobj "success" (if error-p :false t)
                                                "contentItems" (vector (jobj "type" "inputText" "text" text)))))
          (codex-deliver-event server (make-codex-event :kind :tool-result :name name
                                                       :id (jget params "callId") :text text :error error-p
                                                       :thread-id (jget params "threadId")
                                                       :turn-id (jget params "turnId"))))
      (error (e)
        (let ((text (princ-to-string e)))
          (ignore-errors
            (codex-send-json server
                             (jobj "jsonrpc" "2.0" "id" id
                                   "error" (jobj "code" -32603 "message" text))))
          (codex-deliver-event server (make-codex-event :kind :tool-result :name name
                                                       :id (jget params "callId") :text text :error t
                                                       :thread-id (jget params "threadId")
                                                       :turn-id (jget params "turnId"))))))))

(defun codex-server-request (server object)
  (let ((method (jget object "method"))
        (params (jget object "params")))
    (cond
      ((equal method "item/tool/call")
       ;; Tool handlers may wait for inline edit review or runtime approval.
       ;; Keep the protocol reader free to process other responses/events.
       (let ((token (codex-tool-request-token server object)))
         (sb-thread:make-thread
          (lambda ()
            (handler-case (codex-dynamic-tool-call server object token)
              (error (e)
                (ignore-errors
                  (codex-send-json server
                                   (jobj "jsonrpc" "2.0" "id" (jget object "id")
                                         "error" (jobj "code" -32603
                                                       "message" (princ-to-string e))))))))
          :name "Codex dynamic tool")))
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
       (setf (codex-turn-id server) (jget params "turn" "id")
             (gethash (codex-turn-id server) (codex-tool-turns server)) (codex-tool-token server)))
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
      ((equal method "item/completed")
       (let* ((item (jget params "item"))
              (status (jget item "status")))
         (when (jget item "id")
           (codex-deliver-event server (make-codex-event :kind :activity-completed
                                                         :id (jget item "id") :data item
                                                         :error (member status '("failed" "declined" "cancelled")
                                                                           :test #'equal)
                                                         :thread-id thread-id :turn-id turn-id)))))
      ((equal method "turn/completed")
       (let* ((turn (jget params "turn"))
              (status (jget turn "status")))
         (let ((token (gethash (jget turn "id") (codex-tool-turns server))))
           (when token (setf (car token) nil)))
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
                                      :external-format '(:utf-8 :replacement #\?)))
         (server (make-codex-app-server :process process :program program :directory directory
                                        :deliver deliver :on-event on-event :on-exit on-exit)))
    ;; UTF-8 preserves Unicode in both directions. Replace malformed sequences
    ;; so a bad diagnostic cannot kill the RPC reader or stop stderr draining.
    (setf (codex-stderr-thread server)
          (sb-thread:make-thread
           (lambda ()
             (let ((stream (sb-ext:process-error process)))
               (loop for line = (ignore-errors (read-line stream nil))
                     while line
                     do (ignore-errors
                          (funcall deliver
                                   (lambda () (when on-event
                                                (funcall on-event
                                                         (make-codex-event :kind :stderr
                                                                           :text line)))))))))
           :name "codex app-server stderr"))
    (setf (codex-stdout-thread server)
          (sb-thread:make-thread
           (lambda ()
             (let ((stream (sb-ext:process-output process)))
               (loop for line = (ignore-errors (read-line stream nil))
                     while line
                     do (handler-case (codex-handle-line server line)
                          (error (e)
                            (codex-deliver-event server (make-codex-event :kind :protocol-error
                                                                          :text (princ-to-string e)))))))
             (sb-ext:process-wait process)
             (codex-cancel-tools server)
             (codex-fail-pending server "the Codex app-server exited")
             (let ((exit-code (sb-ext:process-exit-code process)))
               (unwind-protect
                    ;; Notify before draining stderr: descendants may still
                    ;; hold that pipe open after the app-server has exited.
                    (unless (codex-stopped server)
                      (funcall deliver (lambda () (when on-exit (funcall on-exit exit-code)))))
                 (let ((stderr-thread (codex-stderr-thread server)))
                   (when (and stderr-thread (not (eq stderr-thread sb-thread:*current-thread*)))
                     (ignore-errors (sb-thread:join-thread stderr-thread :timeout 0.25 :default nil))
                     (when (sb-thread:thread-alive-p stderr-thread)
                       ;; This dedicated reader owns no application state.
                       ;; Release a blocked read before closing its stream.
                       (sb-thread:terminate-thread stderr-thread)
                       (ignore-errors (sb-thread:join-thread stderr-thread :timeout 0.25 :default nil)))))
                 (ignore-errors (sb-ext:process-close process)))))
           :name "codex app-server output"))
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

(defun codex-rate-limits-from-response (response)
  "Return rate-limit buckets from an `account/rateLimits/read` response."
  (let ((buckets (jget response "rateLimitsByLimitId")))
    (cond ((and (jobject-p buckets) (cdr buckets)) (cdr buckets))
          ((jget response "rateLimits")
           (list (cons (or (jget (jget response "rateLimits") "limitId") "codex")
                       (jget response "rateLimits"))))
          (t nil))))

(defun codex-read-rate-limits (program directory)
  "Read the signed-in account's Codex rate limits using app-server."
  (let ((server (start-codex-app-server program directory)))
    (unwind-protect
         (progn
           ;; Request the extended protocol surface for the account read.
           (codex-initialize-app-server server :experimental-api t)
           (codex-rate-limits-from-response
            (codex-request server "account/rateLimits/read" (jobj))))
      (stop-codex-app-server server))))

(defun codex-initialize-app-server (server &key experimental-api)
  "Initialize SERVER once, optionally enabling the experimental tool bridge."
  (unless (codex-initialized server)
    (codex-request server "initialize"
                   (jobj "clientInfo" (jobj "name" "cadre" "title" "Cadre" "version" "0.1")
                         "capabilities" (if experimental-api (jobj "experimentalApi" t) (jobj))))
    (codex-notify server "initialized")
    (setf (codex-initialized server) t))
  server)

(defun codex-list-models (program directory)
  "Return the models advertised by PROGRAM's app-server in DIRECTORY."
  (let ((server (start-codex-app-server program directory)))
    (unwind-protect
         (progn
           (codex-initialize-app-server server)
           (codex-models-from-response (codex-request server "model/list" (jobj))))
      (stop-codex-app-server server))))

(defun codex-start-thread (server &key model instructions (sandbox "read-only"))
  "Initialize app-server and start a thread configured with Cadre's tools."
  (codex-initialize-app-server server :experimental-api t)
  (let* ((result (codex-request server "thread/start"
                                (jobj "cwd" (uiop:native-namestring (codex-directory server))
                                      "model" (or model :null)
                                      "sandbox" sandbox
                                      "approvalPolicy" "on-request"
                                      "baseInstructions" (or instructions :null)
                                      "dynamicTools" (codex-dynamic-tools))))
         (thread-id (jget result "thread" "id")))
    (unless thread-id (error "Codex app-server did not return a thread id"))
    (setf (codex-thread-id server) thread-id)
    (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id thread-id))
    thread-id))

(defun codex-resume-thread (server thread-id &key instructions (sandbox "read-only"))
  "Initialize app-server and resume THREAD-ID using its saved conversation."
  (codex-initialize-app-server server :experimental-api t)
  ;; The app-server restores this thread's persisted dynamic tool catalog.
  ;; Unlike thread/start, thread/resume does not accept a dynamicTools field.
  (let ((result (codex-request server "thread/resume"
                               (jobj "threadId" thread-id
                                     "cwd" (uiop:native-namestring (codex-directory server))
                                     "sandbox" sandbox "approvalPolicy" "on-request"
                                     "baseInstructions" (or instructions :null)))))
    (declare (ignore result))
    (setf (codex-thread-id server) thread-id)
    (codex-deliver-event server (make-codex-event :kind :thread-started :thread-id thread-id))
    thread-id))

(defun codex-start-turn (server text &key model effort (tool-token (codex-prepare-tools server)))
  "Start a user turn on SERVER's active thread."
  (unless (codex-thread-id server) (error "Codex thread has not been started"))
  (unless (car tool-token) (error "The Codex send was cancelled"))
  (codex-request server "turn/start"
                 (jobj "threadId" (codex-thread-id server)
                       "input" (vector (jobj "type" "text" "text" text))
                       "model" (or model :null)
                       "effort" (or effort :null))))

(defun codex-interrupt (server &key (thread-id (codex-thread-id server))
                                   (turn-id (codex-turn-id server)))
  "Interrupt the specified Codex turn, defaulting to the active turn."
  (when (and thread-id turn-id)
    (codex-request server "turn/interrupt"
                   (jobj "threadId" thread-id "turnId" turn-id))))

(defun stop-codex-app-server (server)
  "Stop SERVER and wake any waiting JSON-RPC requests."
  (when server
    (setf (codex-stopped server) t)
    (codex-cancel-tools server)
    (codex-fail-pending server "the Codex app-server was stopped")
    (ignore-errors (close (sb-ext:process-input (codex-process server))))
    (when (sb-ext:process-alive-p (codex-process server))
      (sb-ext:process-kill (codex-process server) 15))))
