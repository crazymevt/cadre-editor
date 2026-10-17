(in-package #:cadre-tests)

;;; M4: JSON, the CLI's stream-json, the MCP server, diffs

(define-test json :parent cadre-tests
  (let ((v (c:parse-json "{\"a\": [1, 2.5, -3e2, true, false, null], \"b\": {\"c\": \"x\\ny\\u00e9\\ud83d\\ude00\"}, \"d\": []}")))
    (is equalp #(1 2.5d0 -300.0d0 t :false :null) (c:jget v "a"))
    (is string= (format nil "x~%yé~a" (code-char #x1F600)) (c:jget v "b" "c"))
    (is = 2.5d0 (c:jget v "a" 1))
    (is eq nil (c:jget v "missing" "deeper"))
    (is equalp #() (c:jget v "d")))
  (is string= "{\"k\":\"a\\\"b\\n\",\"n\":[1,true,false,null],\"o\":{}}"
      (c:json-string (c:jobj "k" (format nil "a\"b~%") "n" (vector 1 t :false nil) "o" (c:jobj) "skip" :omit)))
  (let ((round (c:parse-json (c:json-string (c:jobj "x" (vector (c:jobj "y" "é")))))))
    (is string= "é" (c:jget round "x" 0 "y")))
  (fail (c:parse-json "{\"a\": }") 'c:json-error)
  (fail (c:parse-json "[1, 2") 'c:json-error)
  (fail (c:parse-json "1 2") 'c:json-error)
  (fail (c:parse-json "[-]") 'c:json-error)
  (fail (c:parse-json "1-2") 'c:json-error))

(define-test claude-events :parent cadre-tests
  (let ((e (first (c:parse-claude-event "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"s1\",\"model\":\"claude-sonnet\",\"mcp_servers\":[{\"name\":\"cadre\",\"status\":\"connected\"}]}"))))
    (is eq :init (c:event-kind e))
    (is string= "s1" (c:event-session-id e))
    (is equal '(("cadre" . "connected")) (c:event-data e)))
  (let ((e (first (c:parse-claude-event "{\"type\":\"stream_event\",\"event\":{\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"Hel\"}},\"parent_tool_use_id\":null}"))))
    (is eq :text-delta (c:event-kind e))
    (is string= "Hel" (c:event-text e))
    (false (c:event-subagent e)))
  (let ((events (c:parse-claude-event "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hi\"},{\"type\":\"tool_use\",\"id\":\"t1\",\"name\":\"mcp__cadre__read_buffer\",\"input\":{\"buffer\":\"a.lisp\"}}]}}")))
    (is equal '(:text :tool-use) (mapcar #'c:event-kind events))
    (is string= "a.lisp" (c:jget (c:event-input (second events)) "buffer"))
    (is string= "read_buffer" (c:tool-display-name (c:event-name (second events)))))
  (let ((e (first (c:parse-claude-event "{\"type\":\"user\",\"message\":{\"content\":[{\"type\":\"tool_result\",\"tool_use_id\":\"t1\",\"content\":[{\"type\":\"text\",\"text\":\"ok\"}],\"is_error\":true}]}}"))))
    (is eq :tool-result (c:event-kind e))
    (is string= "ok" (c:event-text e))
    (true (c:event-error e)))
  (let ((e (first (c:parse-claude-event "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"result\":\"Done\",\"total_cost_usd\":0.0123,\"duration_ms\":900,\"session_id\":\"s1\"}"))))
    (is eq :result (c:event-kind e))
    (false (c:event-error e))
    (is = 0.0123d0 (c:event-cost e)))
  (is eq nil (c:parse-claude-event "not json"))
  (let ((line (c:parse-json (c:user-message-line "fix it"))))
    (is string= "user" (c:jget line "type"))
    (is string= "fix it" (c:jget line "message" "content" 0 "text")))
  (let ((args (c:claude-arguments :session-id "u" :model "sonnet" :allowed-tools '("Read" "Grep")
                                  :disallowed-tools '("Edit") :permission-tool "mcp__cadre__approve")))
    (true (search '("--session-id" "u") args :test #'equal))
    (true (search '("--allowedTools" "Read,Grep") args :test #'equal))
    (true (search '("--permission-prompt-tool" "mcp__cadre__approve") args :test #'equal)))
  (true (search '("--resume" "u") (c:claude-arguments :resume "u" :session-id "v") :test #'equal))
  (is = 36 (length (c:make-uuid)))
  (is char= #\4 (char (c:make-uuid) 14)))

(define-test codex-model-catalog :parent cadre-tests
  (let ((models (c:codex-models-from-response
                 (c:parse-json
                  "{\"data\":[{\"id\":\"gpt-visible\",\"model\":\"gpt-visible\",\"displayName\":\"Visible\",\"hidden\":false,\"isDefault\":true,\"defaultReasoningEffort\":\"medium\",\"supportedReasoningEfforts\":[{\"reasoningEffort\":\"medium\",\"description\":\"Balanced\"}]},{\"id\":\"gpt-hidden\",\"hidden\":true}]}"))))
    (is = 1 (length models))
    (is string= "gpt-visible" (c:jget (first models) "id"))
    (is string= "Visible" (c:jget (first models) "displayName"))
    (is string= "medium" (c:jget (first models) "defaultReasoningEffort"))))

(define-test codex-login-and-utf8 :parent cadre-tests
  (true (c::codex-login-output-logged-in-p "Logged in using ChatGPT"))
  (false (c::codex-login-output-logged-in-p "Not logged in"))
  (false (c::codex-login-output-logged-in-p "Authentication required; not authenticated"))
  (let ((c:*codex-program* (namestring (truename "scripts/fake-codex"))))
    (let ((status (c:check-codex)))
      (true (c:codex-status-logged-in status))
      (true (search "0.0.0-fake" (c:codex-status-version status))))))

(define-test codex-unicode-streams :parent cadre-tests
  (let ((server (c:start-codex-app-server (namestring (truename "scripts/fake-codex.py"))
                                          (truename "."))))
    (unwind-protect
         (progn
           (c::codex-initialize-app-server server)
           (is string= (format nil "Café ~a λ" (code-char #x1F680))
               (c::codex-request server "test/unicode"
                                 (c:jobj "text" (format nil "Café ~a λ" (code-char #x1F680))))))
      (c:stop-codex-app-server server))))

(define-test codex-exit-with-inherited-stderr :parent cadre-tests
  (let* ((exited (sb-thread:make-semaphore))
         (exit-code nil)
         (child-pid nil)
         (server (c:start-codex-app-server
                  (namestring (truename "scripts/fake-codex.py")) (truename ".")
                  :on-exit (lambda (code)
                             (setf exit-code code)
                             (sb-thread:signal-semaphore exited)))))
    (unwind-protect
         (progn
           (setf child-pid (c::codex-request server "test/exit-with-inherited-stderr"))
           (true (sb-thread:wait-on-semaphore exited :timeout 3))
           (is eql 0 exit-code)
           (sb-thread:join-thread (c::codex-stdout-thread server) :timeout 3 :default nil)
           (false (sb-thread:thread-alive-p (c::codex-stdout-thread server)))
           (false (sb-thread:thread-alive-p (c::codex-stderr-thread server)))
           (false (open-stream-p (sb-ext:process-error (c::codex-process server))))
           (false (open-stream-p (sb-ext:process-output (c::codex-process server)))))
      (when child-pid (ignore-errors (sb-posix:kill child-pid 15)))
      (c:stop-codex-app-server server))))

(define-test codex-activity-completion :parent cadre-tests
  (let* ((events '())
         (server (c::make-codex-app-server :deliver #'funcall :on-event (lambda (event) (push event events)))))
    (c::codex-handle-notification
     server (c:parse-json
             "{\"method\":\"item/completed\",\"params\":{\"threadId\":\"t1\",\"turnId\":\"u1\",\"item\":{\"id\":\"i1\",\"type\":\"commandExecution\",\"status\":\"failed\",\"exitCode\":1}}}"))
    (let ((event (first events)))
      (is eq :activity-completed (c:codex-event-kind event))
      (is string= "i1" (c:codex-event-id event))
      (true (c:codex-event-error event))
      (is string= "u1" (c:codex-event-turn-id event)))))

(define-test codex-request-timeout :parent cadre-tests
  (let ((now #x10000000))
    (is eql (+ now (round (* 0.1 c::internal-time-units-per-second)))
        (c::codex-request-deadline 0.1 now)))
  (let ((server (c:start-codex-app-server (namestring (truename "scripts/fake-codex.py"))
                                          (truename "."))))
    (unwind-protect
         (let ((c::*codex-request-timeout* 0.1))
           (fail (c::codex-request server "test/hang") 'error))
      (c:stop-codex-app-server server))))

(c:define-mcp-tool "test_echo" (args)
  "Echo the text."
  (("text" "string" "What to echo" :required t))
  (format nil "echo: ~a" (c:tool-argument args "text" :required t)))

(define-test mcp-server :parent cadre-tests
  (let* ((server (c:start-mcp-server))
         (url (c:mcp-server-url server))
         (token (c:mcp-server-token server)))
    (unwind-protect
         (progn
           (multiple-value-bind (status reply)
               (c:http-post-json url token (c:jobj "jsonrpc" "2.0" "id" 1 "method" "initialize"
                                                   "params" (c:jobj "protocolVersion" "2025-03-26")))
             (is = 200 status)
             (is string= "2025-03-26" (c:jget reply "result" "protocolVersion"))
             (is string= "cadre" (c:jget reply "result" "serverInfo" "name")))
           (is = 202 (c:http-post-json url token (c:jobj "jsonrpc" "2.0" "method" "notifications/initialized")))
           (multiple-value-bind (status reply)
               (c:http-post-json url token (c:jobj "jsonrpc" "2.0" "id" 2 "method" "tools/list"))
             (is = 200 status)
             (let ((tool (find "test_echo" (c:jget reply "result" "tools") :key (lambda (x) (c:jget x "name")) :test #'equal)))
               (true tool)
               (is equalp #("text") (c:jget tool "inputSchema" "required"))))
           (multiple-value-bind (status reply)
               (c:http-post-json url token (c:jobj "jsonrpc" "2.0" "id" 3 "method" "tools/call"
                                                   "params" (c:jobj "name" "test_echo" "arguments" (c:jobj "text" "hé"))))
             (is = 200 status)
             (is string= "echo: hé" (c:jget reply "result" "content" 0 "text"))
             (is eq :false (c:jget reply "result" "isError")))
           (let ((reply (nth-value 1 (c:http-post-json url token (c:jobj "jsonrpc" "2.0" "id" 4 "method" "tools/call"
                                                                         "params" (c:jobj "name" "test_echo" "arguments" (c:jobj)))))))
             (is eq t (c:jget reply "result" "isError"))
             (true (search "Missing argument" (c:jget reply "result" "content" 0 "text"))))
           (is = 401 (c:http-post-json url "wrong" (c:jobj "jsonrpc" "2.0" "id" 5 "method" "ping")))
           ;; A huge Content-Length isn't read: without the token it's never
           ;; looked at, and with it the request is refused.
           (flet ((raw-status (authorization length)
                    (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                      (sb-bsd-sockets:socket-connect socket #(127 0 0 1) (c:mcp-server-port server))
                      (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t :timeout 10
                                                                              :external-format :latin-1)))
                        (unwind-protect
                             (progn
                               (format stream "POST /mcp HTTP/1.1~c~cAuthorization: Bearer ~a~c~cContent-Length: ~a~c~c~c~c"
                                       #\Return #\Newline authorization #\Return #\Newline length
                                       #\Return #\Newline #\Return #\Newline)
                               (finish-output stream)
                               (parse-integer (second (uiop:split-string (read-line stream) :separator " "))))
                          (close stream))))))
             (is = 401 (raw-status "wrong" 4000000000))
             (is = 400 (raw-status token 4000000000))
             (is = 400 (raw-status token -5)))
           (let* ((dir (merge-pathnames (format nil "cadre-mcp-test-~d/" (random 100000)) (uiop:temporary-directory)))
                  (path (c:write-mcp-config server dir)))
             (unwind-protect
                  (let ((config (c:parse-json (uiop:read-file-string path))))
                    (is string= url (c:jget config "mcpServers" "cadre" "url"))
                    (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring path))))))
               (uiop:delete-directory-tree dir :validate t))))
      (c:stop-mcp-server server))))

(define-test diffs :parent cadre-tests
  (is equal '((:same "a") (:removed "b") (:added "B") (:same "c"))
      (c:diff-lines '("a" "b" "c") '("a" "B" "c")))
  (is equal '((:same "a") (:added "x") (:same "b"))
      (c:diff-lines '("a" "b") '("a" "x" "b")))
  (is equal '((:removed "a"))
      (c:diff-lines '("a") '()))
  (is equal '("a" "b") (c:split-lines (format nil "a~%b~%")))
  (is string= "one TWO three" (c:replace-unique "one two three" "two" "TWO"))
  (fail (c:replace-unique "a a" "a" "b") 'c:mcp-tool-error)
  (fail (c:replace-unique "abc" "x" "y") 'c:mcp-tool-error))
