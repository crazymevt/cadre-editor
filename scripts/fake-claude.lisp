;;;; fake-claude.lisp — a stand-in for the Claude Code CLI, for `make smoke`
;;;;
;;;; Answers --version and `auth status`, and otherwise holds a stream-json
;;;; conversation: for each user message it plays a scripted turn chosen by
;;;; the message's words, calling Cadre's MCP tools over HTTP as the real
;;;; CLI would, and printing the same kinds of events.

(require :asdf)
(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (push (merge-pathnames "../" (directory-namestring *load-truename*)) asdf:*central-registry*)
  (asdf:load-system "cadre/core"))

(defpackage #:fake-claude (:use #:cl #:cadre))
(in-package #:fake-claude)

(defvar *args* (rest sb-ext:*posix-argv*))
(defvar *session* (or (second (member "--session-id" *args* :test #'string=))
                      (second (member "--resume" *args* :test #'string=))
                      "fake-session"))
(defvar *mcp* (let ((path (second (member "--mcp-config" *args* :test #'string=))))
                (and path (jget (parse-json (uiop:read-file-string path)) "mcpServers" "cadre"))))
(defvar *ids* 0)

(defun emit (json)
  (write-line (json-string json))
  (finish-output))

(defun mcp (method &optional params)
  (multiple-value-bind (status reply)
      (http-post-json (jget *mcp* "url")
                      (subseq (jget *mcp* "headers" "Authorization") 7)
                      (jobj "jsonrpc" "2.0" "id" (incf *ids*) "method" method "params" (or params (jobj))))
    (declare (ignore status))
    reply))

(defun say (text)
  (emit (jobj "type" "stream_event" "event" (jobj "type" "message_start") "parent_tool_use_id" :null))
  (loop for start from 0 below (length text) by 12
        do (emit (jobj "type" "stream_event" "parent_tool_use_id" :null
                       "event" (jobj "type" "content_block_delta" "index" 0
                                     "delta" (jobj "type" "text_delta" "text" (subseq text start (min (length text) (+ start 12))))))))
  (emit (jobj "type" "assistant" "parent_tool_use_id" :null
              "message" (jobj "role" "assistant" "content" (vector (jobj "type" "text" "text" text))))))

(defun use-tool (name arguments)
  "Call Cadre's tool NAME, reporting it as the CLI does; return its text."
  (let ((id (format nil "toolu_~d" (incf *ids*))))
    (emit (jobj "type" "assistant" "parent_tool_use_id" :null
                "message" (jobj "role" "assistant"
                                "content" (vector (jobj "type" "tool_use" "id" id
                                                        "name" (format nil "mcp__cadre__~a" name)
                                                        "input" arguments)))))
    (let* ((reply (mcp "tools/call" (jobj "name" name "arguments" arguments)))
           (text (jget reply "result" "content" 0 "text"))
           (error-p (jtrue-p (jget reply "result" "isError"))))
      (emit (jobj "type" "user" "parent_tool_use_id" :null
                  "message" (jobj "role" "user"
                                  "content" (vector (jobj "type" "tool_result" "tool_use_id" id
                                                          "content" (or text "") "is_error" (if error-p t :false))))))
      text)))

(defun finish (text)
  (emit (jobj "type" "result" "subtype" "success" "is_error" :false "result" text
              "session_id" *session* "total_cost_usd" 0.0042 "duration_ms" 1200)))

(defun turn (message)
  (cond
    ((search "Fix the compiler problems" message)
     (let ((problems (use-tool "get_problems" (jobj))))
       (use-tool "read_buffer" (jobj "buffer" "src/m2.lisp"))
       (say (format nil "I see the problem: ~a" (if (search "UNDEFINED-THING" (string-upcase problems)) "an undefined variable." "none?")))
       (let ((result (use-tool "propose_edit" (jobj "file" "src/m2.lisp" "old_text" "(+ y undefined-thing)"
                                                    "new_text" "(+ y 1)" "explanation" "Use 1 instead of an undefined variable"))))
         (say (format nil "Result: ~a" result))
         (finish result))))
    ((search "Change this code in " message)
     ;; Claude edit: put the instruction above the code as a comment.
     (let* ((start (+ (search "Change this code in " message) 20))
            (file (subseq message start (search " (lines" message :start2 start)))
            (instruction (subseq message (+ 3 (search "): " message)) (position #\Newline message :start (search "): " message))))
            (open (+ 4 (search (format nil "```~%") message)))
            (code (subseq message open (search (format nil "~%```") message :start2 open)))
            (result (use-tool "propose_edit" (jobj "file" file "old_text" code
                                                   "new_text" (format nil ";; ~a~%~a" instruction code)))))
       (say (format nil "Result: ~a" result))
       (finish result)))
    ((search "Try another edit" message)
     (let ((result (use-tool "propose_edit" (jobj "file" "src/m2.lisp" "old_text" "(* 2 x)" "new_text" "(+ x x)"))))
       (say (format nil "Result: ~a" result))
       (finish result)))
    ((search "permission" message)
     (let ((answer (use-tool "approve" (jobj "tool_name" "Bash" "input" (jobj "command" "ls -la")))))
       (say (format nil "Permission: ~a" (jget (parse-json answer) "behavior")))
       (finish "done")))
    ((search "agent check" message)
     (let ((prompt (or (second (member "--append-system-prompt" *args* :test #'string=)) ""))
           (allowed (or (second (member "--allowedTools" *args* :test #'string=)) "")))
       (say (format nil "Mode: agent=~:[no~;yes~] todo=~:[no~;yes~] model=~a"
                    (search "agent mode" prompt) (search "TodoWrite" allowed)
                    (second (member "--model" *args* :test #'string=))))
       (finish "done")))
    ((search "make a plan" message)
     (emit (jobj "type" "assistant" "parent_tool_use_id" :null
                 "message" (jobj "role" "assistant"
                                 "content" (vector (jobj "type" "tool_use" "id" "toolu_plan" "name" "TodoWrite"
                                                         "input" (jobj "todos" (vector (jobj "content" "Read area" "status" "completed" "activeForm" "Reading area")
                                                                                       (jobj "content" "Compile area" "status" "in_progress" "activeForm" "Compiling area")
                                                                                       (jobj "content" "Summarize" "status" "pending" "activeForm" "Summarizing"))))))))
     (use-tool "open_file" (jobj "file" "src/m1.lisp" "line" 2))
     (say (format nil "Compiled: ~a" (use-tool "compile_defun" (jobj "buffer" "src/m1.lisp" "line" 2))))
     (finish "done"))
    ((search "context" message)
     (say (format nil "Context: ~a" (use-tool "current_context" (jobj))))
     (finish "done"))
    (t
     (say (format nil "Hello from fake Claude. Here is some **code**:~%~%```lisp~%(defun hi () :hi)~%```~%Done."))
     (finish "hello"))))

(defun signed-out-marker ()
  "If FAKE_CLAUDE_SIGNED_OUT names a file that exists, we are signed out."
  (let ((path (uiop:getenv "FAKE_CLAUDE_SIGNED_OUT")))
    (and path (plusp (length path)) path)))

(cond
  ((member "--version" *args* :test #'string=) (write-line "9.9.9 (Claude Code)"))
  ((and (member "auth" *args* :test #'string=) (member "status" *args* :test #'string=))
   (if (and (signed-out-marker) (probe-file (signed-out-marker)))
       (write-line "{\"loggedIn\": false, \"authMethod\": \"none\"}")
       (write-line "{\"loggedIn\": true, \"authMethod\": \"fake\"}")))
  ((and (member "auth" *args* :test #'string=) (member "login" *args* :test #'string=))
   (write-line "Opening browser to sign in…")
   (write-line "If the browser didn't open, visit: https://example.invalid/fake-sign-in?code=true")
   (write-string "Paste code here if prompted > ")
   (finish-output)
   (let ((code (read-line *standard-input* nil "")))
     (cond ((string= (string-trim " " code) "good-code")
            (when (signed-out-marker) (ignore-errors (delete-file (signed-out-marker))))
            (write-line "Login successful.")
            (finish-output))
           (t (write-line "Invalid code. Please make sure the full code was copied.")
              (finish-output)
              (uiop:quit 1)))))
  (t
   (let ((initialized nil))
     (loop for line = (read-line *standard-input* nil)
           while line
           do (let ((json (ignore-errors (parse-json line))))
                (when (equal (jget json "type") "user")
                  (unless initialized
                    (setf initialized t)
                    (mcp "initialize" (jobj "protocolVersion" "2025-06-18"))
                    (emit (jobj "type" "system" "subtype" "init" "session_id" *session* "model" "fake"
                                "mcp_servers" (vector (jobj "name" "cadre"
                                                            "status" (if (jget (mcp "tools/list") "result" "tools")
                                                                         "connected" "failed"))))))
                  (let* ((text (jget json "message" "content" 0 "text"))
                         (end (search "</editor-context>" text)))
                    ;; Only the user's own words choose the turn.
                    (turn (if end (subseq text (+ end (length "</editor-context>"))) text)))))))))
