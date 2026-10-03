;;;; mcp.lisp — Cadre's MCP server, through which Claude uses the editor
;;;;
;;;; MCP over "streamable HTTP", reduced to what Claude Code needs: each
;;;; JSON-RPC request is POSTed and answered with one JSON response
;;;; (initialize, tools/list, tools/call, ping); notifications get 202. The
;;;; server listens on 127.0.0.1 on a random port and refuses requests
;;;; without the bearer token it writes to mcp.json. Each connection gets a
;;;; thread, so a tool waiting for the user (an edit to review) doesn't hold
;;;; up the others. Tool handlers run on those threads.

(in-package #:cadre)

;;; Tools

(defstruct (mcp-tool (:conc-name mcp-tool-))
  name description schema handler)

(defvar *mcp-tools* '() "The tools Cadre offers, newest first.")

(defun register-mcp-tool (name description schema handler)
  (setf *mcp-tools* (cons (make-mcp-tool :name name :description description :schema schema :handler handler)
                          (remove name *mcp-tools* :key #'mcp-tool-name :test #'string=)))
  name)

(defmacro define-mcp-tool (name (args) description schema &body body)
  "Define the MCP tool NAME (a string). BODY runs with ARGS bound to the
call's arguments (a JSON object) and returns text, or signals an error
(reported to Claude as a failed call). SCHEMA is the arguments' properties:
a list of (name type description &key required enum)."
  `(register-mcp-tool ,name ,description ',schema (lambda (,args) (declare (ignorable ,args)) ,@body)))

(defun schema-json (properties)
  (jobj "type" "object"
        "properties" (apply #'jobj
                            (loop for (name type description . options) in properties
                                  append (list name (jobj "type" type "description" description
                                                          "enum" (let ((e (getf options :enum)))
                                                                   (if e (coerce e 'vector) :omit))))))
        "required" (coerce (loop for (name nil nil . options) in properties
                                 when (getf options :required) collect name)
                           'vector)))

(define-condition mcp-tool-error (error)
  ((message :initarg :message :reader mcp-tool-error-message))
  (:report (lambda (c s) (write-string (mcp-tool-error-message c) s))))

(defun tool-error (format &rest args)
  "Fail the current tool call with a message for Claude."
  (error 'mcp-tool-error :message (apply #'format nil format args)))

(defun tool-argument (args name &key required)
  (let ((value (jget args name)))
    (when (member value '(:null)) (setf value nil))
    (when (and required (null value)) (tool-error "Missing argument: ~a" name))
    value))

;;; JSON-RPC

(defparameter *mcp-protocol-version* "2025-06-18")

(defun call-mcp-tool (name args)
  "Run tool NAME on ARGS: (values text error-p)."
  (let ((tool (find name *mcp-tools* :key #'mcp-tool-name :test #'string=)))
    (if (null tool)
        (values (format nil "Unknown tool: ~a" name) t)
        (handler-case (let ((result (funcall (mcp-tool-handler tool) (or args (jobj)))))
                        (values (if (stringp result) result (json-string result)) nil))
          (mcp-tool-error (e) (values (mcp-tool-error-message e) t))
          (error (e) (values (format nil "Error: ~a" e) t))))))

(defun mcp-handle (request)
  "The JSON-RPC response to REQUEST (a JSON object), or nil for a notification."
  (let ((id (jget request "id"))
        (method (jget request "method"))
        (params (jget request "params")))
    (flet ((reply (result) (jobj "jsonrpc" "2.0" "id" id "result" result))
           (fail (code message) (jobj "jsonrpc" "2.0" "id" id "error" (jobj "code" code "message" message))))
      (cond
        ((null id) nil)                 ; notifications/initialized and the like
        ((equal method "initialize")
         (reply (jobj "protocolVersion" (or (jget params "protocolVersion") *mcp-protocol-version*)
                      "capabilities" (jobj "tools" (jobj))
                      "serverInfo" (jobj "name" "cadre" "version" "0.1"))))
        ((equal method "ping") (reply (jobj)))
        ((equal method "tools/list")
         (reply (jobj "tools" (coerce (loop for tool in (reverse *mcp-tools*)
                                            collect (jobj "name" (mcp-tool-name tool)
                                                          "description" (mcp-tool-description tool)
                                                          "inputSchema" (schema-json (mcp-tool-schema tool))))
                                      'vector))))
        ((equal method "tools/call")
         (multiple-value-bind (text error-p) (call-mcp-tool (jget params "name") (jget params "arguments"))
           (reply (jobj "content" (vector (jobj "type" "text" "text" text))
                        "isError" (if error-p t :false)))))
        (t (fail -32601 (format nil "Method not found: ~a" method)))))))

;;; HTTP

(defstruct (mcp-server (:conc-name mcp-server-))
  socket port token thread (running t))

(defun read-crlf-line (stream)
  "A line from STREAM (octets, as Latin-1), without the CRLF; nil at end."
  (let ((line (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil)
          do (cond ((null byte) (return (and (plusp (length line)) (coerce line 'string))))
                   ((= byte 10) (return (string-right-trim '(#\Return) (coerce line 'string))))
                   (t (vector-push-extend (code-char byte) line))))))

(defun read-http-request (stream)
  "(values method path headers body) for the request on STREAM, or nil."
  (let ((request-line (read-crlf-line stream)))
    (when request-line
      (let ((parts (uiop:split-string request-line :separator " "))
            (headers '()))
        (loop for line = (read-crlf-line stream)
              while (and line (plusp (length line)))
              do (let ((colon (position #\: line)))
                   (when colon
                     (push (cons (string-downcase (subseq line 0 colon))
                                 (string-trim " " (subseq line (1+ colon))))
                           headers))))
        (let* ((length (parse-integer (or (cdr (assoc "content-length" headers :test #'string=)) "0")
                                      :junk-allowed t))
               (octets (make-array (or length 0) :element-type '(unsigned-byte 8))))
          (read-sequence octets stream)
          (values (first parts) (second parts) headers
                  (sb-ext:octets-to-string octets :external-format :utf-8)))))))

(defun write-http-response (stream status reason &optional body (content-type "application/json"))
  (let ((octets (and body (sb-ext:string-to-octets body :external-format :utf-8))))
    (write-sequence (sb-ext:string-to-octets
                     (format nil "HTTP/1.1 ~d ~a~c~cContent-Length: ~d~c~c~@[Content-Type: ~a~c~c~]Connection: close~c~c~c~c"
                             status reason #\Return #\Newline (length octets) #\Return #\Newline
                             (and body content-type) #\Return #\Newline
                             #\Return #\Newline #\Return #\Newline)
                     :external-format :latin-1)
                    stream)
    (when octets (write-sequence octets stream))
    (force-output stream)))

(defun serve-mcp-connection (server socket)
  (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                          :element-type '(unsigned-byte 8) :buffering :full)))
    (unwind-protect
         (handler-case
             (multiple-value-bind (method path headers body) (read-http-request stream)
               (declare (ignore path))
               (cond ((null method))
                     ((not (equal (cdr (assoc "authorization" headers :test #'string=))
                                  (format nil "Bearer ~a" (mcp-server-token server))))
                      (write-http-response stream 401 "Unauthorized"))
                     ((not (string= method "POST"))
                      (write-http-response stream 405 "Method Not Allowed"))
                     (t (let* ((request (ignore-errors (parse-json body)))
                               (response (cond ((jobject-p request) (mcp-handle request))
                                               ((vectorp request)
                                                (remove nil (map 'vector #'mcp-handle request)))
                                               (t (jobj "jsonrpc" "2.0" "id" :null
                                                        "error" (jobj "code" -32700 "message" "Parse error"))))))
                          (if (or (null response) (and (vectorp response) (zerop (length response))))
                              (write-http-response stream 202 "Accepted")
                              (write-http-response stream 200 "OK" (json-string response)))))))
           (error () nil))
      (ignore-errors (close stream))
      (ignore-errors (sb-bsd-sockets:socket-close socket)))))

(defun start-mcp-server (&key (token (random-token)))
  "Start the MCP server on a free port of 127.0.0.1."
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 16)
    (let ((server (make-mcp-server :socket socket :token token
                                   :port (nth-value 1 (sb-bsd-sockets:socket-name socket)))))
      (setf (mcp-server-thread server)
            (sb-thread:make-thread
             (lambda ()
               (loop while (mcp-server-running server)
                     do (let ((client (ignore-errors (sb-bsd-sockets:socket-accept socket))))
                          (when client
                            (sb-thread:make-thread (lambda () (serve-mcp-connection server client))
                                                   :name "mcp request")))))
             :name "mcp server"))
      server)))

(defun stop-mcp-server (server)
  (when server
    (setf (mcp-server-running server) nil)
    (ignore-errors (sb-bsd-sockets:socket-close (mcp-server-socket server)))))

(defun mcp-server-url (server)
  (format nil "http://127.0.0.1:~d/mcp" (mcp-server-port server)))

(defun write-mcp-config (server directory)
  "Write mcp.json (the server's URL and token) into DIRECTORY, readable only
by the user. Returns its path."
  (let ((directory (uiop:ensure-directory-pathname directory))
        (path nil))
    (ensure-directories-exist directory)
    (sb-posix:chmod (namestring directory) #o700)
    (setf path (merge-pathnames "mcp.json" directory))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (sb-posix:chmod (namestring path) #o600)
      (write-json (jobj "mcpServers"
                        (jobj "cadre" (jobj "type" "http" "url" (mcp-server-url server)
                                            "headers" (jobj "Authorization"
                                                            (format nil "Bearer ~a" (mcp-server-token server))))))
                  out))
    path))

;;; A client, for tests and the stand-in CLI

(defun http-post-json (url token json)
  "POST JSON to URL with the bearer TOKEN; the reply's status and parsed body."
  (let* ((start (+ (search "://" url) 3))
         (slash (position #\/ url :start start))
         (colon (position #\: url :start start :end slash))
         (port (parse-integer url :start (1+ colon) :end slash))
         (path (subseq url slash))
         (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (sb-bsd-sockets:socket-connect socket #(127 0 0 1) port)
    (let ((stream (sb-bsd-sockets:socket-make-stream socket :input t :output t
                                                            :element-type '(unsigned-byte 8) :buffering :full))
          (body (sb-ext:string-to-octets (json-string json) :external-format :utf-8)))
      (unwind-protect
           (progn
             (write-sequence (sb-ext:string-to-octets
                              (format nil "POST ~a HTTP/1.1~c~cHost: 127.0.0.1~c~c~@[Authorization: Bearer ~a~c~c~]Content-Type: application/json~c~cAccept: application/json, text/event-stream~c~cContent-Length: ~d~c~c~c~c"
                                      path #\Return #\Newline #\Return #\Newline
                                      token #\Return #\Newline #\Return #\Newline #\Return #\Newline
                                      (length body) #\Return #\Newline #\Return #\Newline)
                              :external-format :latin-1)
                             stream)
             (write-sequence body stream)
             (force-output stream)
             (let* ((status-line (read-crlf-line stream))
                    (status (parse-integer status-line :start 9 :end 12))
                    (headers (loop for line = (read-crlf-line stream)
                                   while (and line (plusp (length line)))
                                   collect (let ((c (position #\: line)))
                                             (cons (string-downcase (subseq line 0 c))
                                                   (string-trim " " (subseq line (1+ c)))))))
                    (length (parse-integer (or (cdr (assoc "content-length" headers :test #'string=)) "0")))
                    (octets (make-array length :element-type '(unsigned-byte 8))))
               (read-sequence octets stream)
               (values status (and (plusp length)
                                   (parse-json (sb-ext:octets-to-string octets :external-format :utf-8))))))
        (ignore-errors (close stream))
        (ignore-errors (sb-bsd-sockets:socket-close socket))))))
