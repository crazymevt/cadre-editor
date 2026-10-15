(in-package #:cadre-tests)

;;; API requests (core http/): .http files, variables, secrets and curl

(defparameter *sample-http* "@baseUrl = https://api.example.com
@version = v2

### List users
# @no-redirect
GET {{baseUrl}}/{{version}}/users
  ?page=1
  &size=10
Accept: application/json
Authorization: Bearer {{token}}

### Add a user
# @timeout 5
POST {{baseUrl}}/users HTTP/1.1
Content-Type: application/json

{
  \"name\": \"Ann\"
}

###
// @name ping
https://example.com/ping
")

(define-test http-files :parent cadre-tests
  (multiple-value-bind (requests variables) (c:parse-http-file *sample-http*)
    (is equal '(("baseUrl" . "https://api.example.com") ("version" . "v2")) variables)
    (is = 3 (length requests))
    (destructuring-bind (list add ping) requests
      (is string= "List users" (c:http-request-name list))
      (is string= "GET" (c:http-request-method list))
      (is string= "{{baseUrl}}/{{version}}/users?page=1&size=10" (c:http-request-url list))
      (is string= "Bearer {{token}}" (c:http-request-header list "authorization"))
      (is eq t (getf (c:http-request-options list) :no-redirect))
      (is string= "POST" (c:http-request-method add))
      (is string= "{{baseUrl}}/users" (c:http-request-url add))
      (is = 5 (getf (c:http-request-options add) :timeout))
      (is string= (format nil "{~%  \"name\": \"Ann\"~%}") (c:http-request-body add))
      (is string= "ping" (c:http-request-name ping))
      (is string= "https://example.com/ping" (c:http-request-url ping))
      (is string= "GET https://example.com/ping" (c:http-request-title (c:make-http-request :url "https://example.com/ping")))
      ;; Rewriting one request leaves the others, and the file variables, as they were.
      (let* ((changed (c:copy-http-request add)))
        (setf (c:http-request-body changed) "{\"name\": \"Bo\"}")
        (let ((text (c:replace-http-request *sample-http* add changed)))
          (is string= "{\"name\": \"Bo\"}" (c:http-request-body (second (c:parse-http-file text))))
          (is string= "ping" (c:http-request-name (third (c:parse-http-file text))))
          (is equal variables (nth-value 1 (c:parse-http-file text)))))
      (let ((text (c:append-http-request *sample-http* (c:make-http-request :name "New" :url "https://x.test/"))))
        (is equal '("List users" "Add a user" "ping" "New")
            (mapcar #'c:http-request-name (c:parse-http-file text))))
      (is equal '("Add a user" "ping")
          (mapcar #'c:http-request-name (c:parse-http-file (c:remove-http-request *sample-http* list))))))
  ;; What it writes, it reads back.
  (let* ((request (c:make-http-request :name "R" :method "PUT" :url "https://x.test/a"
                                       :headers '(("A" . "1") ("B" . "two: 2")) :body "line 1
line 2" :options '(:insecure t :timeout 9)))
         (back (first (c:parse-http-file (c:render-http-request request)))))
    (is equalp (list "R" "PUT" "https://x.test/a" '(("A" . "1") ("B" . "two: 2")) "line 1
line 2")
        (list (c:http-request-name back) (c:http-request-method back) (c:http-request-url back)
              (c:http-request-headers back) (c:http-request-body back)))
    (is eq t (getf (c:http-request-options back) :insecure))
    (is = 9 (getf (c:http-request-options back) :timeout)))
  (is equal '() (c:parse-http-file "# just a comment
")))

(define-test http-variables :parent cadre-tests
  (let* ((request (first (c:parse-http-file *sample-http*)))
         (lookup (c:http-variable-lookup :file-variables '(("baseUrl" . "{{host}}/api") ("version" . "v2"))
                                         :environment '(("host" . "https://dev.test") ("token" . "public"))
                                         :secrets '(("token" . "s3cret")))))
    (multiple-value-bind (resolved missing) (c:resolve-http-request request lookup)
      (is equal '() missing)
      (is string= "https://dev.test/api/v2/users?page=1&size=10" (c:http-request-url resolved))
      (is string= "Bearer s3cret" (c:http-request-header resolved "Authorization"))
      ;; The request itself still has its variables.
      (is string= "Bearer {{token}}" (c:http-request-header request "Authorization")))
    (is equal '("nope") (nth-value 1 (c:resolve-http-request (c:make-http-request :url "{{nope}}/{{$guid}}")
                                                             (constantly nil))))
    (is = 36 (length (c:http-request-url (c:resolve-http-request (c:make-http-request :url "{{$guid}}") (constantly nil)))
                     ))))

(define-test http-environments :parent cadre-tests
  (let ((file (merge-pathnames (format nil "cadre-env-~d/secret.json" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (let ((envs (c:set-http-environment-variable
                      (c:set-http-environment-variable '() "dev" "token" "abc") "$shared" "who" "me")))
           (c:write-http-environments file envs :private t)
           (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring file)))))
           (let ((read (c:read-http-environments file)))
             (is equal '("dev") (c:http-environment-names read))
             (is equal '(("token" . "abc") ("who" . "me")) (c:http-environment-variables read "dev"))
             (is equal '(("who" . "me")) (c:http-environment-variables read nil))))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname file) :validate t :if-does-not-exist :ignore)))
  (true (ppcre:scan "^myproj-[0-9a-f]{12}\\.json$" (c:http-secrets-file-name #p"/tmp/x/myproj/")))
  ;; The scratch buffer's file: the same key, a .lisp
  (is string= (format nil "~a.lisp" (c:project-file-key #p"/tmp/x/myproj/")) (c:scratch-file-name #p"/tmp/x/myproj/"))
  (false (string= (c:project-file-key #p"/tmp/x/myproj/") (c:project-file-key #p"/tmp/y/myproj/")))
  (is string= "scratch.lisp" (c:scratch-file-name nil)))

(define-test http-secrets :parent cadre-tests
  (let* ((request (c:make-http-request :url "https://x.test/a?api_key=K123&q=1"
                                       :headers '(("Authorization" . "Bearer abc.def") ("Accept" . "*/*")
                                                  ("X-Api-Key" . "{{key}}"))
                                       :body "{\"user\": \"ann\", \"password\": \"hunter2\", \"nested\": {\"client_secret\": \"zz\"}}"))
         (found (c:find-literal-secrets request)))
    (is equal '("token" "api_key" "password" "client_secret") (mapcar (lambda (s) (getf s :variable)) found))
    (is equal '("abc.def" "K123" "hunter2" "zz") (mapcar (lambda (s) (getf s :value)) found))
    (let ((moved (c:move-secrets-out request found)))
      (is string= "Bearer {{token}}" (c:http-request-header moved "Authorization"))
      (is string= "https://x.test/a?api_key={{api_key}}&q=1" (c:http-request-url moved))
      (true (search "\"password\": \"{{password}}\"" (c:http-request-body moved)))
      (is equal '() (c:find-literal-secrets moved)))))

(define-test http-captures :parent cadre-tests
  (let ((json (c:parse-json "{\"token\": \"abc\", \"data\": [{\"id\": 7, \"ok\": true}], \"odd key\": 1}")))
    (is string= "abc" (c:json-path-value json "$.token"))
    (is string= "abc" (c:json-path-value json "token"))
    (is = 7 (c:json-path-value json "$.data[0].id"))
    (is = 1 (c:json-path-value json "$['odd key']"))
    (is equal '(nil nil) (multiple-value-list (c:json-path-value json "$.data[3]")))
    (is equal '(nil nil) (multiple-value-list (c:json-path-value json "$.nope"))))
  (let ((response (c:make-http-response :status 201 :headers '(("Location" . "/users/9"))
                                        :body "{\"token\": \"abc\", \"user\": {\"id\": 9, \"admin\": false}}")))
    (multiple-value-bind (found missing)
        (c:capture-response-values response '(("token" . "$.token") ("id" . "$.user.id") ("admin" . "$.user.admin")
                                              ("where" . "header location") ("code" . "status") ("gone" . "$.x")))
      (is equal '(("token" . "abc") ("id" . "9") ("admin" . "false") ("where" . "/users/9") ("code" . "201")) found)
      (is equal '("gone") missing)))
  ;; Captures are kept in the file.
  (let* ((text "### Log in
# @capture token = $.token
# @capture id = $.user.id
POST https://x.test/login
")
         (request (first (c:parse-http-file text))))
    (is equal '(("token" . "$.token") ("id" . "$.user.id")) (getf (c:http-request-options request) :captures))
    (is equal (getf (c:http-request-options request) :captures)
        (getf (c:http-request-options (first (c:parse-http-file (c:render-http-request request)))) :captures)))
  (is string= "new" (funcall (c:http-variable-lookup :captured '(("token" . "new")) :secrets '(("token" . "old"))) "token")))

(define-test http-auth :parent cadre-tests
  (flet ((auth (&rest headers)
           (multiple-value-list (c:http-request-auth (c:make-http-request :headers headers)))))
    (is equal '(:bearer "token" "Authorization") (auth '("Authorization" . "Bearer {{token}}")))
    (is equal '(:basic "basic_auth" "Authorization") (auth '("authorization" . "basic {{ basic_auth }}")))
    (is equal '(:bearer nil "Authorization") (auth '("Authorization" . "Bearer abc")))
    (is equal '(:api-key "key" "X-Api-Key") (auth '("Accept" . "*/*") '("X-Api-Key" . "{{key}}")))
    (is equal '(nil nil nil) (auth '("Accept" . "*/*"))))
  (is equal '("Authorization" . "Bearer {{t}}") (c:auth-header :bearer "t"))
  (is equal '("X-Token" . "{{k}}") (c:auth-header :api-key "k" :header "X-Token")))

(define-test http-bodies :parent cadre-tests
  (is equal '(("a" . "1") ("b" . "two words")) (c:form-body-pairs "a=1&b=two%20words"))
  (is string= "a=1&b=two%20words&c=x%26y" (c:pairs-form-body '(("a" . "1") ("b" . "two words") ("c" . "x&y"))))
  (is string= "./data/x.json" (c:body-file-reference "  < ./data/x.json
"))
  (is equal nil (c:body-file-reference "{\"a\": 1}"))
  (let* ((parts '((:name "title" :value "Hello
world") (:name "photo" :file "./cat.png" :type "image/png")))
         (body (c:render-multipart-body parts "B0"))
         (request (c:make-http-request :method "POST" :url "https://x.test/up" :body body
                                       :headers '(("Content-Type" . "multipart/form-data; boundary=B0")))))
    (is string= "B0" (c:multipart-boundary request))
    (is equal '((:name "title" :value "Hello
world") (:name "photo" :file "./cat.png" :filename "cat.png" :type "image/png"))
        (c:parse-multipart-body body "B0"))
    (let ((config (c:curl-config request :directory #p"/home/me/api/")))
      (true (search "form-string = \"title=Hello\\nworld\"" config))
      (true (search "form = \"photo=@\\\"/home/me/api/cat.png\\\";filename=\\\"cat.png\\\";type=image/png\"" config))
      (false (search "Content-Type" config)))
    ;; Copied as a command line and pasted back.
    (let ((again (c:parse-curl-command (c:http-request-curl-command request :directory #p"/home/me/api/"))))
      (is equal '((:name "title" :value "Hello
world") (:name "photo" :file "/home/me/api/cat.png" :filename "cat.png" :type "image/png"))
          (c:parse-multipart-body (c:http-request-body again) (c:multipart-boundary again)))))
  (is string= "< /tmp/x.json" (c:http-request-body (c:parse-curl-command "curl -X PUT --data-binary @/tmp/x.json https://x.test")))
  (true (search "data-binary = \"@/home/me/api/x.json\""
                (c:curl-config (c:make-http-request :method "POST" :url "u" :body "< ./x.json") :directory #p"/home/me/api/")))
  (is string= "./sub/a.png" (c:relative-body-path "/home/me/api/sub/a.png" #p"/home/me/api/"))
  (is string= "/elsewhere/a.png" (c:relative-body-path "/elsewhere/a.png" #p"/home/me/api/")))

(define-test cookie-jars :parent cadre-tests
  (let ((jar (c:parse-cookie-jar (format nil "# Netscape HTTP Cookie File~%~%127.0.0.1~cFALSE~c/~cFALSE~c0~csession~cabc~%#HttpOnly_.x.test~cTRUE~c/app~cTRUE~c1893456000~cid~c42~%"
                                         #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab #\Tab))))
    (is equal '("session" "id") (mapcar (lambda (c) (getf c :name)) jar))
    (is equal nil (getf (first jar) :expires))
    (is eq t (getf (second jar) :http-only))
    (is eq t (getf (second jar) :secure))
    (is string= "/app" (getf (second jar) :path)))
  (let ((r (first (c:parse-http-file "### R
# @no-cookie-jar
GET https://x.test/
"))))
    (is eq t (getf (c:http-request-options r) :no-cookie-jar))
    (true (search "@no-cookie-jar" (c:render-http-request r)))
    (false (search "cookie-jar" (c:curl-config r :cookie-jar #p"/tmp/jar.txt"))))
  (true (search "cookie-jar = \"/tmp/jar.txt\"" (c:curl-config (c:make-http-request :url "u") :cookie-jar #p"/tmp/jar.txt"))))

(define-test query-parameters :parent cadre-tests
  (is equal '(("q" . "a b") ("tag" . "λ") ("token" . "{{token}}") ("flag" . ""))
      (c:url-query-parameters "https://x.test/s?q=a%20b&tag=%CE%BB&token={{token}}&flag#top"))
  (is equal '() (c:url-query-parameters "{{baseUrl}}/users"))
  (is string= "https://x.test/s?q=a%20b&tag=%CE%BB&token={{token}}&flag#top"
      (c:url-with-query-parameters "https://x.test/s?old=1#top"
                                   '(("q" . "a b") ("tag" . "λ") ("token" . "{{token}}") ("flag" . "") ("" . "dropped"))))
  (is string= "{{baseUrl}}/users" (c:url-with-query-parameters "{{baseUrl}}/users?x=1" '()))
  (is string= "a&b=c%26d" (subseq (c:url-with-query-parameters "u" '(("a" . "") ("b" . "c&d"))) 2))
  ;; Reading what it wrote gives the same parameters.
  (let ((params '(("name" . "Ann & Bo") ("x" . "1+1=2") ("path" . "/a/b"))))
    (is equal params (c:url-query-parameters (c:url-with-query-parameters "https://x.test/" params)))))

(define-test curl-commands :parent cadre-tests
  (is equal '("curl" "-H" "A: b c" "it's" "" "x\"y" "joined")
      (c:shell-words "curl -H 'A: b c' \\
  it\\'s '' \"x\\\"y\" join\"ed\""))
  (let ((request (c:parse-curl-command "curl 'https://x.test/u?a=1' -H 'Accept: application/json' \\
  -H \"Authorization: Bearer t0k\" --data-raw '{\"n\":1}' --compressed -k")))
    (is string= "POST" (c:http-request-method request))
    (is string= "https://x.test/u?a=1" (c:http-request-url request))
    (is string= "{\"n\":1}" (c:http-request-body request))
    (is string= "application/json" (c:http-request-header request "Content-Type"))
    (is eq t (getf (c:http-request-options request) :insecure))
    ;; Copied out, pasted back.
    (let ((again (c:parse-curl-command (c:http-request-curl-command request))))
      (is string= (c:http-request-url request) (c:http-request-url again))
      (is equal (c:http-request-headers request) (c:http-request-headers again))
      (is string= (c:http-request-body request) (c:http-request-body again))))
  (is string= "Basic dXNlcjpwYXNz" (c:http-request-header (c:parse-curl-command "curl -u user:pass https://x.test") "Authorization"))
  (is string= "DELETE" (c:http-request-method (c:parse-curl-command "curl -XDELETE https://x.test/1")))
  (is string= "https://x.test/s?q=a" (c:http-request-url (c:parse-curl-command "curl -G -d q=a https://x.test/s")))
  (is equal nil (c:parse-curl-command "wget https://x.test"))
  (is string= "aGk=" (c:base64-string "hi")))

;;; A server for curl to talk to: it answers each request with what it got, as JSON.

(defun serve-http-echo (socket)
  (loop
    (let ((client (handler-case (sb-bsd-sockets:socket-accept socket) (error () (return)))))
      (let ((stream (sb-bsd-sockets:socket-make-stream client :input t :output t :element-type :default
                                                              :external-format :latin-1 :buffering :full)))
        (unwind-protect
             (let* ((request-line (string-right-trim '(#\Return) (read-line stream)))
                    (headers (loop for line = (string-right-trim '(#\Return) (read-line stream))
                                   until (string= line "")
                                   collect (let ((colon (position #\: line)))
                                             (cons (string-downcase (subseq line 0 colon))
                                                   (string-trim " " (subseq line (1+ colon)))))))
                    (length (let ((l (cdr (assoc "content-length" headers :test #'string=)))) (if l (parse-integer l) 0)))
                    (body (let ((s (make-string length))) (read-sequence s stream) s))
                    (parts (uiop:split-string request-line :separator " "))
                    (redirect (search "/redirect" (second parts)))
                    (set-cookie (search "/set-cookie" (second parts)))
                    (reply (c:json-string (c:jobj "method" (first parts) "path" (second parts)
                                                  "auth" (or (cdr (assoc "authorization" headers :test #'string=)) "")
                                                  "cookie" (or (cdr (assoc "cookie" headers :test #'string=)) "")
                                                  "body" body))))
               (if redirect
                   (format stream "HTTP/1.1 302 Found~c~cLocation: /landed~c~cContent-Length: 0~c~c~c~c"
                           #\Return #\Newline #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                   (format stream "HTTP/1.1 201 Created~c~cContent-Type: application/json~c~cX-Echo: yes~c~c~aContent-Length: ~d~c~c~c~c~a"
                           #\Return #\Newline #\Return #\Newline #\Return #\Newline
                           (if set-cookie (format nil "Set-Cookie: session=abc123; Path=/~c~c" #\Return #\Newline) "")
                           (length reply)
                           #\Return #\Newline #\Return #\Newline reply))
               (finish-output stream))
          (close stream))))))

(define-test curl-sends :parent cadre-tests
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 5)
    (let ((port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
          (thread nil))
      (setf thread (sb-thread:make-thread (lambda () (serve-http-echo socket)) :name "http echo"))
      (unwind-protect
           (let ((response (c:send-http-request
                            (c:make-http-request :method "POST" :url (format nil "http://127.0.0.1:~d/things?x=1" port)
                                                 :headers '(("Authorization" . "Bearer s3cret") ("Content-Type" . "application/json"))
                                                 :body "{\"name\": \"λ\"}"))))
             (is equal nil (c:http-response-error response))
             (is = 201 (c:http-response-status response))
             (is string= "Created" (c:http-response-reason response))
             (is string= "yes" (cdr (assoc "X-Echo" (c:http-response-headers response) :test #'string-equal)))
             (let ((json (c:parse-json (c:http-response-body response))))
               (is string= "POST" (c:jget json "method"))
               (is string= "/things?x=1" (c:jget json "path"))
               (is string= "Bearer s3cret" (c:jget json "auth"))
               ;; The body went, and came back, as UTF-8.
               (is string= "{\"name\": \"λ\"}" (c:jget json "body")))
             (true (search (format nil "~%  \"method\"") (c:response-body-text response)))
             ;; Redirects are followed unless the request says not to.
             (let ((followed (c:send-http-request (c:make-http-request :url (format nil "http://127.0.0.1:~d/redirect" port)))))
               (is = 201 (c:http-response-status followed))
               (is string= "/landed" (c:jget (c:parse-json (c:http-response-body followed)) "path")))
             (is = 302 (c:http-response-status
                        (c:send-http-request (c:make-http-request :url (format nil "http://127.0.0.1:~d/redirect" port)
                                                                  :options '(:no-redirect t)))))
             ;; A file as the body, and a multipart upload, from files beside the .http file.
             (let ((dir (uiop:ensure-directory-pathname
                         (merge-pathnames (format nil "cadre-body-~d/" (random 1000000)) (uiop:temporary-directory)))))
               (ensure-directories-exist dir)
               (unwind-protect
                    (progn
                      (with-open-file (o (merge-pathnames "p.json" dir) :direction :output) (write-string "{\"from\": \"file\"}" o))
                      (let ((sent (c:send-http-request (c:make-http-request :method "POST" :url (format nil "http://127.0.0.1:~d/f" port)
                                                                            :body "< ./p.json")
                                                       :directory dir)))
                        (is string= "{\"from\": \"file\"}" (c:jget (c:parse-json (c:http-response-body sent)) "body")))
                      (let* ((body (c:render-multipart-body '((:name "note" :value "hi") (:name "doc" :file "./p.json")) "XB"))
                             (sent (c:send-http-request
                                    (c:make-http-request :method "POST" :url (format nil "http://127.0.0.1:~d/m" port) :body body
                                                         :headers '(("Content-Type" . "multipart/form-data; boundary=XB")))
                                    :directory dir))
                             (echoed (c:jget (c:parse-json (c:http-response-body sent)) "body")))
                        (true (search "name=\"note\"" echoed))
                        (true (search "filename=\"p.json\"" echoed))
                        (true (search "{\"from\": \"file\"}" echoed))))
                 (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))
             ;; A cookie the server sets comes back on the next request through the jar.
             (let ((jar (merge-pathnames (format nil "cadre-jar-~d/jar.txt" (random 1000000)) (uiop:temporary-directory))))
               (unwind-protect
                    (progn
                      (c:send-http-request (c:make-http-request :url (format nil "http://127.0.0.1:~d/set-cookie" port)) :cookie-jar jar)
                      (is equal '("session") (mapcar (lambda (c) (getf c :name)) (c:read-cookie-jar jar)))
                      (let ((next (c:send-http-request (c:make-http-request :url (format nil "http://127.0.0.1:~d/next" port)) :cookie-jar jar)))
                        (is string= "session=abc123" (c:jget (c:parse-json (c:http-response-body next)) "cookie")))
                      (let ((without (c:send-http-request (c:make-http-request :url (format nil "http://127.0.0.1:~d/next" port)
                                                                               :options '(:no-cookie-jar t))
                                                          :cookie-jar jar)))
                        (is string= "" (c:jget (c:parse-json (c:http-response-body without)) "cookie"))))
                 (uiop:delete-directory-tree (uiop:pathname-directory-pathname jar) :validate t :if-does-not-exist :ignore)))
             (let ((failed (c:send-http-request (c:make-http-request :url "http://127.0.0.1:1/"))))
               (true (c:http-response-error failed))))
        (sb-bsd-sockets:socket-close socket)
        (ignore-errors (sb-thread:join-thread thread :timeout 2))))))
