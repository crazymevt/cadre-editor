;;;; http.lisp — API requests: .http files, variables, secrets, and curl
;;;;
;;;; Requests are kept in .http files, the format of VS Code's REST Client
;;;; and JetBrains' HTTP Client, so they can be committed and other tools
;;;; can run them:
;;;;
;;;;   @baseUrl = https://api.example.com        a file variable
;;;;
;;;;   ### List users                            ### starts a request, and names it
;;;;   # @no-redirect                            options: @no-redirect, @insecure, @timeout 10, @no-cookie-jar
;;;;   GET {{baseUrl}}/users?page=1
;;;;   Authorization: Bearer {{token}}
;;;;
;;;;   ### Log in
;;;;   # @capture token = $.token                a response value, as {{token}} from then on
;;;;   POST {{baseUrl}}/login
;;;;
;;;;   ### Add a user
;;;;   POST {{baseUrl}}/users
;;;;   Content-Type: application/json
;;;;
;;;;   {"name": "Ann"}
;;;;
;;;; {{name}} is a variable: the file's own, then the environment's. The
;;;; environments are in the project's http-client.env.json (JetBrains'
;;;; format: {"dev": {"baseUrl": "…"}, "$shared": {…}}); their secrets in
;;;; a file of the same shape outside the project (under the config
;;;; directory), so they are never committed. {{$guid}}, {{$timestamp}},
;;;; {{$isoTimestamp}} and {{$randomInt}} are made up when a request is sent.
;;;;
;;;; A request is sent by running curl, given its options on standard input
;;;; (so headers and their secrets don't show in the process list).

(in-package #:cadre)

;;; Requests

(defstruct (http-request (:conc-name http-request-))
  (name "")
  (method "GET")
  (url "")
  (headers '())                         ; (name . value) each, in order
  (body "")
  (options '())                         ; plist: :no-redirect :insecure :timeout :captures :no-cookie-jar
  (start nil)                           ; where its block is in its file's text
  (end nil))

(defparameter *http-methods* '("GET" "POST" "PUT" "PATCH" "DELETE" "HEAD" "OPTIONS"))

(defun http-request-header (request name)
  (cdr (assoc name (http-request-headers request) :test #'string-equal)))

(defun http-request-title (request)
  "A request's name, or its method and URL."
  (let ((name (http-request-name request)))
    (if (plusp (length name))
        name
        (format nil "~a ~a" (http-request-method request) (http-request-url request)))))

;;; Reading a .http file

(defun text-lines-with-offsets (text)
  "TEXT's lines, each (start end string), END before its newline."
  (loop with n = (length text)
        for start = 0 then (1+ end)
        for end = (or (position #\Newline text :start start) n)
        collect (list start end (string-right-trim '(#\Return) (subseq text start end)))
        while (< end n)))

(defun separator-line-p (line) (uiop:string-prefix-p "###" line))

(defun comment-line-p (line)
  (let ((trimmed (string-left-trim " 	" line)))
    (or (uiop:string-prefix-p "#" trimmed) (uiop:string-prefix-p "//" trimmed))))

(defun comment-text (line)
  (string-trim " 	" (string-left-trim "#/" (string-left-trim " 	" line))))

(defun file-variable-line (line)
  "(name . value) for a line \"@name = value\", or nil."
  (multiple-value-bind (match groups) (ppcre:scan-to-strings "^\\s*@([A-Za-z_][\\w.-]*)\\s*=\\s*(.*?)\\s*$" line)
    (and match (cons (aref groups 0) (aref groups 1)))))

(defun request-line (line)
  "The method and URL of a request line: \"POST url HTTP/1.1\", or a URL alone (GET)."
  (let* ((parts (remove "" (uiop:split-string (string-trim " 	" line) :separator " 	") :test #'string=))
         (method (and (rest parts) (find (first parts) *http-methods* :test #'string-equal))))
    (when (and parts (ppcre:scan "^HTTP/" (car (last parts))) (if method (cddr parts) (rest parts)))
      (setf parts (butlast parts)))
    (if method
        (values (string-upcase method) (format nil "~{~a~^ ~}" (rest parts)))
        (values "GET" (format nil "~{~a~^ ~}" parts)))))

(defun parse-http-block (lines start end variables)
  "The request in LINES (one block's, (start end string) each), or nil;
@name lines are added to VARIABLES (an adjustable vector)."
  (let ((request (make-http-request :start start :end end))
        (state :before)
        (body-lines '()))
    (dolist (entry lines)
      (let ((line (third entry)))
        (ecase state
          (:before
           (cond ((separator-line-p line)
                  (setf (http-request-name request) (string-trim " 	" (subseq line 3))))
                 ((string= (string-trim " 	" line) ""))
                 ((file-variable-line line) (vector-push-extend (file-variable-line line) variables))
                 ((comment-line-p line)
                  (let ((comment (comment-text line)))
                    (multiple-value-bind (match groups) (ppcre:scan-to-strings "^@([\\w-]+)\\s*(.*)$" comment)
                      (when match
                        (let ((option (aref groups 0)) (value (aref groups 1)))
                          (cond ((string-equal option "name")
                                 (when (string= (http-request-name request) "")
                                   (setf (http-request-name request) value)))
                                ((string-equal option "no-redirect")
                                 (setf (getf (http-request-options request) :no-redirect) t))
                                ((string-equal option "insecure")
                                 (setf (getf (http-request-options request) :insecure) t))
                                ((string-equal option "no-cookie-jar")
                                 (setf (getf (http-request-options request) :no-cookie-jar) t))
                                ((string-equal option "capture")
                                 (multiple-value-bind (m g) (ppcre:scan-to-strings "^([$\\w.-]+)\\s*=\\s*(.+?)\\s*$" value)
                                   (when m
                                     (setf (getf (http-request-options request) :captures)
                                           (append (getf (http-request-options request) :captures)
                                                   (list (cons (aref g 0) (aref g 1))))))))
                                ((string-equal option "timeout")
                                 (let ((seconds (ignore-errors (parse-integer value :junk-allowed t))))
                                   (when seconds (setf (getf (http-request-options request) :timeout) seconds))))))))))
                 (t (multiple-value-bind (method url) (request-line line)
                      (setf (http-request-method request) method
                            (http-request-url request) url
                            state :headers)))))
          (:headers
           (let ((trimmed (string-trim " 	" line)))
             (cond ((string= trimmed "") (setf state :body))
                   ;; A query string continued on the next lines.
                   ((or (uiop:string-prefix-p "?" trimmed) (uiop:string-prefix-p "&" trimmed))
                    (setf (http-request-url request) (concatenate 'string (http-request-url request) trimmed)))
                   ((comment-line-p line))
                   (t (let ((colon (position #\: line)))
                        (when colon
                          (setf (http-request-headers request)
                                (append (http-request-headers request)
                                        (list (cons (string-trim " 	" (subseq line 0 colon))
                                                    (string-trim " 	" (subseq line (1+ colon)))))))))))))
          (:body (push line body-lines)))))
    (setf (http-request-body request)
          (string-trim '(#\Newline #\Space #\Tab #\Return) (format nil "~{~a~^~%~}" (reverse body-lines))))
    (and (not (eq state :before)) request)))

(defun parse-http-file (text)
  "The requests in TEXT, a .http file, and its file variables ((name . value) each)."
  (let* ((lines (text-lines-with-offsets text))
         (variables (make-array 0 :adjustable t :fill-pointer t))
         (blocks '())
         (current '()))
    (dolist (line lines)
      (when (and (separator-line-p (third line)) current)
        (push (reverse current) blocks)
        (setf current '()))
      (push line current))
    (when current (push (reverse current) blocks))
    (setf blocks (reverse blocks))
    (values (loop for (block next) on blocks
                  for request = (parse-http-block block (first (first block))
                                                  (if next (first (first next)) (length text))
                                                  variables)
                  when request collect request)
            (coerce variables 'list))))

;;; Writing one

(defun render-http-request (request)
  "REQUEST as a block of a .http file, starting with its ### line."
  (with-output-to-string (out)
    (format out "### ~a~%" (http-request-name request))
    (let ((options (http-request-options request)))
      (when (getf options :no-redirect) (format out "# @no-redirect~%"))
      (when (getf options :insecure) (format out "# @insecure~%"))
      (when (getf options :no-cookie-jar) (format out "# @no-cookie-jar~%"))
      (when (getf options :timeout) (format out "# @timeout ~d~%" (getf options :timeout)))
      (loop for (name . path) in (getf options :captures)
            do (format out "# @capture ~a = ~a~%" name path)))
    (format out "~a ~a~%" (http-request-method request) (http-request-url request))
    (loop for (name . value) in (http-request-headers request)
          do (format out "~a: ~a~%" name value))
    (when (plusp (length (http-request-body request)))
      (format out "~%~a~%" (http-request-body request)))
    (terpri out)))

(defun block-variable-lines (text request)
  "The @name lines in REQUEST's block of TEXT, which go with it when it's rewritten."
  (with-output-to-string (out)
    (dolist (line (text-lines-with-offsets (subseq text (http-request-start request) (http-request-end request))))
      (let ((text (third line)))
        (cond ((file-variable-line text) (format out "~a~%" text))
              ((or (separator-line-p text) (comment-line-p text)
                   (string= (string-trim '(#\Space #\Tab) text) "")))
              (t (return)))))))

(defun replace-http-request (text old new)
  "TEXT with request OLD's block (from parsing TEXT) replaced by NEW."
  (let ((variables (block-variable-lines text old)))
    (concatenate 'string
                 (subseq text 0 (http-request-start old))
                 variables (if (plusp (length variables)) (string #\Newline) "")
                 (render-http-request new)
                 (subseq text (http-request-end old)))))

(defun append-http-request (text request)
  "TEXT with REQUEST added at the end."
  (let ((text (string-right-trim '(#\Newline #\Space #\Tab) text)))
    (concatenate 'string text (if (plusp (length text)) (format nil "~%~%") "") (render-http-request request))))

(defun remove-http-request (text request)
  (concatenate 'string (subseq text 0 (http-request-start request))
               (block-variable-lines text request)
               (subseq text (http-request-end request))))

;;; Variables

(defparameter *http-variable-scanner* (ppcre:create-scanner "\\{\\{\\s*([$\\w.-]+)\\s*\\}\\}"))

(defun http-variable-names (string)
  (let ((names '()))
    (ppcre:do-register-groups (name) (*http-variable-scanner* string)
      (pushnew name names :test #'string=))
    (nreverse names)))

(defun dynamic-http-variable (name)
  (cond ((member name '("$guid" "$uuid" "$random.uuid") :test #'string=) (string-downcase (make-uuid)))
        ((string= name "$timestamp") (princ-to-string (- (get-universal-time) #.(encode-universal-time 0 0 0 1 1 1970 0))))
        ((string= name "$isoTimestamp")
         (multiple-value-bind (s m h d mo y) (decode-universal-time (get-universal-time) 0)
           (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ" y mo d h m s)))
        ((string= name "$randomInt") (princ-to-string (random 1000)))))

(defun substitute-http-variables (string lookup)
  "STRING with each {{name}} replaced by (funcall LOOKUP name), or by a made-up
value for $guid and the like. Returns it and the names nothing gave a value."
  (let ((missing '()))
    (values (ppcre:regex-replace-all *http-variable-scanner* string
                                     (lambda (match name)
                                       (or (funcall lookup name)
                                           (dynamic-http-variable name)
                                           (progn (pushnew name missing :test #'string=) match)))
                                     :simple-calls t)
            (nreverse missing))))

(defun resolve-http-request (request lookup)
  "A copy of REQUEST with its variables' values put in (LOOKUP: a name to its
value, or nil), and the names that had none. A variable's value may hold
others: they are put in too."
  (let ((missing '()))
    (labels ((resolve (string &optional (depth 0))
               (multiple-value-bind (result names) (substitute-http-variables string lookup)
                 (if (and (< depth 5) (http-variable-names result) (not (equal result string)))
                     (resolve result (1+ depth))
                     (progn (dolist (n names) (pushnew n missing :test #'string=)) result)))))
      (let ((copy (copy-http-request request)))
        (setf (http-request-url copy) (resolve (http-request-url request))
              (http-request-headers copy) (mapcar (lambda (h) (cons (resolve (car h)) (resolve (cdr h))))
                                                  (http-request-headers request))
              (http-request-body copy) (resolve (http-request-body request)))
        (values copy (nreverse missing))))))

(defun http-variable-lookup (&key file-variables captured environment secrets)
  "A lookup for RESOLVE-HTTP-REQUEST: the file's variables, then values
captured from responses, then the environment's secrets, then the
environment's own (each an alist)."
  (lambda (name)
    (cdr (or (assoc name file-variables :test #'string=)
             (assoc name captured :test #'string=)
             (assoc name secrets :test #'string=)
             (assoc name environment :test #'string=)))))

;;; Environments and secrets: {"env": {"name": "value"}, "$shared": {…}}

(defun project-http-environments-file (project)
  (merge-pathnames "http-client.env.json" (uiop:ensure-directory-pathname project)))

(defun read-http-environments (file)
  "FILE's environments, as (name . ((variable . value) …)) each; none if it doesn't exist or isn't JSON."
  (let ((json (ignore-errors (parse-json (uiop:read-file-string file)))))
    (when (jobject-p json)
      (loop for (name . vars) in (cdr json)
            when (jobject-p vars)
              collect (cons name (loop for (k . v) in (cdr vars)
                                       collect (cons k (if (stringp v) v (json-string v)))))))))

(defun write-http-environments (file environments &key private)
  "Save ENVIRONMENTS in FILE as JSON; PRIVATE files (secrets) only their owner can read."
  (ensure-directories-exist file)
  (when private
    (ignore-errors (sb-posix:chmod (uiop:native-namestring (uiop:pathname-directory-pathname file)) #o700)))
  (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string (format-json (json-string (cons :object (loop for (name . vars) in environments
                                                                collect (cons name (cons :object vars))))))
                  out)
    (terpri out))
  (when private
    (sb-posix:chmod (uiop:native-namestring file) #o600))
  file)

(defun http-environment-names (environments)
  (remove "$shared" (mapcar #'car environments) :test #'string=))

(defun http-environment-variables (environments name)
  "The variables of environment NAME (nil: none chosen), over the $shared ones."
  (append (and name (cdr (assoc name environments :test #'string=)))
          (cdr (assoc "$shared" environments :test #'string=))))

(defun set-http-environment-variable (environments env name value)
  "ENVIRONMENTS with NAME set to VALUE in environment ENV (made if need be)."
  (let* ((environments (copy-alist environments))
         (entry (assoc env environments :test #'string=)))
    (if entry
        (setf (cdr entry) (append (remove name (cdr entry) :key #'car :test #'string=) (list (cons name value))))
        (setf environments (append environments (list (list env (cons name value))))))
    environments))

(defun http-secrets-file-name (project)
  "The name of PROJECT's secrets file: the folder's name and a hash of its path."
  (format nil "~a.json" (project-file-key project)))

;;; Authentication, as headers

(defparameter *api-key-headers* '("x-api-key" "api-key" "apikey" "x-auth-token" "x-access-token"))

(defun http-request-auth (request)
  "How REQUEST authenticates, from its headers: :bearer, :basic or :api-key
(or nil), the {{variable}} holding the secret (nil if the value is typed
in), and the header it's in."
  (let ((authorization (http-request-header request "Authorization")))
    (flet ((variable (value)
             (multiple-value-bind (m g) (ppcre:scan-to-strings "^\\s*\\{\\{\\s*([$\\w.-]+)\\s*\\}\\}\\s*$" value)
               (and m (aref g 0)))))
      (cond (authorization
             (multiple-value-bind (m g) (ppcre:scan-to-strings "(?i)^(bearer|basic)\\s+(.*)$" authorization)
               (if m
                   (values (if (string-equal (aref g 0) "basic") :basic :bearer) (variable (aref g 1)) "Authorization")
                   (values nil nil nil))))
            (t (let ((header (find-if (lambda (h) (member (car h) *api-key-headers* :test #'string-equal))
                                      (http-request-headers request))))
                 (if header
                     (values :api-key (variable (cdr header)) (car header))
                     (values nil nil nil))))))))

(defun auth-header (type variable &key (header "X-API-Key"))
  "The header (name . value) for TYPE (:bearer, :basic, :api-key) with its secret in {{VARIABLE}}."
  (ecase type
    (:bearer (cons "Authorization" (format nil "Bearer {{~a}}" variable)))
    (:basic (cons "Authorization" (format nil "Basic {{~a}}" variable)))
    (:api-key (cons header (format nil "{{~a}}" variable)))))

;;; Secrets left in a request

(defparameter *secret-header-names*
  '("authorization" "proxy-authorization" "x-api-key" "api-key" "apikey" "x-auth-token" "x-access-token"
    "cookie" "x-csrf-token"))

(defparameter *secret-key-scanner*
  (ppcre:create-scanner "(?i)(pass(word|wd)?|secret|token|api[_-]?key|access[_-]?key|private[_-]?key|credential)"))

(defun variable-free-p (string) (not (search "{{" string)))

(defun secret-variable-name (key)
  (let ((name (string-trim "_" (ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase key) "_"))))
    (if (string= name "") "secret" name)))

(defun find-literal-secrets (request)
  "What in REQUEST looks like a secret typed in, not a {{variable}}: a list of
plists (:where :header/:query/:body, :key, :value, :variable — a name for it)."
  (let ((found '()))
    (loop for (name . value) in (http-request-headers request)
          when (and (member name *secret-header-names* :test #'string-equal) (variable-free-p value))
            do (multiple-value-bind (match groups) (ppcre:scan-to-strings "(?i)^(bearer|basic|token|digest)\\s+(\\S+)$" value)
                 (push (list :where :header :key name :value (if match (aref groups 1) value)
                             :variable (cond ((not match) (secret-variable-name name))
                                             ((string-equal (aref groups 0) "basic") "basic_auth")
                                             (t "token")))
                       found)))
    (let ((query (position #\? (http-request-url request))))
      (when query
        (dolist (pair (uiop:split-string (subseq (http-request-url request) (1+ query)) :separator "&"))
          (let ((equals (position #\= pair)))
            (when (and equals (ppcre:scan *secret-key-scanner* (subseq pair 0 equals))
                       (variable-free-p (subseq pair (1+ equals))) (< (1+ equals) (length pair)))
              (push (list :where :query :key (subseq pair 0 equals) :value (subseq pair (1+ equals))
                          :variable (secret-variable-name (subseq pair 0 equals)))
                    found))))))
    (let ((json (ignore-errors (parse-json (http-request-body request)))))
      (labels ((walk (x)
                 (cond ((jobject-p x)
                        (loop for (k . v) in (cdr x)
                              do (if (and (stringp v) (plusp (length v)) (variable-free-p v)
                                          (ppcre:scan *secret-key-scanner* k))
                                     (push (list :where :body :key k :value v :variable (secret-variable-name k)) found)
                                     (walk v))))
                       ((vectorp x) (map nil #'walk x)))))
        (when (or (jobject-p json) (vectorp json)) (walk json))))
    ;; Different values need different names.
    (let ((used '()))
      (dolist (secret (reverse found))
        (let ((name (getf secret :variable)))
          (loop for i from 2 while (member name used :test #'string=)
                do (setf name (format nil "~a_~d" (getf secret :variable) i)))
          (push name used)
          (setf (getf secret :variable) name))))
    (reverse found)))

(defun move-secrets-out (request secrets)
  "A copy of REQUEST with each of SECRETS (from FIND-LITERAL-SECRETS) replaced by its {{variable}}."
  (let ((copy (copy-http-request request)))
    (dolist (secret secrets copy)
      (let ((value (getf secret :value))
            (variable (format nil "{{~a}}" (getf secret :variable))))
        (ecase (getf secret :where)
          (:header (setf (http-request-headers copy)
                         (mapcar (lambda (h) (if (string-equal (car h) (getf secret :key))
                                                 (cons (car h) (replace-unique-substring (cdr h) value variable))
                                                 h))
                                 (http-request-headers copy))))
          (:query (setf (http-request-url copy)
                        (replace-unique-substring (http-request-url copy)
                                                  (format nil "~a=~a" (getf secret :key) value)
                                                  (format nil "~a=~a" (getf secret :key) variable))))
          (:body (setf (http-request-body copy)
                       (replace-unique-substring (http-request-body copy)
                                                 (json-string value) (format nil "\"~a\"" variable)))))))))

(defun replace-unique-substring (string old new)
  (let ((at (search old string)))
    (if at (concatenate 'string (subseq string 0 at) new (subseq string (+ at (length old)))) string)))

;;; Capturing values from a response

(defun json-path-value (json path)
  "The value at PATH in JSON: $ is the whole of it, .key or ['key'] a member,
[n] an element (from 0). A path without $ is taken to start with $. (token
is $.token). Returns the value and whether it was found."
  (let ((i 0) (n (length path)) (value json))
    (flet ((fail () (return-from json-path-value (values nil nil))))
      (cond ((and (> n 0) (char= (char path 0) #\$)) (setf i 1))
            ((and (> n 0) (not (find (char path 0) ".["))) (setf path (concatenate 'string "." path) n (length path))))
      (loop while (< i n)
            do (let ((c (char path i)))
                 (cond ((char= c #\.)
                        (let ((end (or (position-if (lambda (c) (find c ".[")) path :start (1+ i)) n)))
                          (unless (jobject-p value) (fail))
                          (let ((entry (assoc (subseq path (1+ i) end) (cdr value) :test #'string=)))
                            (unless entry (fail))
                            (setf value (cdr entry) i end))))
                       ((char= c #\[)
                        (let ((close (or (position #\] path :start i) (fail)))
                              (inside (string-trim " " (subseq path (1+ i) (position #\] path :start i)))))
                          (cond ((and (plusp (length inside)) (find (char inside 0) "'\""))
                                 (unless (jobject-p value) (fail))
                                 (let ((entry (assoc (string-trim "'\"" inside) (cdr value) :test #'string=)))
                                   (unless entry (fail))
                                   (setf value (cdr entry))))
                                (t (let ((index (ignore-errors (parse-integer inside))))
                                     (unless (and index (vectorp value) (< -1 index (length value))) (fail))
                                     (setf value (aref value index)))))
                          (setf i (1+ close))))
                       (t (fail)))))
      (values value t))))

(defun capture-text (value)
  (cond ((stringp value) value)
        ((eq value t) "true")
        ((eq value :false) "false")
        ((eq value :null) "null")
        ((numberp value) (princ-to-string value))
        (t (json-string value))))

(defun capture-response-values (response captures)
  "The values CAPTURES ((name . path) each) take from RESPONSE: a JSON path
into its body, \"header Name\", or \"status\". Returns the (name . value)
found and the names that weren't."
  (let ((json :unparsed) (found '()) (missing '()))
    (loop for (name . path) in captures
          do (let ((path (string-trim " " path)))
               (multiple-value-bind (value ok)
                   (cond ((string-equal path "status")
                          (values (princ-to-string (http-response-status response)) t))
                         ((and (> (length path) 7) (string-equal "header " path :end2 7))
                          (let ((entry (assoc (string-trim " " (subseq path 7)) (http-response-headers response)
                                              :test #'string-equal)))
                            (values (cdr entry) (and entry t))))
                         (t (when (eq json :unparsed)
                              (setf json (ignore-errors (parse-json (http-response-body response)))))
                            (if json (json-path-value json path) (values nil nil))))
                 (if ok
                     (push (cons name (capture-text value)) found)
                     (push name missing)))))
    (values (nreverse found) (nreverse missing))))

;;; Query parameters

(defun percent-decode (string)
  "STRING with %XX escapes (UTF-8) and + (a space) decoded; malformed escapes are kept."
  (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t))
        (i 0) (n (length string)))
    (loop while (< i n)
          do (let ((c (char string i)))
               (cond ((and (char= c #\%) (<= (+ i 3) n) (digit-char-p (char string (+ i 1)) 16)
                           (digit-char-p (char string (+ i 2)) 16))
                      (vector-push-extend (parse-integer string :start (1+ i) :end (+ i 3) :radix 16) octets)
                      (incf i 3))
                     ((char= c #\+) (vector-push-extend 32 octets) (incf i))
                     (t (loop for o across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                              do (vector-push-extend o octets))
                        (incf i)))))
    (handler-case (sb-ext:octets-to-string (coerce octets '(vector (unsigned-byte 8))) :external-format :utf-8)
      (error () string))))

(defun percent-encode-component (string)
  "STRING for a query's name or value: percent-encoded (UTF-8), but {{variables}} left as they are."
  (with-output-to-string (out)
    (let ((i 0) (n (length string)))
      (loop while (< i n)
            do (let ((variable (and (< (1+ i) n) (string= "{{" string :start2 i :end2 (+ i 2))
                                    (search "}}" string :start2 (+ i 2)))))
                 (if variable
                     (progn (write-string string out :start i :end (+ variable 2))
                            (setf i (+ variable 2)))
                     (let ((c (char string i)))
                       (if (or (alphanumericp c) (find c "-._~") (and (< (char-code c) 128) (find c "/:@!$'()*,;")))
                           (if (< (char-code c) 128) (write-char c out)
                               (loop for o across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                                     do (format out "%~2,'0X" o)))
                           (loop for o across (sb-ext:string-to-octets (string c) :external-format :utf-8)
                                 do (format out "%~2,'0X" o)))
                       (incf i))))))))

(defun url-query-parameters (url)
  "URL's query parameters, (name . value) each, decoded ({{variables}} as they are)."
  (let* ((hash (position #\# url))
         (query (position #\? url :end hash)))
    (when query
      (loop for pair in (uiop:split-string (subseq url (1+ query) hash) :separator "&")
            unless (string= pair "")
              collect (let ((equals (position #\= pair)))
                        (if equals
                            (cons (percent-decode (subseq pair 0 equals)) (percent-decode (subseq pair (1+ equals))))
                            (cons (percent-decode pair) "")))))))

(defun url-with-query-parameters (url parameters)
  "URL with its query made of PARAMETERS ((name . value) each; empty names left out), encoded."
  (let* ((hash (position #\# url))
         (query (position #\? url :end hash))
         (base (subseq url 0 (or query hash (length url))))
         (fragment (if hash (subseq url hash) ""))
         (pairs (loop for (name . value) in parameters
                      when (plusp (length name))
                        collect (if (string= value "")
                                    (percent-encode-component name)
                                    (format nil "~a=~a" (percent-encode-component name) (percent-encode-component value))))))
    (format nil "~a~@[?~{~a~^&~}~]~a" base pairs fragment)))

;;; Bodies: text, a form, multipart parts, or a file

(defun http-request-content-type (request)
  (string-downcase (or (http-request-header request "Content-Type") "")))

(defun multipart-boundary (request)
  "The boundary of REQUEST's multipart/form-data body, or nil if it isn't one."
  (let ((type (or (http-request-header request "Content-Type") "")))
    (when (search "multipart/form-data" (string-downcase type))
      (multiple-value-bind (m g) (ppcre:scan-to-strings "(?i)boundary=\"?([^\";]+)\"?" type)
        (and m (aref g 0))))))

(defun form-body-pairs (body)
  "An application/x-www-form-urlencoded BODY's (name . value) pairs, decoded."
  (url-query-parameters (concatenate 'string "?" (string-trim '(#\Space #\Newline #\Return #\Tab) body))))

(defun pairs-form-body (pairs)
  "PAIRS ((name . value) each) as an application/x-www-form-urlencoded body."
  (let ((url (url-with-query-parameters "" pairs)))
    (if (plusp (length url)) (subseq url 1) "")))

(defun body-file-reference (body)
  "The file a body of the form \"< path\" sends, or nil."
  (let ((body (string-trim '(#\Space #\Newline #\Return #\Tab) body)))
    (and (uiop:string-prefix-p "< " body) (not (find #\Newline body))
         (string-trim " " (subseq body 2)))))

(defun parse-multipart-body (body boundary)
  "BODY's parts, plists of :name and either :value (text) or :file (a path,
from a \"< path\" line), with :filename and :type when given."
  (let ((parts '()) (part nil) (state nil) (content '()))
    (flet ((finish ()
             (when part
               (let* ((text (string-right-trim '(#\Newline #\Return) (format nil "~{~a~^~%~}" (reverse content))))
                      (file (body-file-reference text)))
                 ;; Always in this order: :name, :value or :file, :filename, :type.
                 (push (append (list :name (getf part :name))
                               (if file (list :file file) (list :value text))
                               (and (getf part :filename) (list :filename (getf part :filename)))
                               (and (getf part :type) (list :type (getf part :type))))
                       parts)))
             (setf part nil content '())))
      (dolist (line (uiop:split-string body :separator '(#\Newline)))
        (let ((line (string-right-trim '(#\Return) line)))
          (cond ((string= line (format nil "--~a--" boundary)) (finish) (setf state :done))
                ((string= line (format nil "--~a" boundary)) (finish) (setf part (list :name "") state :headers))
                ((eq state :headers)
                 (cond ((string= (string-trim " " line) "") (setf state :content))
                       ((uiop:string-prefix-p "content-disposition:" (string-downcase line))
                        (multiple-value-bind (m g) (ppcre:scan-to-strings "(?i)\\bname=\"([^\"]*)\"" line)
                          (when m (setf (getf part :name) (aref g 0))))
                        (multiple-value-bind (m g) (ppcre:scan-to-strings "(?i)filename=\"([^\"]*)\"" line)
                          (when m (setf (getf part :filename) (aref g 0)))))
                       ((uiop:string-prefix-p "content-type:" (string-downcase line))
                        (setf (getf part :type) (string-trim " " (subseq line (1+ (position #\: line))))))))
                ((eq state :content) (push line content)))))
      (finish))
    (nreverse parts)))

(defun render-multipart-body (parts boundary)
  "PARTS (as PARSE-MULTIPART-BODY gives them) as a multipart body with BOUNDARY."
  (with-output-to-string (out)
    (dolist (part parts)
      (format out "--~a~%Content-Disposition: form-data; name=\"~a\"~@[; filename=\"~a\"~]~%~@[Content-Type: ~a~%~]~%~a~%"
              boundary (getf part :name)
              (or (getf part :filename) (and (getf part :file) (file-namestring (getf part :file))))
              (getf part :type)
              (if (getf part :file) (format nil "< ~a" (getf part :file)) (or (getf part :value) ""))))
    (format out "--~a--" boundary)))

(defun body-path (path directory)
  "PATH (from a body's \"< path\") as a full path: relative to DIRECTORY, the .http file's."
  (let ((path (expand-tilde path)))
    (loop while (uiop:string-prefix-p "./" path) do (setf path (subseq path 2)))
    (uiop:native-namestring
     (if (and directory (not (uiop:absolute-pathname-p (uiop:parse-native-namestring path))))
         (merge-pathnames (uiop:parse-native-namestring path) (uiop:ensure-directory-pathname directory))
         (uiop:parse-native-namestring path)))))

(defun relative-body-path (path directory)
  "PATH as written in a .http file in DIRECTORY: ./name when it's under it."
  (let ((base (and directory (uiop:native-namestring (uiop:ensure-directory-pathname directory)))))
    (if (and base (uiop:string-prefix-p base path))
        (concatenate 'string "./" (subseq path (length base)))
        path)))

;;; curl

(defun curl-config-string (string)
  "STRING quoted for a curl config file."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for c across string
          do (case c
               (#\\ (write-string "\\\\" out))
               (#\" (write-string "\\\"" out))
               (#\Newline (write-string "\\n" out))
               (#\Return (write-string "\\r" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char c out))))
    (write-char #\" out)))

(defun curl-config (request &key body-file header-file output-file directory cookie-jar)
  "The curl config (curl -K) that sends REQUEST (its variables already put in).
Files a body names (\"< path\") are relative to DIRECTORY."
  (let ((options (http-request-options request))
        (boundary (multipart-boundary request))
        (file (body-file-reference (http-request-body request))))
    (with-output-to-string (out)
      (flet ((option (name &optional value)
               (if value
                   (format out "~a = ~a~%" name (curl-config-string value))
                   (format out "~a~%" name))))
        (option "url" (http-request-url request))
        (if (string= (http-request-method request) "HEAD")
            (option "head")
            (option "request" (http-request-method request)))
        (loop for (name . value) in (http-request-headers request)
              ;; curl writes a multipart body's Content-Type, with its own boundary.
              unless (and boundary (string-equal name "Content-Type"))
                do (option "header" (format nil "~a: ~a" name value)))
        (cond (boundary
               (dolist (part (parse-multipart-body (http-request-body request) boundary))
                 (if (getf part :file)
                     (option "form" (format nil "~a=@\"~a\"~@[;filename=\"~a\"~]~@[;type=~a~]" (getf part :name)
                                            (body-path (getf part :file) directory)
                                            (getf part :filename) (getf part :type)))
                     (option "form-string" (format nil "~a=~a" (getf part :name) (getf part :value))))))
              (file (option "data-binary" (format nil "@~a" (body-path file directory))))
              ((and body-file (plusp (length (http-request-body request))))
               (option "data-binary" (format nil "@~a" (uiop:native-namestring body-file)))))
        ;; Cookies the server sets are kept in COOKIE-JAR and sent back.
        (when (and cookie-jar (not (getf options :no-cookie-jar)))
          (option "cookie" (uiop:native-namestring cookie-jar))
          (option "cookie-jar" (uiop:native-namestring cookie-jar)))
        (when (http-request-follows-redirects-p request)
          (option "location"))
        (when (getf options :insecure) (option "insecure"))
        (option "max-time" (princ-to-string (or (getf options :timeout) 60)))
        (option "silent")
        (option "show-error")
        (when header-file (option "dump-header" (uiop:native-namestring header-file)))
        (when output-file (option "output" (uiop:native-namestring output-file)))
        (option "write-out" "%{json}")))))

;;; Redirects. curl sends a request's headers again to wherever a redirect
;;; goes, another host too; it leaves out only Authorization and Cookie, and
;;; only in newer versions. So redirects are followed only when every header
;;; is one that can't carry a secret.

(defparameter *redirect-safe-headers*
  '("Accept" "Accept-Encoding" "Accept-Language" "Cache-Control" "Content-Language"
    "Content-Type" "If-Match" "If-Modified-Since" "If-None-Match" "If-Unmodified-Since"
    "Origin" "Pragma" "Range" "Referer" "User-Agent")
  "Request headers that are safe to send to another host after a redirect.")

(defun http-redirect-blocking-headers (request)
  "The names of REQUEST's headers that keep its redirects from being followed."
  (loop for (name) in (http-request-headers request)
        unless (member name *redirect-safe-headers* :test #'string-equal)
          collect name))

(defun http-request-follows-redirects-p (request)
  (and (not (getf (http-request-options request) :no-redirect))
       (null (http-redirect-blocking-headers request))))

(defstruct (http-response (:conc-name http-response-))
  (status 0) (reason "") (headers '()) (body "") (size 0) (milliseconds 0)
  (content-type nil) (url nil) (error nil))

(defun parse-response-headers (text)
  "The status, reason and headers of the last response in TEXT (curl's
dump of them: after redirects, there is one block for each response)."
  (let ((blocks (ppcre:split "\\r?\\n\\r?\\n" (string-trim '(#\Return #\Newline) text))))
    (let* ((last (car (last (remove-if-not (lambda (b) (uiop:string-prefix-p "HTTP" b)) blocks))))
           (lines (and last (remove "" (mapcar (lambda (l) (string-right-trim '(#\Return) l))
                                               (uiop:split-string last :separator '(#\Newline)))
                                   :test #'string=))))
      (if (null lines)
          (values 0 "" '())
          (multiple-value-bind (match groups) (ppcre:scan-to-strings "^HTTP/\\S+\\s+(\\d+)\\s*(.*)$" (first lines))
            (values (if match (parse-integer (aref groups 0)) 0)
                    (if match (aref groups 1) "")
                    (loop for line in (rest lines)
                          for colon = (position #\: line)
                          when colon collect (cons (subseq line 0 colon) (string-trim " " (subseq line (1+ colon)))))))))))

(defun decode-response-body (octets)
  (handler-case (sb-ext:octets-to-string octets :external-format :utf-8)
    (error () (sb-ext:octets-to-string octets :external-format :latin-1))))

(defstruct (curl-run (:conc-name curl-run-)) process files)

(defun start-curl (request &key directory cookie-jar)
  "Start curl sending REQUEST (variables put in); files its body names are
relative to DIRECTORY. FINISH-CURL waits for it."
  (let* ((work (uiop:ensure-directory-pathname
                (merge-pathnames (format nil "cadre-curl-~a/" (subseq (random-token) 0 12)) (uiop:temporary-directory))))
         (body (merge-pathnames "request-body" work))
         (headers (merge-pathnames "response-headers" work))
         (output (merge-pathnames "response-body" work)))
    (ensure-directories-exist work)
    (sb-posix:chmod (uiop:native-namestring work) #o700)
    (when cookie-jar
      (ensure-directories-exist cookie-jar)
      (ignore-errors (sb-posix:chmod (uiop:native-namestring (uiop:pathname-directory-pathname cookie-jar)) #o700)))
    (when (plusp (length (http-request-body request)))
      (with-open-file (out body :direction :output :external-format :utf-8)
        (write-string (http-request-body request) out)))
    (let ((process (uiop:launch-program (list "curl" "--config" "-") :input :stream :output :stream
                                                                      :error-output :stream)))
      (with-open-stream (in (uiop:process-info-input process))
        (write-string (curl-config request :body-file body :header-file headers :output-file output
                                           :directory directory :cookie-jar cookie-jar)
                      in))
      (make-curl-run :process process :files (list work headers output)))))

(defun finish-curl (run)
  "Wait for RUN's curl; its HTTP-RESPONSE."
  (destructuring-bind (directory headers output) (curl-run-files run)
    (unwind-protect
         (let* ((process (curl-run-process run))
                (stdout (uiop:slurp-stream-string (uiop:process-info-output process)))
                (stderr (uiop:slurp-stream-string (uiop:process-info-error-output process)))
                (code (uiop:wait-process process))
                (json (ignore-errors (parse-json stdout)))
                (octets (if (probe-file output)
                            (with-open-file (in output :element-type '(unsigned-byte 8))
                              (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                                (read-sequence v in) v))
                            (make-array 0 :element-type '(unsigned-byte 8)))))
           (multiple-value-bind (status reason header-list)
               (if (probe-file headers) (parse-response-headers (uiop:read-file-string headers)) (values 0 "" '()))
             (make-http-response
              :status (if (plusp status) status (or (jget json "http_code") 0))
              :reason reason :headers header-list
              :body (decode-response-body octets) :size (length octets)
              :milliseconds (round (* 1000 (or (jget json "time_total") 0)))
              :content-type (let ((ct (jget json "content_type"))) (and (stringp ct) ct))
              :url (jget json "url_effective")
              :error (cond ((member code '(0 nil)) nil)
                           ((not (eql code 0))
                            (let ((message (or (let ((m (jget json "errormsg"))) (and (stringp m) m))
                                               (string-trim '(#\Newline #\Space) stderr))))
                              (if (plusp (length message)) message (format nil "curl failed (~d)" code))))))))
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))))

(defun cancel-curl (run)
  (ignore-errors (uiop:terminate-process (curl-run-process run))))

(defun send-http-request (request &key directory cookie-jar)
  "Send REQUEST (variables put in) and wait for its response."
  (finish-curl (start-curl request :directory directory :cookie-jar cookie-jar)))

;;; Cookie jars (curl's, in Netscape's format)

(defun parse-cookie-jar (text)
  "The cookies in TEXT, a cookie jar curl wrote: plists of :domain :path
:secure :expires (universal time, or nil for the session) :name :value :http-only."
  (loop for line in (uiop:split-string text :separator '(#\Newline))
        for http-only = (uiop:string-prefix-p "#HttpOnly_" line)
        for content = (string-right-trim '(#\Return) (if http-only (subseq line 10) line))
        for fields = (uiop:split-string content :separator '(#\Tab))
        when (and (plusp (length content)) (char/= (char content 0) #\#) (= 7 (length fields)))
          collect (destructuring-bind (domain subdomains path secure expires name value) fields
                    (declare (ignore subdomains))
                    (let ((seconds (ignore-errors (parse-integer expires))))
                      (list :domain domain :path path :secure (string-equal secure "TRUE")
                            :expires (and seconds (plusp seconds)
                                          (+ seconds #.(encode-universal-time 0 0 0 1 1 1970 0)))
                            :name name :value value :http-only http-only)))))

(defun read-cookie-jar (file)
  (if (probe-file file) (parse-cookie-jar (uiop:read-file-string file)) '()))

;;; Copying and pasting curl command lines

(defun shell-quote (string)
  (if (and (plusp (length string)) (every (lambda (c) (or (alphanumericp c) (find c "-_./:=@,+%"))) string))
      string
      (format nil "'~a'" (ppcre:regex-replace-all "'" string "'\\\\''"))))

(defun http-request-curl-command (request &key directory)
  "A curl command line that sends REQUEST (files its body names relative to DIRECTORY)."
  (let ((options (http-request-options request)))
    (with-output-to-string (out)
      (format out "curl")
      (cond ((string= (http-request-method request) "HEAD") (format out " --head"))
            ((not (string= (http-request-method request) "GET"))
             (format out " -X ~a" (http-request-method request))))
      (format out " ~a" (shell-quote (http-request-url request)))
      (let ((boundary (multipart-boundary request))
            (file (body-file-reference (http-request-body request))))
        (loop for (name . value) in (http-request-headers request)
              unless (and boundary (string-equal name "Content-Type"))
                do (format out " \\~%  -H ~a" (shell-quote (format nil "~a: ~a" name value))))
        (cond (boundary
               (dolist (part (parse-multipart-body (http-request-body request) boundary))
                 (format out " \\~%  -F ~a"
                         (shell-quote (if (getf part :file)
                                          (format nil "~a=@~a~@[;type=~a~]" (getf part :name)
                                                  (body-path (getf part :file) directory) (getf part :type))
                                          (format nil "~a=~a" (getf part :name) (getf part :value)))))))
              (file (format out " \\~%  --data-binary ~a" (shell-quote (format nil "@~a" (body-path file directory)))))
              ((plusp (length (http-request-body request)))
               (format out " \\~%  --data-raw ~a" (shell-quote (http-request-body request))))))
      (when (http-request-follows-redirects-p request)
        (format out " -L"))
      (when (getf options :insecure) (format out " -k")))))

(defun shell-words (text)
  "TEXT split into words as a POSIX shell would: quotes, backslashes, and
backslash-newline continuing the line."
  (let ((words '()) (chars '()) (in-word nil) (i 0) (n (length text)))
    (flet ((add (c) (push c chars) (setf in-word t))
           (end-word ()
             (when in-word (push (coerce (reverse chars) 'string) words))
             (setf chars '() in-word nil)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((char= c #\\)
                        (cond ((and (< (1+ i) n) (char= (char text (1+ i)) #\Newline)) (incf i 2))
                              ((and (< (+ 2 i) n) (char= (char text (1+ i)) #\Return) (char= (char text (+ 2 i)) #\Newline))
                               (incf i 3))
                              ((< (1+ i) n) (add (char text (1+ i))) (incf i 2))
                              (t (incf i))))
                       ((char= c #\')
                        (setf in-word t)
                        (let ((close (or (position #\' text :start (1+ i)) n)))
                          (loop for j from (1+ i) below close do (add (char text j)))
                          (setf i (1+ close))))
                       ((char= c #\")
                        (setf in-word t)
                        (incf i)
                        (loop while (and (< i n) (char/= (char text i) #\"))
                              do (if (and (char= (char text i) #\\) (< (1+ i) n) (find (char text (1+ i)) "\"\\$`"))
                                     (progn (add (char text (1+ i))) (incf i 2))
                                     (progn (add (char text i)) (incf i))))
                        (incf i))
                       ((member c '(#\Space #\Tab #\Newline #\Return)) (end-word) (incf i))
                       (t (add c) (incf i)))))
      (end-word))
    (nreverse words)))

(defun parse-curl-command (text)
  "The request a curl command line TEXT sends, or nil if it isn't one."
  (let ((words (shell-words (string-trim '(#\Space #\Newline #\Tab #\Return) text))))
    (when (and words (string= (car (last (uiop:split-string (first words) :separator "/"))) "curl"))
      (let ((request (make-http-request))
            (method nil) (data '()) (get nil) (head nil) (forms '())
            (words (rest words)))
        (flet ((header (name value)
                 (setf (http-request-headers request)
                       (append (remove name (http-request-headers request) :key #'car :test #'string-equal)
                               (list (cons name value))))))
          (loop while words
                do (let ((word (pop words)))
                     (flet ((value () (or (pop words) "")))
                       (cond ((member word '("-X" "--request") :test #'string=) (setf method (string-upcase (value))))
                             ((uiop:string-prefix-p "-X" word) (setf method (string-upcase (subseq word 2))))
                             ((member word '("-H" "--header") :test #'string=)
                              (let* ((h (value)) (colon (position #\: h)))
                                (when colon (header (string-trim " " (subseq h 0 colon)) (string-trim " " (subseq h (1+ colon)))))))
                             ((member word '("-d" "--data" "--data-raw" "--data-binary" "--data-ascii" "--data-urlencode")
                                      :test #'string=)
                              ;; @file: curl sends the file; a body of "< file" does.
                              (let ((v (value)))
                                (push (if (and (uiop:string-prefix-p "@" v) (string/= word "--data-raw"))
                                          (format nil "< ~a" (subseq v 1))
                                          v)
                                      data)))
                             ((string= word "--json")
                              (push (value) data)
                              (unless (http-request-header request "Content-Type") (header "Content-Type" "application/json"))
                              (unless (http-request-header request "Accept") (header "Accept" "application/json")))
                             ((member word '("-F" "--form" "--form-string") :test #'string=)
                              (let* ((field (value)) (equals (position #\= field))
                                     (name (if equals (subseq field 0 equals) field))
                                     (value (if equals (subseq field (1+ equals)) "")))
                                (push (if (and (uiop:string-prefix-p "@" value) (string/= word "--form-string"))
                                          (let ((semi (position #\; value)))
                                            (list* :name name :file (subseq value 1 semi)
                                                   (multiple-value-bind (m g) (ppcre:scan-to-strings ";type=([^;]+)" value)
                                                     (and m (list :type (aref g 0))))))
                                          (list :name name :value value))
                                      forms)))
                             ((member word '("-u" "--user") :test #'string=)
                              (header "Authorization" (format nil "Basic ~a" (base64-string (value)))))
                             ((member word '("-A" "--user-agent") :test #'string=) (header "User-Agent" (value)))
                             ((member word '("-b" "--cookie") :test #'string=) (header "Cookie" (value)))
                             ((member word '("-e" "--referer") :test #'string=) (header "Referer" (value)))
                             ((string= word "--url") (setf (http-request-url request) (value)))
                             ((member word '("-G" "--get") :test #'string=) (setf get t))
                             ((member word '("-I" "--head") :test #'string=) (setf head t))
                             ((member word '("-k" "--insecure") :test #'string=)
                              (setf (getf (http-request-options request) :insecure) t))
                             ((member word '("-m" "--max-time") :test #'string=)
                              (let ((s (ignore-errors (parse-integer (value) :junk-allowed t))))
                                (when s (setf (getf (http-request-options request) :timeout) s))))
                             ;; Options that take a value Cadre doesn't use.
                             ((member word '("-o" "--output" "-w" "--write-out" "-x" "--proxy" "-c" "--cookie-jar"
                                             "--connect-timeout" "-D" "--dump-header" "--retry"
                                             "--cacert" "--cert" "--key" "-E")
                                      :test #'string=)
                              (pop words))
                             ((uiop:string-prefix-p "-" word))
                             ((string= (http-request-url request) "") (setf (http-request-url request) word))))))
          (setf data (reverse data))
          (when forms
            (setf (http-request-body request) (render-multipart-body (reverse forms) "CadreBoundary"))
            (header "Content-Type" "multipart/form-data; boundary=CadreBoundary"))
          (cond (get
                 (when data
                   (setf (http-request-url request)
                         (format nil "~a~:[?~;&~]~{~a~^&~}" (http-request-url request)
                                 (find #\? (http-request-url request)) data))))
                (data (setf (http-request-body request) (format nil "~{~a~^&~}" data))
                      (unless (or (http-request-header request "Content-Type") (string= (http-request-body request) "")
                                  (member (char (string-left-trim " " (http-request-body request)) 0) '(#\{ #\[)))
                        (header "Content-Type" "application/x-www-form-urlencoded"))))
          (setf (http-request-method request)
                (cond (method) (head "HEAD") ((and (or data forms) (not get)) "POST") (t "GET")))
          (when (and (not (string= (http-request-url request) ""))
                     (not (string= (http-request-body request) ""))
                     (char= (char (string-left-trim " " (http-request-body request)) 0) #\{)
                     (not (http-request-header request "Content-Type")))
            (header "Content-Type" "application/json"))
          (and (plusp (length (http-request-url request))) request))))))

(defun base64-string (string)
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"))
    (with-output-to-string (out)
      (loop for i from 0 below (length octets) by 3
            do (let* ((n (length octets))
                      (b0 (aref octets i))
                      (b1 (if (< (+ i 1) n) (aref octets (+ i 1)) 0))
                      (b2 (if (< (+ i 2) n) (aref octets (+ i 2)) 0))
                      (word (logior (ash b0 16) (ash b1 8) b2)))
                 (write-char (char alphabet (ldb (byte 6 18) word)) out)
                 (write-char (char alphabet (ldb (byte 6 12) word)) out)
                 (write-char (if (< (+ i 1) n) (char alphabet (ldb (byte 6 6) word)) #\=) out)
                 (write-char (if (< (+ i 2) n) (char alphabet (ldb (byte 6 0) word)) #\=) out))))))

(defun response-body-mode (response)
  "The major mode RESPONSE's body is shown in: JSON, XML or HTML by its
Content-Type, or else by how it starts; plain text otherwise."
  (let ((type (string-downcase (or (http-response-content-type response) ""))))
    (cond ((search "json" type) 'json-mode)
          ((search "html" type) 'html-mode)
          ((search "xml" type) 'xml-mode)
          (t (let ((guess (major-mode-for-text (http-response-body response))))
               (if (member guess '(json-mode html-mode xml-mode)) guess 'fundamental-mode))))))

(defun response-body-text (response &key (pretty t))
  "RESPONSE's body to show: with PRETTY, JSON and XML laid out (HTML, often
not well-formed XML, as it came); as it came if it can't be."
  (let ((body (http-response-body response)))
    (or (and pretty (plusp (length body))
             (case (response-body-mode response)
               (json-mode (ignore-errors (and (parse-json body) (format-json body))))
               (xml-mode (ignore-errors (format-xml body)))))
        body)))
