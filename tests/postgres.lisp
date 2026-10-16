(in-package #:cadre-tests)

;;; PostgreSQL (core database/postgres.lisp), against a server started for
;;; the test in a temporary folder. Skipped where PostgreSQL isn't installed.

(defun pg-program (name)
  (let ((brew (probe-file (format nil "/opt/homebrew/opt/postgresql@17/bin/~a" name))))
    (or (and brew (namestring brew))
        (ignore-errors (string-trim '(#\Newline) (uiop:run-program (list "which" name) :output :string))))))

(defun free-port ()
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (unwind-protect (progn (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
                           (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket))))

(defun call-with-postgres (function)
  "Call FUNCTION with the port of a fresh PostgreSQL server (user cadre,
password secret, database postgres); nil if PostgreSQL isn't installed."
  (let ((initdb (pg-program "initdb")) (pg-ctl (pg-program "pg_ctl")))
    (when (and initdb pg-ctl (plusp (length initdb)) (plusp (length pg-ctl)))
      (let* ((dir (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "cadre-pg-~d/" (random 1000000)) (uiop:temporary-directory))))
             (data (merge-pathnames "data/" dir))
             (pwfile (merge-pathnames "pw" dir))
             (port (free-port)))
        (ensure-directories-exist dir)
        (with-open-file (out pwfile :direction :output) (write-line "secret" out))
        (unwind-protect
             (progn
               (uiop:run-program (list initdb "-D" (namestring data) "-U" "cadre" "--auth=scram-sha-256"
                                       (format nil "--pwfile=~a" (namestring pwfile)) "-E" "UTF8" "--no-locale")
                                 :output nil :error-output nil)
               ;; On macOS the server won't start without a locale set.
               (sb-posix:setenv "LC_ALL" "en_US.UTF-8" 1)
               (uiop:run-program (list pg-ctl "-D" (namestring data) "-w" "-l" (namestring (merge-pathnames "log" dir))
                                       "-o" (format nil "-p ~d -k ~a -c listen_addresses=127.0.0.1" port (namestring dir))
                                       "start")
                                 :output nil :error-output nil)
               (funcall function port))
          (ignore-errors (uiop:run-program (list pg-ctl "-D" (namestring data) "-m" "immediate" "stop")
                                           :output nil :error-output nil))
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))

(define-test postgres :parent cadre-tests
  (is string= "SELECT $1, '?', \"a?\" FROM t WHERE x = $2 -- ?" (cadre::pg-placeholders "SELECT ?, '?', \"a?\" FROM t WHERE x = ? -- ?"))
  (is string= "SELECT 'a''?''b', /* ? */ $1, $$ ? $$, $fn$ ? $fn$, E'c\\'?', $2"
      (cadre::pg-placeholders "SELECT 'a''?''b', /* ? */ ?, $$ ? $$, $fn$ ? $fn$, E'c\\'?', ?"))
  (call-with-postgres
   (lambda (port)
     (fail (c:open-postgres :host "127.0.0.1" :port port :user "cadre" :password "wrong") c:editor-error)
     (let* ((connection (c:make-db-connection :name "pg" :kind :postgres :host "127.0.0.1" :port port
                                              :database "postgres" :user "cadre"))
            (db (c:open-database connection :password "secret")))
       (unwind-protect
            (progn
              (c:db-run db "CREATE TABLE people (id serial PRIMARY KEY, name text NOT NULL, age integer,
  born date, score numeric(5,2), active boolean DEFAULT true, photo bytea);
CREATE TABLE tags (tag text);
CREATE VIEW adults AS SELECT * FROM people WHERE age >= 18;
CREATE INDEX people_age ON people (age);")
              (dolist (row '(("Ann" 34 "1990-05-01") ("Bo" 12 "2012-01-02") ("Cy" :null :null)))
                (is = 1 (c:db-result-changes (c:db-run db "INSERT INTO people (name, age, born) VALUES (?, ?, ?::date)" :params row))))
              (is equal '((:name "people" :kind :table) (:name "tags" :kind :table) (:name "adults" :kind :view))
                  (c:db-tables db))
              (is equal '("id" "name" "age" "born" "score" "active" "photo")
                  (mapcar (lambda (c) (getf c :name)) (c:db-columns db "people")))
              (is eq t (getf (second (c:db-columns db "people")) :not-null))
              (is = 1 (getf (first (c:db-columns db "people")) :primary-key))
              (is equal '("id") (c:db-row-key db "people"))
              (is equal nil (c:db-row-key db "tags"))
              (is equal nil (c:db-row-key db "adults"))
              (true (find "people_age" (c:db-indexes db "people") :key (lambda (i) (getf i :name)) :test #'string=))
              (true (search "CREATE VIEW" (c:db-create-statement db "adults")))
              (true (search "PRIMARY KEY (\"id\")" (c:db-create-statement db "people")))
              ;; The table browser's SQL works here too.
              (multiple-value-bind (sql params) (c:table-select-sql "people" :key '("id") :filters '(("age" . ">20") ("name" . "an")))
                (let ((result (c:db-run db sql :params params)))
                  (is equal '("id" "id" "name" "age" "born" "score" "active" "photo") (c:db-result-columns result))
                  (is equalp '(#(1 1 "Ann" 34 "1990-05-01" :null "true" :null)) (c:db-result-rows result))))
              (multiple-value-bind (sql params) (c:update-row-sql "people" '("id") #(2) '(("name" . "Bob") ("age" . "13")))
                (is = 1 (c:db-result-changes (c:db-run db sql :params params))))
              (is equalp '(#("Bob" 13)) (c:db-result-rows (c:db-run db "SELECT name, age FROM people WHERE id = 2")))
              (let ((result (c:db-run db "SELECT name FROM people ORDER BY id" :limit 2)))
                (is = 2 (length (c:db-result-rows result)))
                (is eq t (c:db-result-more result)))
              (is equalp '(#("λ → ✓")) (c:db-result-rows (c:db-run db "SELECT ?::text" :params '("λ → ✓"))))
              (fail (c:db-run db "SELECT * FROM nowhere") c:editor-error)
              ;; Still usable after an error.
              (is equalp '(#(1)) (c:db-result-rows (c:db-run db "SELECT 1"))))
         (c:db-close db)))
     (let ((read-only (c:open-postgres :host "127.0.0.1" :port port :user "cadre" :password "secret" :read-only t)))
       (unwind-protect (fail (c:db-run read-only "DELETE FROM people") c:editor-error)
         (c:db-close read-only))))))

(define-test db-passwords :parent cadre-tests
  (let ((file (merge-pathnames (format nil "cadre-pw-~d/passwords.json" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (c:write-db-password file "id-1" "one")
           (c:write-db-password file "id-2" "two")
           (c:write-db-password file "id-1" "uno")
           (is string= "uno" (c:read-db-password file "id-1"))
           (is string= "two" (c:read-db-password file "id-2"))
           (c:write-db-password file "id-2" "")
           (is equal nil (c:read-db-password file "id-2"))
           (is = #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (uiop:native-namestring file))))))
      (uiop:delete-directory-tree (uiop:pathname-directory-pathname file) :validate t :if-does-not-exist :ignore)))
  ;; A server connection keeps its host, not a path; and never a password.
  (let ((c (c:make-db-connection :name "pg" :kind :postgres :host "db.test" :port 5433 :database "app" :user "me")))
    (is string= "PostgreSQL · me@db.test:5433/app" (c:db-connection-description c))
    (let ((file (merge-pathnames (format nil "cadre-conn-~d.sexp" (random 1000000)) (uiop:temporary-directory))))
      (unwind-protect
           (progn (c:write-db-connections file (list c))
                  (let ((back (first (c:read-db-connections file :project))))
                    (is string= "db.test" (c:db-connection-host back))
                    (is = 5433 (c:db-connection-port back))
                    (is string= "app" (c:db-connection-database back))))
        (uiop:delete-file-if-exists file)))))
