(in-package #:cadre-tests)

;;; MySQL / MariaDB (core database/mysql.lisp), against a MariaDB server
;;; started for the test in a temporary folder. Skipped where MariaDB isn't
;;; installed.

(defun mariadb-program (name)
  (let ((brew (probe-file (format nil "/opt/homebrew/opt/mariadb/bin/~a" name))))
    (and brew (namestring brew))))

(defun call-with-mariadb (function)
  "Call FUNCTION with the port of a fresh MariaDB server (user cadre,
password secret, database shop); nil if MariaDB isn't installed."
  (let ((install (mariadb-program "mariadb-install-db"))
        (server (mariadb-program "mariadbd"))
        (client (mariadb-program "mariadb")))
    (when (and install server client)
      (let* ((dir (uiop:ensure-directory-pathname
                   (merge-pathnames (format nil "cdb-~d/" (random 1000000)) #p"/tmp/")))
             (data (merge-pathnames "data/" dir))
             (socket (namestring (merge-pathnames "s.sock" dir)))
             (port (free-port))
             (process nil))
        (ensure-directories-exist dir)
        (unwind-protect
             (progn
               (uiop:run-program (list install (format nil "--datadir=~a" (namestring data))
                                       "--auth-root-authentication-method=socket" "--skip-test-db")
                                 :output nil :error-output nil)
               (setf process (uiop:launch-program (list server (format nil "--datadir=~a" (namestring data))
                                                        (format nil "--port=~d" port) "--bind-address=127.0.0.1"
                                                        (format nil "--socket=~a" socket) "--skip-name-resolve")
                                                  :output nil :error-output nil))
               (loop repeat 100 until (probe-file socket) do (sleep 0.1))
               (uiop:run-program (list client "-S" socket "-u" (uiop:getenv "USER") "-e"
                                       "CREATE USER 'cadre'@'%' IDENTIFIED BY 'secret'; GRANT ALL ON *.* TO 'cadre'@'%'; CREATE DATABASE shop;")
                                 :output nil :error-output nil)
               (funcall function port))
          (when process
            (ignore-errors (uiop:terminate-process process))
            (ignore-errors (uiop:wait-process process)))
          (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))))

(define-test mysql :parent cadre-tests
  (let ((c:*identifier-quote* #\`))
    (is string= "`a``b`" (c:quote-sql-identifier "a`b")))
  ;; Strings go as hex, so neither quotes nor backslashes in them are read
  ;; as SQL, whatever the server's sql_mode.
  (is string= "SELECT _utf8mb4 X'697427735C', NULL, 3, X'0102', _utf8mb4 X'' FROM t WHERE a = '?'"
      (cadre::my-inline-params "SELECT ?, ?, ?, ?, ? FROM t WHERE a = '?'"
                               (list "it's\\" :null 3 (make-array 2 :element-type '(unsigned-byte 8) :initial-contents '(1 2)) "")))
  (is string= "SELECT _utf8mb4 X'CEBB'" (cadre::my-inline-params "SELECT ?" (list "λ")))
  (is string= "SELECT 1.5" (cadre::my-inline-params "SELECT ?" (list 1.5d0)))
  (is string= "SELECT 1.0e20" (cadre::my-inline-params "SELECT ?" (list 1.0d20)))
  (is string= "SELECT 'a''?''b', /* ? */ 1 # ?
, 'c\\'?', 2" (cadre::my-inline-params "SELECT 'a''?''b', /* ? */ ? # ?
, 'c\\'?', ?" (list 1 2)))
  (call-with-mariadb
   (lambda (port)
     (fail (c:open-mysql :host "127.0.0.1" :port port :user "cadre" :password "wrong" :database "shop") c:editor-error)
     (let* ((connection (c:make-db-connection :name "my" :kind :mysql :host "127.0.0.1" :port port
                                              :database "shop" :user "cadre"))
            (db (c:open-database connection :password "secret")))
       (unwind-protect
            (c:with-db-quoting (db)
              ;; Values with quotes and backslashes come back as they went,
              ;; with the server's default sql_mode (backslashes escape) and
              ;; with NO_BACKSLASH_ESCAPES.
              (dolist (mode '("DEFAULT" "'NO_BACKSLASH_ESCAPES'"))
                (c:db-run db (format nil "SET SESSION sql_mode = ~a" mode))
                (dolist (value (list "it's\\a path" "\\' OR 1=1 -- " "ends in \\" "C:\\new\\table" "a\\\\b" ""))
                  (let ((result (c:db-run db "SELECT ?, 'after'" :params (list value))))
                    (is equal (list value "after") (coerce (first (c:db-result-rows result)) 'list)))))
              (c:db-run db "SET SESSION sql_mode = DEFAULT")
              (c:db-run db "CREATE TABLE people (id int AUTO_INCREMENT PRIMARY KEY, name varchar(40) NOT NULL, age int,
  born date, score decimal(5,2), photo blob);
CREATE TABLE tags (tag varchar(20));
CREATE VIEW adults AS SELECT * FROM people WHERE age >= 18;
CREATE UNIQUE INDEX people_name ON people (name);")
              (dolist (row '(("Ann" 34 "1990-05-01") ("Bo" 12 "2012-01-02") ("Cy" :null :null)))
                (is = 1 (c:db-result-changes (c:db-run db "INSERT INTO people (name, age, born) VALUES (?, ?, ?)" :params row))))
              (is equal '((:name "people" :kind :table) (:name "tags" :kind :table) (:name "adults" :kind :view))
                  (c:db-tables db))
              (is equal '("id" "name" "age" "born" "score" "photo") (mapcar (lambda (c) (getf c :name)) (c:db-columns db "people")))
              (is equal '("id") (c:db-row-key db "people"))
              (is equal nil (c:db-row-key db "tags"))
              (is equal nil (c:db-row-key db "adults"))
              (let ((index (find "people_name" (c:db-indexes db "people") :key (lambda (i) (getf i :name)) :test #'string=)))
                (true (getf index :unique))
                (is equal '("name") (getf index :columns)))
              (true (search "CREATE TABLE `people`" (c:db-create-statement db "people")))
              (multiple-value-bind (sql params) (c:table-select-sql "people" :key '("id") :filters '(("age" . ">20") ("name" . "an")))
                (true (search "`people`" sql))
                (let ((result (c:db-run db sql :params params)))
                  (is equal '("id" "id" "name" "age" "born" "score" "photo") (c:db-result-columns result))
                  (is = 1 (length (c:db-result-rows result)))
                  (is string= "Ann" (aref (first (c:db-result-rows result)) 2))
                  (is string= "1990-05-01" (aref (first (c:db-result-rows result)) 4))))
              ;; A value that would end a backslash-escaped string doesn't
              ;; make the rest of it SQL (it would match every row).
              (is equalp '(#(0)) (c:db-result-rows (c:db-run db "SELECT count(*) FROM people WHERE name = ?"
                                                             :params '("\\' OR 1=1 -- "))))
              (multiple-value-bind (sql params) (c:update-row-sql "people" '("id") #(3) '(("name" . "Cy\\' OR 1=1 -- ")))
                (is = 1 (c:db-result-changes (c:db-run db sql :params params))))
              (is equalp '(#("Ann") #("Bo") #("Cy\\' OR 1=1 -- "))
                  (c:db-result-rows (c:db-run db "SELECT name FROM people ORDER BY id")))
              (multiple-value-bind (sql params) (c:update-row-sql "people" '("id") #(3) '(("name" . "Cy")))
                (is = 1 (c:db-result-changes (c:db-run db sql :params params))))
              (multiple-value-bind (sql params) (c:update-row-sql "people" '("id") #(2) '(("name" . "Bob") ("age" . "13")))
                (is = 1 (c:db-result-changes (c:db-run db sql :params params))))
              (is equalp '(#("Bob" 13)) (c:db-result-rows (c:db-run db "SELECT name, age FROM people WHERE id = 2")))
              ;; Setting what's already there still counts the row, or saving would think it went away
              (multiple-value-bind (sql params) (c:update-row-sql "people" '("id") #(2) '(("age" . "13.0")))
                (is = 1 (c:db-result-changes (c:db-run db sql :params params))))
              (is equalp '(#(3 2.5d0 "1.50")) (c:db-result-rows (c:db-run db "SELECT count(*), CAST(2.5 AS DOUBLE), CAST(1.5 AS DECIMAL(5,2)) FROM people")))
              (is equalp '(#("Cy" :null)) (c:db-result-rows (c:db-run db "SELECT name, age FROM people WHERE id = 3")))
              (let ((result (c:db-run db "SELECT name FROM people ORDER BY id" :limit 2)))
                (is = 2 (length (c:db-result-rows result)))
                (is eq t (c:db-result-more result)))
              (is equalp '(#("λ → ✓")) (c:db-result-rows (c:db-run db "SELECT ?" :params '("λ → ✓"))))
              (fail (c:db-run db "SELECT * FROM nowhere") c:editor-error)
              (is equalp '(#(1)) (c:db-result-rows (c:db-run db "SELECT 1"))))
         (c:db-close db)))
     (let ((read-only (c:open-mysql :host "127.0.0.1" :port port :user "cadre" :password "secret" :database "shop"
                                    :read-only t)))
       (unwind-protect (fail (c:db-run read-only "DELETE FROM people") c:editor-error)
         (c:db-close read-only))))))
