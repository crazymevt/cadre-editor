(in-package #:cadre-tests)

;;; Databases (core database/): SQL text, saved connections, and SQLite

(define-test sql-filters :parent cadre-tests
  (is equal nil (c:parse-column-filter "  "))
  (is equal '(:like "%bob%") (c:parse-column-filter "bob"))
  (is equal '(:> 5) (c:parse-column-filter ">5"))
  (is equal '(:>= 2.5d0) (c:parse-column-filter ">= 2.5"))
  (is equal '(:!= "x") (c:parse-column-filter "<>x"))
  (is equal '(:= "abc") (c:parse-column-filter "=abc"))
  (is equal '(:null) (c:parse-column-filter "NULL"))
  (is equal '(:not-null) (c:parse-column-filter "!null"))
  (multiple-value-bind (sql params) (c:where-sql '(("name" . "bo") ("age" . ">30") ("x" . "")))
    (is string= " WHERE LOWER(CAST(\"name\" AS TEXT)) LIKE LOWER(?) AND \"age\" > ?" sql)
    (is equal '("%bo%" 30) params)))

(define-test sql-builders :parent cadre-tests
  (is string= "\"a\"\"b\"" (c:quote-sql-identifier "a\"b"))
  (multiple-value-bind (sql params)
      (c:table-select-sql "people" :key '(:rowid) :filters '(("name" . "=Ann")) :sort "age" :descending t
                                   :limit 100 :offset 200)
    (is string= "SELECT rowid, \"people\".* FROM \"people\" WHERE \"name\" = ? ORDER BY \"age\" DESC LIMIT 100 OFFSET 200" sql)
    (is equal '("Ann") params))
  (is string= "SELECT \"v\".* FROM \"v\" LIMIT 10" (c:table-select-sql "v" :limit 10 :offset 0))
  (multiple-value-bind (sql params) (c:update-row-sql "t" '("a" "b") #(1 2) '(("x" . "new") ("y" . :null)))
    (is string= "UPDATE \"t\" SET \"x\" = ?, \"y\" = ? WHERE \"a\" = ? AND \"b\" = ?" sql)
    (is equal '("new" :null 1 2) params))
  (is string= "INSERT INTO \"t\" DEFAULT VALUES" (c:insert-row-sql "t" '()))
  (is string= "INSERT INTO \"t\" (\"x\") VALUES (?)" (c:insert-row-sql "t" '(("x" . 1))))
  (is string= "DELETE FROM \"t\" WHERE rowid = ?" (c:delete-row-sql "t" '(:rowid) #(4))))

(define-test sql-statements :parent cadre-tests
  (let ((text "select 1;  -- a; comment
select 'a;b' from \"x;y\";
/* only ; a comment */
  select 3"))
    (is = 3 (length (c:sql-statement-ranges text)))
    (is string= "select 1" (c:sql-statement-at text 3))
    (is string= "-- a; comment
select 'a;b' from \"x;y\"" (c:sql-statement-at text 20))
    (is string= "/* only ; a comment */
  select 3" (c:sql-statement-at text (length text))))
  ;; After the last semicolon: the statement before it.
  (is string= "select 1" (c:sql-statement-at "select 1;  " 11))
  (is equal nil (c:sql-statement-at "  -- nothing" 3))
  (is string= "select 'it''s'" (c:sql-statement-at "select 'it''s'; select 2" 2)))

(define-test db-connections-file :parent cadre-tests
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "cadre-db-test-~d/" (random 1000000)) (uiop:temporary-directory))))
         (file (c:project-db-connections-file dir)))
    (unwind-protect
         (let ((a (c:make-db-connection :name "App" :path "data/app.db" :scope :project))
               (b (c:make-db-connection :name "Other" :path "/tmp/o.db" :read-only t :scope :project)))
           (is equal '() (c:read-db-connections file :project))
           (c:write-db-connections file (list a b))
           (let ((read (c:read-db-connections file :project)))
             (is equal '("App" "Other") (mapcar #'c:db-connection-name read))
             (is string= (c:db-connection-id a) (c:db-connection-id (first read)))
             (is eq t (c:db-connection-read-only (second read))))
           (is equal (merge-pathnames "data/app.db" dir) (c:db-connection-file a dir))
           (is string= "data/app.db" (c:path-for-connection (merge-pathnames "data/app.db" dir) :project dir))
           (is string= (uiop:native-namestring (merge-pathnames "data/app.db" dir))
               (c:path-for-connection (merge-pathnames "data/app.db" dir) :global dir)))
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))))

(define-test sqlite :parent cadre-tests
  (let ((file (merge-pathnames (format nil "cadre-test-~d.db" (random 1000000)) (uiop:temporary-directory))))
    (unwind-protect
         (let ((db (c:open-sqlite file :create t)))
           (unwind-protect
                (progn
                  (c:db-run db "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER, photo BLOB);
CREATE TABLE tags (tag TEXT);
CREATE VIEW adults AS SELECT * FROM people WHERE age >= 18;
CREATE INDEX people_age ON people (age);")
                  (dolist (row '(("Ann" 34) ("Bo" 12) ("Cy" :null)))
                    (is = 1 (c:db-result-changes
                             (c:db-run db "INSERT INTO people (name, age) VALUES (?, ?)" :params row))))
                  (c:db-run db "UPDATE people SET photo = ? WHERE name = 'Ann'"
                            :params (list (make-array 3 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3))))
                  (is equal '((:name "people" :kind :table) (:name "tags" :kind :table) (:name "adults" :kind :view))
                      (c:db-tables db))
                  (is equal '("id" "name" "age" "photo") (mapcar (lambda (c) (getf c :name)) (c:db-columns db "people")))
                  (is eq t (getf (second (c:db-columns db "people")) :not-null))
                  (is equal '("id") (c:db-row-key db "people"))
                  (is equal '(:rowid) (c:db-row-key db "tags"))
                  (is equal nil (c:db-row-key db "adults"))
                  (is equal '("age") (getf (first (c:db-indexes db "people")) :columns))
                  (true (search "CREATE VIEW" (c:db-create-statement db "adults")))
                  (multiple-value-bind (sql params)
                      (c:table-select-sql "people" :key '("id") :filters '(("age" . ">20")))
                    (true (search "ORDER BY \"id\"" sql))
                    (let ((result (c:db-run db sql :params params)))
                      (is equal '("id" "id" "name" "age" "photo") (c:db-result-columns result))
                      (is = 1 (length (c:db-result-rows result)))
                      (is equalp #(1 1 "Ann" 34 #(1 2 3)) (first (c:db-result-rows result)))))
                  (let ((result (c:db-run db "SELECT name, age, 1.5 FROM people ORDER BY name" :limit 2)))
                    (is equalp '(#("Ann" 34 1.5d0) #("Bo" 12 1.5d0)) (c:db-result-rows result))
                    (is eq t (c:db-result-more result)))
                  (is equalp '(#("Cy" :null)) (c:db-result-rows (c:db-run db "SELECT name, age FROM people WHERE age IS NULL")))
                  (is string= "NULL" (c:db-value-text :null))
                  (is string= "1.5" (c:db-value-text 1.5d0))
                  (is string= "<blob, 3 bytes>"
                      (c:db-value-text (make-array 3 :element-type '(unsigned-byte 8) :initial-element 0)))
                  (is string= (format nil "name,age~%Ann,34~%\"a,b\",~%")
                      (c:db-result-csv '("name" "age") (list #("Ann" 34) #("a,b" :null))))
                  ;; Unicode survives the trip.
                  (is equalp '(#("λ → ✓")) (c:db-result-rows (c:db-run db "SELECT ?" :params '("λ → ✓"))))
                  (fail (c:db-run db "SELECT * FROM nowhere") c:editor-error))
             (c:db-close db))
           (let ((read-only (c:open-sqlite file :read-only t)))
             (unwind-protect (fail (c:db-run read-only "DELETE FROM people") c:editor-error)
               (c:db-close read-only)))
           (fail (c:open-sqlite (merge-pathnames "no-such.db" (uiop:temporary-directory))) c:editor-error))
      (uiop:delete-file-if-exists file))))
