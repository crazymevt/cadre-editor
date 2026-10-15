;;;; postgres.lisp — PostgreSQL through cl-postgres (Postmodern's client)
;;;;
;;;; cl-postgres speaks PostgreSQL's protocol itself (SCRAM and MD5 logins
;;;; included), so nothing needs installing. Tables are those of the
;;;; connection's current schema (public, usually). Dates, times, intervals
;;;; and numerics come back as the text PostgreSQL gives; booleans as
;;;; "true" and "false". SSL isn't offered yet.

(in-package #:cadre)

(defclass postgres-database (database)
  ((connection :initarg :connection :accessor pg-connection)
   (login :initarg :login :reader pg-login :documentation "(database user password host port), to cancel a query.")
   (pid :initform nil :accessor pg-pid)))

(defparameter *pg-text-types* '(1082 1083 1114 1184 1186 1266 1700)
  "The OIDs of date, time, timestamp, timestamptz, interval, timetz and
numeric, read as text instead of cl-postgres's Lisp values.")

(defvar *pg-readtable* nil)

(defun pg-readtable ()
  (or *pg-readtable*
      (let ((table (cl-postgres:copy-sql-readtable)))
        (dolist (oid *pg-text-types*) (cl-postgres:set-sql-reader oid nil :table table))
        (setf *pg-readtable* table))))

(defun pg-error (condition)
  (editor-error "~a" (if (typep condition 'cl-postgres:database-error)
                         (cl-postgres:database-error-message condition)
                         condition)))

(defmacro with-pg-errors (&body body)
  `(handler-case (let ((cl-postgres:*sql-readtable* (pg-readtable))) ,@body)
     (cl-postgres:database-error (e) (pg-error e))
     (cl-postgres:database-connection-error (e) (editor-error "Can't reach the database: ~a" e))
     ((or sb-bsd-sockets:socket-error stream-error end-of-file) (e)
       (editor-error "Lost the connection to the database: ~a" e))))

(defun open-postgres (&key host port database user password read-only)
  (let ((login (list (or database "postgres") (or user (uiop:getenv "USER") "postgres") (or password "")
                     (or host "localhost") (or port 5432))))
    (let ((connection (with-pg-errors (apply #'cl-postgres:open-database (append login (list :no))))))
      (let ((db (make-instance 'postgres-database :connection connection :login login :read-only read-only)))
        (when read-only
          (with-pg-errors (cl-postgres:exec-query connection "SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY")))
        (setf (pg-pid db) (ignore-errors
                           (with-pg-errors (caar (cl-postgres:exec-query connection "SELECT pg_backend_pid()"
                                                                         'cl-postgres:list-row-reader)))))
        db))))

(defmethod open-database-kind ((kind (eql :postgres)) connection &key project password create)
  (declare (ignore project create))
  (open-postgres :host (db-connection-host connection) :port (db-connection-port connection)
                 :database (db-connection-database connection) :user (db-connection-user connection)
                 :password password :read-only (db-connection-read-only connection)))

(defmethod db-close ((db postgres-database))
  (when (pg-connection db)
    (ignore-errors (cl-postgres:close-database (pg-connection db)))
    (setf (pg-connection db) nil)))

(defmethod db-open-p ((db postgres-database))
  (and (pg-connection db) (cl-postgres:database-open-p (pg-connection db))))

(defmethod db-interrupt ((db postgres-database))
  "Ask the server, over a second connection, to cancel the query running."
  (when (pg-pid db)
    (ignore-errors
     (let ((other (apply #'cl-postgres:open-database (append (pg-login db) (list :no)))))
       (unwind-protect (cl-postgres:exec-query other (format nil "SELECT pg_cancel_backend(~d)" (pg-pid db)))
         (cl-postgres:close-database other))))))

(defun pg-placeholders (sql)
  "SQL with each ? (outside strings, quoted names and comments) made $1, $2, …"
  (with-output-to-string (out)
    (let ((i 0) (n (length sql)) (count 0))
      (loop while (< i n)
            do (let ((c (char sql i)))
                 (cond ((member c '(#\' #\"))
                        (let ((close (or (position c sql :start (1+ i)) (1- n))))
                          (write-string sql out :start i :end (1+ close))
                          (setf i (1+ close))))
                       ((and (char= c #\-) (< (1+ i) n) (char= (char sql (1+ i)) #\-))
                        (let ((end (or (position #\Newline sql :start i) n)))
                          (write-string sql out :start i :end end)
                          (setf i end)))
                       ((char= c #\?) (format out "$~d" (incf count)) (incf i))
                       (t (write-char c out) (incf i))))))))

(defun pg-value (value)
  (case value
    ((t) "true")
    ((nil) "false")
    (t value)))

(defun pg-param (value)
  (case value
    ((nil) :null)
    (t value)))

(defun pg-execute-one (db sql params limit)
  (let ((start (get-internal-real-time))
        (columns '()) (rows '()) (more nil))
    (let ((reader (cl-postgres:row-reader (fields)
                    (setf columns (map 'list #'cl-postgres:field-name fields))
                    (let ((n 0))
                      (loop while (cl-postgres:next-row)
                            do (let ((row (map 'vector (lambda (f) (pg-value (cl-postgres:next-field f))) fields)))
                                 (if (and limit (>= n limit))
                                     (setf more t)
                                     (progn (push row rows) (incf n)))))))))
      (multiple-value-bind (result changed)
          (with-pg-errors
            (if params
                (progn (cl-postgres:prepare-query (pg-connection db) "" (pg-placeholders sql))
                       (cl-postgres:exec-prepared (pg-connection db) "" (mapcar #'pg-param params) reader))
                (cl-postgres:exec-query (pg-connection db) sql reader)))
        (declare (ignore result))
        (make-db-result :columns columns :rows (nreverse rows) :more more
                        :changes (if (and (null columns) (integerp changed)) changed 0)
                        :milliseconds (round (* 1000 (- (get-internal-real-time) start))
                                             internal-time-units-per-second))))))

(defmethod db-execute ((db postgres-database) sql &key params limit)
  "Run SQL; several statements one after another (cl-postgres prepares each,
and a prepared statement holds one), PARAMS going to the last. The last
statement's result, or the last one with rows."
  (unless (db-open-p db) (editor-error "The database is closed"))
  (let ((ranges (sql-statement-ranges sql)))
    (if (<= (length ranges) 1)
        (pg-execute-one db sql params limit)
        (let ((result nil) (changes 0) (milliseconds 0))
          (loop for (range . rest) on ranges
                for one = (pg-execute-one db (subseq sql (car range) (cdr range)) (and (null rest) params) limit)
                do (incf changes (db-result-changes one))
                   (incf milliseconds (db-result-milliseconds one))
                   (when (or (db-result-columns one) (null rest) (null result))
                     (setf result one)))
          (setf (db-result-changes result) changes
                (db-result-milliseconds result) milliseconds)
          result))))

;;; The schema: the current schema's tables

(defun pg-rows (db sql &rest params)
  (db-result-rows (db-execute db sql :params params)))

(defmethod db-tables ((db postgres-database))
  (mapcar (lambda (row) (list :name (aref row 0) :kind (if (string= (aref row 1) "VIEW") :view :table)))
          (pg-rows db "SELECT table_name, table_type FROM information_schema.tables
WHERE table_schema = current_schema() AND table_type IN ('BASE TABLE', 'VIEW')
ORDER BY table_type, table_name")))

(defun pg-primary-key (db table)
  "TABLE's primary key columns, in the key's order."
  (mapcar (lambda (row) (aref row 0))
          (pg-rows db "SELECT a.attname FROM pg_index i
JOIN pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey)
WHERE i.indrelid = (quote_ident(current_schema()) || '.' || quote_ident(?))::regclass AND i.indisprimary
ORDER BY array_position(i.indkey::int2[], a.attnum)" table)))

(defmethod db-columns ((db postgres-database) table)
  (let ((key (pg-primary-key db table)))
    (mapcar (lambda (row)
              (let ((name (aref row 0)))
                (list :name name :type (aref row 1) :not-null (string= (aref row 2) "NO")
                      :default (let ((d (aref row 3))) (if (eq d :null) nil d))
                      :primary-key (let ((p (position name key :test #'string=))) (and p (1+ p))))))
            (pg-rows db "SELECT column_name, data_type, is_nullable, column_default FROM information_schema.columns
WHERE table_schema = current_schema() AND table_name = ? ORDER BY ordinal_position" table))))

(defmethod db-indexes ((db postgres-database) table)
  (mapcar (lambda (row)
            (list :name (aref row 0) :unique (equal (aref row 1) "true")
                  :columns (uiop:split-string (aref row 2) :separator ",")))
          (pg-rows db "SELECT c.relname, i.indisunique::text,
  (SELECT string_agg(pg_get_indexdef(i.indexrelid, k, true), ',') FROM generate_subscripts(i.indkey, 1) k)
FROM pg_index i JOIN pg_class c ON c.oid = i.indexrelid
WHERE i.indrelid = (quote_ident(current_schema()) || '.' || quote_ident(?))::regclass
ORDER BY c.relname" table)))

(defmethod db-foreign-keys ((db postgres-database) table)
  (mapcar (lambda (row) (list :column (aref row 0) :table (aref row 1) :to (aref row 2)))
          (pg-rows db "SELECT kcu.column_name, ccu.table_name, ccu.column_name
FROM information_schema.table_constraints tc
JOIN information_schema.key_column_usage kcu
  ON tc.constraint_name = kcu.constraint_name AND tc.table_schema = kcu.table_schema
JOIN information_schema.constraint_column_usage ccu
  ON tc.constraint_name = ccu.constraint_name AND tc.table_schema = ccu.table_schema
WHERE tc.constraint_type = 'FOREIGN KEY' AND tc.table_schema = current_schema() AND tc.table_name = ?" table)))

(defmethod db-create-statement ((db postgres-database) name)
  "A view's definition; for a table, a CREATE TABLE made from its columns
\(PostgreSQL keeps no text of it)."
  (let ((view (pg-rows db "SELECT pg_get_viewdef((quote_ident(current_schema()) || '.' || quote_ident(?))::regclass, true)
FROM information_schema.views WHERE table_schema = current_schema() AND table_name = ?" name name)))
    (if view
        (format nil "CREATE VIEW ~a AS~%~a" (quote-sql-identifier name) (aref (first view) 0))
        (let ((columns (db-columns db name))
              (key (pg-primary-key db name)))
          (when columns
            (format nil "CREATE TABLE ~a (~%~{  ~a~^,~%~}~@[,~%  PRIMARY KEY (~{~a~^, ~})~]~%)"
                    (quote-sql-identifier name)
                    (mapcar (lambda (c) (format nil "~a ~a~:[~; NOT NULL~]~@[ DEFAULT ~a~]" (quote-sql-identifier (getf c :name))
                                                (getf c :type) (getf c :not-null) (getf c :default)))
                            columns)
                    (and key (mapcar #'quote-sql-identifier key))))))))

(defmethod db-row-key ((db postgres-database) table)
  "TABLE's primary key; nil (no editing) for a view or a table without one."
  (let ((kind (getf (find table (db-tables db) :key (lambda (x) (getf x :name)) :test #'string=) :kind)))
    (and (eq kind :table) (pg-primary-key db table))))
