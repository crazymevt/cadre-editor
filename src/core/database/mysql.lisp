;;;; mysql.lisp — MySQL and MariaDB through qmynd
;;;;
;;;; qmynd speaks MySQL's protocol itself (with MySQL 8's caching_sha2
;;;; login), so nothing needs installing. Tables are those of the
;;;; connection's database. Names are quoted `like this`. Values come back
;;;; as text, as the server gives them (dates, decimals); NULL as :NULL.
;;;; SSL isn't offered yet.

(in-package #:cadre)

(defclass mysql-database (database)
  ((connection :initarg :connection :accessor my-connection)
   (login :initarg :login :reader my-login :documentation "The MYSQL-CONNECT arguments, to stop a query.")
   (id :initform nil :accessor my-connection-id)))

(defmethod db-identifier-quote ((db mysql-database)) #\`)
(defmethod db-text-type ((db mysql-database)) "CHAR")

(defmacro with-mysql-errors (&body body)
  `(handler-case (progn ,@body)
     (qmynd:mysql-error (e) (editor-error "~a" (let ((text (princ-to-string e)))
                                                 ;; "MySQL Error [1146]: "Table … doesn't exist"" → the message
                                                 (string-trim "\" " (subseq text (1+ (or (position #\: text) -1)))))))
     (qmynd:mysql-base-error (e) (editor-error "MySQL: ~a" e))
     ((or sb-bsd-sockets:socket-error usocket:socket-error usocket:ns-error stream-error end-of-file) (e)
       (editor-error "Can't reach the database: ~a" e))))

(defun open-mysql (&key host port database user password read-only)
  (let ((login (list :host (or host "localhost") :port (or port 3306) :username (or user "root")
                     :password (or password "") :database (and (plusp (length database)) database) :ssl nil)))
    (let* ((connection (with-mysql-errors (apply #'qmynd:mysql-connect login)))
           (db (make-instance 'mysql-database :connection connection :login login :read-only read-only)))
      (when read-only
        (with-mysql-errors (qmynd:mysql-query connection "SET SESSION TRANSACTION READ ONLY")))
      (setf (my-connection-id db)
            (ignore-errors (parse-integer (aref (aref (qmynd:mysql-query connection "SELECT CONNECTION_ID()" :as-text t) 0) 0))))
      db)))

(defmethod open-database-kind ((kind (eql :mysql)) connection &key project password create)
  (declare (ignore project create))
  (open-mysql :host (db-connection-host connection) :port (db-connection-port connection)
              :database (db-connection-database connection) :user (db-connection-user connection)
              :password password :read-only (db-connection-read-only connection)))

(defmethod db-close ((db mysql-database))
  (when (my-connection db)
    (ignore-errors (qmynd:mysql-disconnect (my-connection db)))
    (setf (my-connection db) nil)))

(defmethod db-open-p ((db mysql-database))
  (and (my-connection db) t))

(defmethod db-interrupt ((db mysql-database))
  "Stop the running query, from a second connection."
  (when (my-connection-id db)
    (ignore-errors
     (let ((other (apply #'qmynd:mysql-connect (my-login db))))
       (unwind-protect (qmynd:mysql-query other (format nil "KILL QUERY ~d" (my-connection-id db)))
         (qmynd:mysql-disconnect other))))))

(defun my-value (value)
  "VALUE from qmynd as a cell holds it: NULL as :NULL, dates and decimals as text."
  (typecase value
    (null :null)
    (qmynd:mysql-date-time
     (format nil "~4,'0d-~2,'0d-~2,'0d~:[ ~2,'0d:~2,'0d:~2,'0d~;~3*~]"
             (qmynd:mysql-date-time-year value) (qmynd:mysql-date-time-month value) (qmynd:mysql-date-time-day value)
             (and (zerop (qmynd:mysql-date-time-hour value)) (zerop (qmynd:mysql-date-time-minute value))
                  (zerop (qmynd:mysql-date-time-second value)))
             (qmynd:mysql-date-time-hour value) (qmynd:mysql-date-time-minute value) (qmynd:mysql-date-time-second value)))
    (qmynd:mysql-time-interval (princ-to-string (qmynd:mysql-time-interval-to-seconds value)))
    (ratio (format nil "~f" (coerce value 'double-float)))
    (t value)))

(defun my-literal (value)
  "VALUE as a MySQL literal: NULL, a number, X'…' for bytes, or a quoted string."
  (typecase value
    ((or null (eql :null)) "NULL")
    (integer (princ-to-string value))
    (float (let* ((*read-default-float-format* 'double-float)
                  (text (princ-to-string value)))
             ;; Common Lisp's D exponent is not part of MySQL's numeric grammar.
             (substitute #\e #\d (substitute #\e #\D text))))
    ((vector (unsigned-byte 8)) (format nil "X'~{~2,'0X~}'" (coerce value 'list)))
    (t (with-output-to-string (out)
         (write-char #\' out)
         (loop for c across (princ-to-string value)
               do (case c
                    ;; Doubling quotes is valid even with NO_BACKSLASH_ESCAPES.
                    ;; Preserve backslashes and control characters literally.
                    (#\' (write-string "''" out))
                    (t (write-char c out))))
         (write-char #\' out)))))

(defun my-inline-params (sql params)
  "SQL with each ? (outside strings, quoted names and comments) replaced by
the next of PARAMS as a literal. qmynd's prepared statements misread some
types (dates), so parameters go in the text as SQL literals."
  (prog1 (replace-sql-placeholders sql (lambda ()
                                         (when (null params) (editor-error "More ? in the statement than values for them"))
                                         (my-literal (pop params)))
                                   :backslash-escapes t :hash-comments t)
    (when params (editor-error "More values than ? in the statement"))))

(defparameter *my-integer-types* '(1 2 3 8 9 13) "MySQL's tiny, short, long, longlong, int24 and year column types.")
(defparameter *my-float-types* '(4 5) "MySQL's float and double.")

(defun my-typed-value (text type)
  "TEXT, a value from the text protocol, as a number for numeric column TYPEs."
  (cond ((not (stringp text)) text)
        ((member type *my-integer-types*) (or (ignore-errors (parse-integer text)) text))
        ((member type *my-float-types*)
         (or (ignore-errors (let ((*read-default-float-format* 'double-float) (*read-eval* nil))
                              (let ((v (read-from-string text))) (and (realp v) (coerce v 'double-float)))))
             text))
        (t text)))

(defun my-column-names (columns)
  (map 'list (lambda (c) (qmynd-impl::column-definition-v41-packet-name c)) columns))

(defun my-rows-matched (ok)
  "The rows statement result OK touched. An UPDATE's affected rows leave out those already holding the
new value, which saving a table would take for a row gone away, so it's the \"Rows matched\" of its
info text, as the other databases count. (qmynd's :client-found-rows, which asks the server for that,
clears its other capabilities.)"
  (let ((matched (nth-value 1 (ppcre:scan-to-strings "Rows matched: (\\d+)" ; after qmynd's stray length byte
                                                     (or (qmynd-impl::response-ok-packet-info ok) "")))))
    (if matched (parse-integer (aref matched 0)) (qmynd:response-ok-packet-affected-rows ok))))

(defun my-execute-one (db sql params limit)
  (let ((start (get-internal-real-time)))
    (multiple-value-bind (result columns)
        (with-mysql-errors
          (qmynd:mysql-query (my-connection db) (if params (my-inline-params sql params) sql) :as-text t))
      (let ((milliseconds (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))
        (if (typep result 'qmynd:response-ok-packet)
            (make-db-result :changes (my-rows-matched result) :milliseconds milliseconds)
            (let* ((types (map 'list (lambda (c) (qmynd-impl::column-definition-v41-packet-type c)) columns))
                   (rows (map 'list (lambda (row)
                                      (map 'vector (lambda (v type) (my-typed-value (my-value v) type)) row types))
                              result))
                   (more (and limit (> (length rows) limit))))
              (make-db-result :columns (my-column-names columns)
                              :rows (if more (subseq rows 0 limit) rows) :more more
                              :milliseconds milliseconds)))))))

(defmethod db-execute ((db mysql-database) sql &key params limit)
  "Run SQL, one statement at a time (the server takes one a query), PARAMS
going to the last. The last statement's result, or the last one with rows."
  (unless (db-open-p db) (editor-error "The database is closed"))
  (let ((ranges (sql-statement-ranges sql :kind :mysql)))
    (if (<= (length ranges) 1)
        (my-execute-one db sql params limit)
        (let ((result nil) (changes 0) (milliseconds 0))
          (loop for (range . rest) on ranges
                for one = (my-execute-one db (subseq sql (car range) (cdr range)) (and (null rest) params) limit)
                do (incf changes (db-result-changes one))
                   (incf milliseconds (db-result-milliseconds one))
                   (when (or (db-result-columns one) (null rest) (null result))
                     (setf result one)))
          (setf (db-result-changes result) changes
                (db-result-milliseconds result) milliseconds)
          result))))

;;; The schema: the connection's database

(defun my-rows (db sql &rest params)
  (db-result-rows (db-execute db sql :params params)))

(defmethod db-tables ((db mysql-database))
  (mapcar (lambda (row) (list :name (aref row 0) :kind (if (string= (aref row 1) "VIEW") :view :table)))
          (my-rows db "SELECT table_name, table_type FROM information_schema.tables
WHERE table_schema = DATABASE() ORDER BY table_type, table_name")))

(defun my-primary-key (db table)
  (mapcar (lambda (row) (aref row 0))
          (my-rows db "SELECT column_name FROM information_schema.statistics
WHERE table_schema = DATABASE() AND table_name = ? AND index_name = 'PRIMARY' ORDER BY seq_in_index" table)))

(defmethod db-columns ((db mysql-database) table)
  (let ((key (my-primary-key db table)))
    (mapcar (lambda (row)
              (let ((name (aref row 0)))
                (list :name name :type (aref row 1) :not-null (string= (aref row 2) "NO")
                      :default (let ((d (aref row 3))) (if (eq d :null) nil d))
                      :primary-key (let ((p (position name key :test #'string=))) (and p (1+ p))))))
            (my-rows db "SELECT column_name, column_type, is_nullable, column_default FROM information_schema.columns
WHERE table_schema = DATABASE() AND table_name = ? ORDER BY ordinal_position" table))))

(defmethod db-indexes ((db mysql-database) table)
  (let ((indexes '()))                  ; (name unique column …), newest first
    (dolist (row (my-rows db "SELECT index_name, non_unique, column_name FROM information_schema.statistics
WHERE table_schema = DATABASE() AND table_name = ? ORDER BY index_name, seq_in_index" table))
      (let ((entry (assoc (aref row 0) indexes :test #'string=)))
        (if entry
            (setf (cddr entry) (append (cddr entry) (list (aref row 2))))
            (push (list (aref row 0) (equal (princ-to-string (aref row 1)) "0") (aref row 2)) indexes))))
    (mapcar (lambda (entry) (list :name (first entry) :unique (second entry) :columns (cddr entry)))
            (nreverse indexes))))

(defmethod db-foreign-keys ((db mysql-database) table)
  (mapcar (lambda (row) (list :column (aref row 0) :table (aref row 1) :to (aref row 2)))
          (my-rows db "SELECT column_name, referenced_table_name, referenced_column_name
FROM information_schema.key_column_usage
WHERE table_schema = DATABASE() AND table_name = ? AND referenced_table_name IS NOT NULL" table)))

(defmethod db-create-statement ((db mysql-database) name)
  (let ((rows (ignore-errors (db-result-rows (db-execute db (format nil "SHOW CREATE TABLE `~a`"
                                                                    (ppcre:regex-replace-all "`" name "``")))))))
    (and rows (let ((v (aref (first rows) 1))) (and (stringp v) v)))))

(defmethod db-row-key ((db mysql-database) table)
  (let ((kind (getf (find table (db-tables db) :key (lambda (x) (getf x :name)) :test #'string=) :kind)))
    (and (eq kind :table) (my-primary-key db table))))
