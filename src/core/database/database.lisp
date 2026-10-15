;;;; database.lisp — databases: connections, what a backend provides, and SQL
;;;;
;;;; A connection is a saved description (DB-CONNECTION): its name, kind,
;;;; file and options. Opening one gives a DATABASE, an object of the kind's
;;;; backend (sqlite.lisp for :sqlite), which answers the generic functions
;;;; below. Another kind of database is another backend: a class, an
;;;; OPEN-DATABASE-KIND method and the DB- methods.
;;;;
;;;; The rest is plain SQL text, for any backend: the SELECT a table browser
;;;; runs (with filters typed above its columns, a sort and a page), the
;;;; UPDATE, INSERT and DELETE its edits become, and the statement around
;;;; the cursor in a SQL console.
;;;;
;;;; Connections are saved in two files: global ones in the config directory,
;;;; a project's in its .cadre/databases.sexp (where a file path is kept
;;;; relative to the project, so the file can be committed).

(in-package #:cadre)

;;; Connections

(defparameter *database-kinds* '((:sqlite . "SQLite") (:postgres . "PostgreSQL"))
  "The kinds of database Cadre can open, and their names.")

(defstruct (db-connection (:conc-name db-connection-))
  (id (make-uuid))
  (name "")
  (kind :sqlite)
  (path nil)                            ; a file database's file (relative to the project's folder for a project connection)
  (read-only nil)
  (scope :global)                       ; :global, or :project for one saved in the project
  ;; A server database's (PostgreSQL); its password is kept apart (DB-PASSWORD).
  (host nil) (port nil) (database nil) (user nil))

(defun db-connection-server-p (connection)
  (not (eq (db-connection-kind connection) :sqlite)))

(defun db-connection-plist (connection)
  (append (list :id (db-connection-id connection) :name (db-connection-name connection)
                :kind (db-connection-kind connection))
          (if (db-connection-server-p connection)
              (list :host (db-connection-host connection) :port (db-connection-port connection)
                    :database (db-connection-database connection) :user (db-connection-user connection))
              (list :path (db-connection-path connection)))
          (list :read-only (db-connection-read-only connection))))

(defun plist-db-connection (plist scope)
  (destructuring-bind (&key id name kind path read-only host port database user &allow-other-keys) plist
    (make-db-connection :id (or id (make-uuid)) :name (or name "") :kind (or kind :sqlite)
                        :path path :read-only read-only :scope scope
                        :host host :port port :database database :user user)))

;;; Passwords: kept apart from the connections (which may be committed with a
;;; project), in a file only you can read: {"connection id": "password"}.

(defun read-db-password (file id)
  (let ((json (ignore-errors (parse-json (uiop:read-file-string file)))))
    (and (jobject-p json) (let ((p (jget json id))) (and (stringp p) p)))))

(defun write-db-password (file id password)
  "Keep PASSWORD for connection ID in FILE (nil or \"\" forgets it)."
  (let* ((json (ignore-errors (parse-json (uiop:read-file-string file))))
         (entries (remove id (and (jobject-p json) (cdr json)) :key #'car :test #'string=)))
    (when (plusp (length password)) (setf entries (append entries (list (cons id password)))))
    (ensure-directories-exist file)
    (ignore-errors (sb-posix:chmod (uiop:native-namestring (uiop:pathname-directory-pathname file)) #o700))
    (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
      (write-string (json-string (cons :object entries)) out))
    (sb-posix:chmod (uiop:native-namestring file) #o600)
    file))

(defun read-db-connections (file scope)
  "The connections saved in FILE (none if it doesn't exist), as SCOPE."
  (let ((forms (ignore-errors
                (with-open-file (in file :if-does-not-exist nil)
                  (and in (with-standard-io-syntax
                            (let ((*read-eval* nil))
                              (read in nil nil))))))))
    (loop for plist in (and (listp forms) forms)
          when (and (listp plist) (getf plist :name))
            collect (plist-db-connection plist scope))))

(defun write-db-connections (file connections)
  "Save CONNECTIONS in FILE."
  (ensure-directories-exist file)
  (with-open-file (out file :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-readably* nil))
        (format out ";;; Database connections, saved by Cadre~%(~{~s~^~% ~})~%"
                (mapcar #'db-connection-plist connections))))))

(defun project-db-connections-file (project)
  (merge-pathnames ".cadre/databases.sexp" (uiop:ensure-directory-pathname project)))

(defun db-connection-file (connection project)
  "CONNECTION's database file, absolute: a project connection's path is
relative to PROJECT."
  (let ((path (db-connection-path connection)))
    (when (and path (plusp (length path)))
      (let ((path (uiop:parse-native-namestring (uiop:native-namestring (expand-tilde path)))))
        (if (and project (uiop:relative-pathname-p path))
            (merge-pathnames path (uiop:ensure-directory-pathname project))
            path)))))

(defun expand-tilde (path)
  (if (and (plusp (length path)) (char= (char path 0) #\~))
      (concatenate 'string (string-right-trim "/" (uiop:native-namestring (user-homedir-pathname)))
                   (subseq path 1))
      path))

(defun path-for-connection (file scope project)
  "How to save FILE in a connection: relative to PROJECT when SCOPE is
:project and FILE is inside it, else its full path."
  (flet ((real (path)
           ;; Its true name (macOS's /var is /private/var), or its folder's
           ;; if it doesn't exist yet.
           (uiop:native-namestring
            (or (ignore-errors (truename path))
                (let ((folder (ignore-errors (truename (uiop:pathname-directory-pathname path)))))
                  (and folder (merge-pathnames (file-namestring path) folder)))
                path)))
         (inside (name base)
           (and (uiop:string-prefix-p base name) (subseq name (length base)))))
    (let ((name (uiop:native-namestring file)))
      (or (and (eq scope :project) project
               (let ((base (uiop:native-namestring (uiop:ensure-directory-pathname project))))
                 (or (inside name base)
                     (inside (real file) (real (uiop:ensure-directory-pathname project))))))
          name))))

(defun db-connection-description (connection)
  (format nil "~a~@[ · ~a~]~:[~; · read-only~]"
          (or (cdr (assoc (db-connection-kind connection) *database-kinds*)) (db-connection-kind connection))
          (if (db-connection-server-p connection)
              (format nil "~@[~a@~]~a~@[:~a~]/~a" (db-connection-user connection)
                      (or (db-connection-host connection) "localhost") (db-connection-port connection)
                      (db-connection-database connection))
              (db-connection-path connection))
          (db-connection-read-only connection)))

;;; What a backend provides

(defclass database ()
  ((read-only :initarg :read-only :initform nil :reader db-read-only-p)
   (lock :initform (sb-thread:make-mutex :name "database") :reader db-lock
         :documentation "Held while a statement runs: one at a time."))
  (:documentation "An open database. Each kind of database is a subclass."))

(defstruct db-result
  (columns '())                         ; the column names
  (rows '())                            ; each a vector of values: integers, floats, strings, octet vectors, :NULL
  (more nil)                            ; true when there were more rows than the limit
  (changes 0)                           ; rows inserted, updated or deleted
  (milliseconds 0))

(defgeneric open-database-kind (kind connection &key project password create)
  (:documentation "Open CONNECTION, a database of KIND (:sqlite, :postgres)."))

(defmethod open-database-kind ((kind (eql :sqlite)) connection &key project password create)
  (declare (ignore password))
  (open-sqlite (or (db-connection-file connection project) (editor-error "Choose the database file"))
               :read-only (db-connection-read-only connection) :create create))

(defmethod open-database-kind (kind connection &key project password create)
  (declare (ignore connection project password create))
  (editor-error "Cadre can't open ~a databases" kind))

(defun open-database (connection &key project password create)
  (open-database-kind (db-connection-kind connection) connection
                      :project project :password password :create create))

(defgeneric db-close (db))
(defgeneric db-open-p (db))
(defgeneric db-interrupt (db) (:documentation "Stop the statement running, from another thread."))
(defgeneric db-execute (db sql &key params limit)
  (:documentation "Run SQL with PARAMS bound to its ? marks; at most LIMIT rows. A DB-RESULT."))
(defgeneric db-tables (db) (:documentation "Its tables and views: plists of :name and :kind (:table or :view)."))
(defgeneric db-columns (db table)
  (:documentation "TABLE's columns: plists of :name :type :not-null :default :primary-key (its place in the key)."))
(defgeneric db-indexes (db table) (:documentation "Plists of :name :unique :columns."))
(defgeneric db-foreign-keys (db table) (:documentation "Plists of :column :table :to."))
(defgeneric db-create-statement (db name) (:documentation "The SQL that made table or view NAME, or nil."))
(defgeneric db-row-key (db table)
  (:documentation "The column names that pick out one row of TABLE (:ROWID for SQLite's own), or nil if
its rows can't be told apart (a view): then it can't be edited."))

(defun db-run (db sql &rest args &key params limit)
  "DB-EXECUTE, one statement at a time across threads."
  (declare (ignore params limit))
  (sb-thread:with-mutex ((db-lock db))
    (apply #'db-execute db sql args)))

;;; Values as text

(defun db-value-text (value &key (max 400))
  "VALUE as a table cell shows it (NULL as \"NULL\"), cut to MAX characters."
  (let ((text (typecase value
                ((eql :null) "NULL")
                (string value)
                (float (let ((*read-default-float-format* 'double-float))
                         (princ-to-string value)))
                ((vector (unsigned-byte 8)) (format nil "<blob, ~:d byte~:p>" (length value)))
                (t (princ-to-string value)))))
    (if (and max (> (length text) max))
        (concatenate 'string (subseq text 0 max) "…")
        text)))

(defun csv-field (text)
  (if (find-if (lambda (c) (member c '(#\, #\" #\Newline #\Return))) text)
      (format nil "\"~a\"" (ppcre:regex-replace-all "\"" text "\"\""))
      text))

(defun db-result-csv (columns rows)
  "COLUMNS and ROWS as CSV text (NULL as an empty field)."
  (with-output-to-string (out)
    (format out "~{~a~^,~}~%" (mapcar #'csv-field columns))
    (dolist (row rows)
      (format out "~{~a~^,~}~%"
              (map 'list (lambda (v) (if (eq v :null) "" (csv-field (db-value-text v :max nil)))) row)))))

;;; SQL text

(defun quote-sql-identifier (name)
  "NAME as a quoted SQL identifier: \"name\", with \" doubled."
  (format nil "\"~a\"" (ppcre:regex-replace-all "\"" name "\"\"")))

(defun key-column-sql (column)
  (if (eq column :rowid) "rowid" (quote-sql-identifier column)))

(defun sql-number-p (text)
  (ppcre:scan "^[+-]?(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?$" text))

(defun sql-filter-value (text)
  "TEXT as a parameter: a number if it reads as one, else the text."
  (cond ((ppcre:scan "^[+-]?\\d{1,18}$" text) (parse-integer text))
        ((sql-number-p text)
         (let ((*read-default-float-format* 'double-float))
           (coerce (let ((*read-eval* nil)) (read-from-string text)) 'double-float)))
        (t text)))

(defun parse-column-filter (text)
  "What a filter typed above a column means, as (OP VALUE): =, !=, <, <=, >,
>= and a value; NULL and !NULL; or else the column contains the text, in any case (:like).
Nil for an empty filter."
  (let ((text (string-trim " " text)))
    (cond ((string= text "") nil)
          ((string-equal text "null") (list :null))
          ((member text '("!null" "not null" "!= null") :test #'string-equal) (list :not-null))
          ((string= text "''") (list := ""))
          (t (multiple-value-bind (match groups) (ppcre:scan-to-strings "^(<=|>=|!=|<>|=|<|>)\\s*(.*)$" text)
               (if match
                   (let ((op (aref groups 0)) (value (aref groups 1)))
                     (list (cond ((string= op "<>") :!=) (t (intern op :keyword))) (sql-filter-value value)))
                   (list :like (concatenate 'string "%" text "%"))))))))

(defun filter-sql (column filter)
  "A WHERE condition on COLUMN for FILTER (from PARSE-COLUMN-FILTER) and its parameters."
  (let ((name (quote-sql-identifier column)))
    (destructuring-bind (op &optional value) filter
      (case op
        (:null (values (format nil "~a IS NULL" name) '()))
        (:not-null (values (format nil "~a IS NOT NULL" name) '()))
        ;; Any case, in any database (PostgreSQL's LIKE minds it, SQLite's doesn't).
        (:like (values (format nil "LOWER(CAST(~a AS TEXT)) LIKE LOWER(?)" name) (list value)))
        (t (values (format nil "~a ~a ?" name (string-downcase op)) (list value)))))))

(defun where-sql (filters)
  "FILTERS, (column . filter-text) each, as a WHERE clause (or \"\") and its parameters."
  (let ((conditions '()) (params '()))
    (loop for (column . text) in filters
          for filter = (parse-column-filter text)
          when filter
            do (multiple-value-bind (sql ps) (filter-sql column filter)
                 (push sql conditions)
                 (setf params (append params ps))))
    (values (if conditions (format nil " WHERE ~{~a~^ AND ~}" (reverse conditions)) "")
            params)))

(defun table-select-sql (table &key key filters sort descending limit offset)
  "The SELECT for a page of TABLE: its KEY columns first (when there is a
key), then every column, with FILTERS, sorted by column SORT (or else by the
key, so rows keep their places: PostgreSQL's come in no order). Returns the
SQL and its parameters."
  (multiple-value-bind (where params) (where-sql filters)
    (values (format nil "SELECT ~{~a, ~}* FROM ~a~a~@[ ORDER BY ~a~]~@[ LIMIT ~d~]~@[ OFFSET ~d~]"
                    (mapcar #'key-column-sql key) (quote-sql-identifier table) where
                    (cond (sort (format nil "~a~:[~; DESC~]" (quote-sql-identifier sort) descending))
                          (key (format nil "~{~a~^, ~}" (mapcar #'key-column-sql key))))
                    limit (and offset (plusp offset) offset))
            params)))

(defun table-count-sql (table &key filters)
  (multiple-value-bind (where params) (where-sql filters)
    (values (format nil "SELECT count(*) FROM ~a~a" (quote-sql-identifier table) where) params)))

(defun key-where-sql (key key-values)
  (values (format nil "~{~a = ?~^ AND ~}" (mapcar #'key-column-sql key))
          (coerce key-values 'list)))

(defun update-row-sql (table key key-values changes)
  "An UPDATE of the row of TABLE whose KEY columns have KEY-VALUES, setting
CHANGES, (column . value) each. Returns the SQL and its parameters."
  (multiple-value-bind (where params) (key-where-sql key key-values)
    (values (format nil "UPDATE ~a SET ~{~a = ?~^, ~} WHERE ~a" (quote-sql-identifier table)
                    (mapcar (lambda (c) (quote-sql-identifier (car c))) changes) where)
            (append (mapcar #'cdr changes) params))))

(defun insert-row-sql (table values)
  "An INSERT into TABLE of VALUES, (column . value) each (none: the defaults)."
  (if values
      (values (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~a~^, ~})" (quote-sql-identifier table)
                      (mapcar (lambda (c) (quote-sql-identifier (car c))) values)
                      (make-list (length values) :initial-element "?"))
              (mapcar #'cdr values))
      (values (format nil "INSERT INTO ~a DEFAULT VALUES" (quote-sql-identifier table)) '())))

(defun delete-row-sql (table key key-values)
  (multiple-value-bind (where params) (key-where-sql key key-values)
    (values (format nil "DELETE FROM ~a WHERE ~a" (quote-sql-identifier table) where) params)))

(defun sql-statement-ranges (text)
  "Where the statements in TEXT are, as (start . end) each, split at
semicolons outside strings, quoted names and comments. A range leaves out its
semicolon; ranges of only whitespace and comments are dropped."
  (let ((ranges '()) (start 0) (i 0) (n (length text)) (code nil))
    (flet ((finish (end)
             (when code (push (cons start end) ranges))
             (setf code nil)))
      (loop while (< i n)
            do (let ((c (char text i)))
                 (cond ((and (char= c #\-) (< (1+ i) n) (char= (char text (1+ i)) #\-))
                        (setf i (or (position #\Newline text :start i) n)))
                       ((and (char= c #\/) (< (1+ i) n) (char= (char text (1+ i)) #\*))
                        (setf i (let ((e (search "*/" text :start2 (+ i 2)))) (if e (+ e 2) n))))
                       ((member c '(#\' #\" #\` #\[))
                        (setf code t)
                        (let ((close (if (char= c #\[) #\] c)))
                          (setf i (1+ i))
                          (loop while (< i n)
                                do (if (char= (char text i) close)
                                       (if (and (< (1+ i) n) (char= (char text (1+ i)) close) (char/= c #\[))
                                           (incf i 2)
                                           (return))
                                       (incf i)))
                          (setf i (min n (1+ i)))))
                       ((char= c #\;) (finish i) (setf i (1+ i) start i))
                       (t (unless (member c '(#\Space #\Tab #\Newline #\Return)) (setf code t))
                          (incf i)))))
      (finish n))
    (nreverse ranges)))

(defun sql-statement-at (text position)
  "The statement in TEXT around POSITION (or, after the last semicolon, the
one before it), trimmed, with its start and end; nil if there is none."
  (let* ((ranges (sql-statement-ranges text))
         (range (or (find-if (lambda (r) (<= (car r) position (cdr r))) ranges)
                    (find-if (lambda (r) (<= (cdr r) position)) ranges :from-end t)
                    (first ranges))))
    (when range
      (let* ((whitespace '(#\Space #\Tab #\Newline #\Return))
             (start (or (position-if-not (lambda (c) (member c whitespace)) text :start (car range)) (car range)))
             (end (1+ (or (position-if-not (lambda (c) (member c whitespace)) text :end (cdr range) :from-end t)
                          (1- (cdr range))))))
        (values (subseq text start end) start end)))))
