;;;; sqlite.lisp — SQLite through the libsqlite3 that macOS and Linux ship
;;;;
;;;; Just enough of the C API to open a file, run statements with bound
;;;; parameters and read their rows. Values come back as Lisp integers,
;;;; double-floats, strings, octet vectors (blobs) and :NULL.

(in-package #:cadre)

(cffi:define-foreign-library libsqlite3
  (:darwin "libsqlite3.dylib")
  (:unix (:or "libsqlite3.so.0" "libsqlite3.so"))
  (t (:default "libsqlite3")))

(defvar *sqlite-loaded* nil)

(defun load-sqlite ()
  "Load libsqlite3 the first time it's needed. Signals an editor-error if it can't be found."
  (unless *sqlite-loaded*
    (handler-case (cffi:use-foreign-library libsqlite3)
      (error (e) (editor-error "Can't load libsqlite3: ~a" e)))
    (setf *sqlite-loaded* t))
  t)

(defconstant +sqlite-ok+ 0)
(defconstant +sqlite-row+ 100)
(defconstant +sqlite-done+ 101)
(defconstant +sqlite-open-readonly+ #x01)
(defconstant +sqlite-open-readwrite+ #x02)
(defconstant +sqlite-open-create+ #x04)

(cffi:defcfun ("sqlite3_libversion" %sqlite-libversion) :string)
(cffi:defcfun ("sqlite3_open_v2" %sqlite-open) :int
  (filename :string) (db :pointer) (flags :int) (vfs :pointer))
(cffi:defcfun ("sqlite3_close_v2" %sqlite-close) :int (db :pointer))
(cffi:defcfun ("sqlite3_errmsg" %sqlite-errmsg) :string (db :pointer))
(cffi:defcfun ("sqlite3_busy_timeout" %sqlite-busy-timeout) :int (db :pointer) (ms :int))
(cffi:defcfun ("sqlite3_changes" %sqlite-changes) :int (db :pointer))
(cffi:defcfun ("sqlite3_interrupt" %sqlite-interrupt) :void (db :pointer))
(cffi:defcfun ("sqlite3_prepare_v2" %sqlite-prepare) :int
  (db :pointer) (sql :pointer) (bytes :int) (statement :pointer) (tail :pointer))
(cffi:defcfun ("sqlite3_finalize" %sqlite-finalize) :int (statement :pointer))
(cffi:defcfun ("sqlite3_step" %sqlite-step) :int (statement :pointer))
(cffi:defcfun ("sqlite3_column_count" %sqlite-column-count) :int (statement :pointer))
(cffi:defcfun ("sqlite3_column_name" %sqlite-column-name) :string (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_type" %sqlite-column-type) :int (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_int64" %sqlite-column-int64) :int64 (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_double" %sqlite-column-double) :double (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_text" %sqlite-column-text) :pointer (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_blob" %sqlite-column-blob) :pointer (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_column_bytes" %sqlite-column-bytes) :int (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_bind_parameter_count" %sqlite-bind-parameter-count) :int (statement :pointer))
(cffi:defcfun ("sqlite3_bind_null" %sqlite-bind-null) :int (statement :pointer) (i :int))
(cffi:defcfun ("sqlite3_bind_int64" %sqlite-bind-int64) :int (statement :pointer) (i :int) (value :int64))
(cffi:defcfun ("sqlite3_bind_double" %sqlite-bind-double) :int (statement :pointer) (i :int) (value :double))
(cffi:defcfun ("sqlite3_bind_text" %sqlite-bind-text) :int
  (statement :pointer) (i :int) (text :pointer) (bytes :int) (destructor :pointer))
(cffi:defcfun ("sqlite3_bind_blob" %sqlite-bind-blob) :int
  (statement :pointer) (i :int) (blob :pointer) (bytes :int) (destructor :pointer))

(defun sqlite-transient ()
  "SQLITE_TRANSIENT: SQLite copies a bound value before the call returns."
  (cffi:make-pointer (ldb (byte (* 8 (cffi:foreign-type-size :pointer)) 0) -1)))

(defun sqlite-version ()
  (load-sqlite)
  (%sqlite-libversion))

(defclass sqlite-database (database)
  ((handle :initarg :handle :accessor sqlite-handle)
   (path :initarg :path :reader sqlite-path)))

(defun sqlite-error (db format &rest args)
  (editor-error "~?: ~a" format args
                (if (and db (not (cffi:null-pointer-p (sqlite-handle db))))
                    (%sqlite-errmsg (sqlite-handle db))
                    "no database")))

(defun open-sqlite (path &key read-only create)
  "Open the SQLite database file at PATH. Without CREATE, the file must exist."
  (load-sqlite)
  (let ((name (uiop:native-namestring path)))
    (unless (or create (probe-file name))
      (editor-error "No database file at ~a" name))
    (cffi:with-foreign-object (out :pointer)
      (let* ((flags (cond (read-only +sqlite-open-readonly+)
                          (create (logior +sqlite-open-readwrite+ +sqlite-open-create+))
                          (t +sqlite-open-readwrite+)))
             (code (%sqlite-open name out flags (cffi:null-pointer)))
             (handle (cffi:mem-ref out :pointer)))
        (unless (= code +sqlite-ok+)
          (let ((why (if (cffi:null-pointer-p handle) "out of memory" (%sqlite-errmsg handle))))
            (unless (cffi:null-pointer-p handle) (%sqlite-close handle))
            (editor-error "Can't open ~a: ~a" name why)))
        (%sqlite-busy-timeout handle 2000)
        (make-instance 'sqlite-database :handle handle :path (pathname name) :read-only read-only)))))

(defmethod db-close ((db sqlite-database))
  (unless (cffi:null-pointer-p (sqlite-handle db))
    (%sqlite-close (sqlite-handle db))
    (setf (sqlite-handle db) (cffi:null-pointer))))

(defmethod db-open-p ((db sqlite-database))
  (not (cffi:null-pointer-p (sqlite-handle db))))

(defmethod db-interrupt ((db sqlite-database))
  (when (db-open-p db) (%sqlite-interrupt (sqlite-handle db))))

(defun sqlite-bind (db statement params)
  (let ((count (%sqlite-bind-parameter-count statement)))
    (unless (= count (length params))
      (editor-error "The statement takes ~d parameter~:p, not ~d" count (length params)))
    (loop for value in params
          for i from 1
          do (let ((code
                     (etypecase value
                       ((member :null nil) (%sqlite-bind-null statement i))
                       ((signed-byte 64) (%sqlite-bind-int64 statement i value))
                       (real (%sqlite-bind-double statement i (coerce value 'double-float)))
                       (string (cffi:with-foreign-string ((text bytes) value :null-terminated-p nil)
                                 (%sqlite-bind-text statement i text bytes (sqlite-transient))))
                       ((vector (unsigned-byte 8))
                        (cffi:with-foreign-object (blob :uint8 (max 1 (length value)))
                          (dotimes (j (length value)) (setf (cffi:mem-aref blob :uint8 j) (aref value j)))
                          (%sqlite-bind-blob statement i blob (length value) (sqlite-transient)))))))
               (unless (= code +sqlite-ok+) (sqlite-error db "Can't bind parameter ~d" i))))))

(defun sqlite-column-value (statement i)
  (case (%sqlite-column-type statement i)
    (1 (%sqlite-column-int64 statement i))
    (2 (%sqlite-column-double statement i))
    (3 (let ((bytes (%sqlite-column-bytes statement i))
             (text (%sqlite-column-text statement i)))
         (if (cffi:null-pointer-p text) "" (cffi:foreign-string-to-lisp text :count bytes :encoding :utf-8))))
    (4 (let* ((blob (%sqlite-column-blob statement i))
              (bytes (%sqlite-column-bytes statement i))
              (octets (make-array bytes :element-type '(unsigned-byte 8))))
         (dotimes (j bytes) (setf (aref octets j) (cffi:mem-aref blob :uint8 j)))
         octets))
    (t :null)))

(defun sqlite-run-one (db statement limit)
  "Step STATEMENT to its end, or to LIMIT rows. Returns its column names, its
rows (vectors), whether there were more than LIMIT rows, and the rows it changed."
  (let* ((count (%sqlite-column-count statement))
         (columns (loop for i below count collect (%sqlite-column-name statement i)))
         (rows '())
         (n 0)
         (more nil))
    (loop
      (let ((code (%sqlite-step statement)))
        (cond ((= code +sqlite-done+) (return))
              ((/= code +sqlite-row+) (sqlite-error db "Query failed"))
              ((and limit (>= n limit)) (setf more t) (return))
              (t (let ((row (make-array count)))
                   (dotimes (i count) (setf (aref row i) (sqlite-column-value statement i)))
                   (push row rows)
                   (incf n))))))
    (values columns (nreverse rows) more (if (zerop count) (%sqlite-changes (sqlite-handle db)) 0))))

(defmethod db-execute ((db sqlite-database) sql &key params limit)
  "Run SQL, which may hold several statements (PARAMS go to the last). Returns
the last statement's results as a DB-RESULT."
  (unless (db-open-p db) (editor-error "The database is closed"))
  (let ((start (get-internal-real-time))
        (result nil)
        (changes 0))
    (cffi:with-foreign-string ((text bytes) sql)
      (let* ((end (cffi:inc-pointer text (1- bytes)))
             (at text))
        (cffi:with-foreign-objects ((out :pointer) (tail :pointer))
          (loop while (< (cffi:pointer-address at) (cffi:pointer-address end))
                do (let ((code (%sqlite-prepare (sqlite-handle db) at -1 out tail))
                         (statement (cffi:mem-ref out :pointer)))
                     (unless (= code +sqlite-ok+) (sqlite-error db "SQL error"))
                     (setf at (cffi:mem-ref tail :pointer))
                     (unless (cffi:null-pointer-p statement)
                       (unwind-protect
                            (let ((last (>= (cffi:pointer-address (skip-sql-space at end))
                                            (cffi:pointer-address end))))
                              (when last (sqlite-bind db statement params))
                              (multiple-value-bind (columns rows more changed)
                                  (sqlite-run-one db statement limit)
                                (incf changes changed)
                                (when (or columns last (null result))
                                  (setf result (make-db-result :columns columns :rows rows :more more)))))
                         (%sqlite-finalize statement))))))))
    (unless result (setf result (make-db-result)))
    (setf (db-result-changes result) changes
          (db-result-milliseconds result) (round (* 1000 (- (get-internal-real-time) start))
                                                 internal-time-units-per-second))
    result))

(defun skip-sql-space (at end)
  "The first pointer from AT (before END) that isn't ASCII whitespace."
  (loop while (and (< (cffi:pointer-address at) (cffi:pointer-address end))
                   (member (cffi:mem-ref at :uint8) '(9 10 13 32)))
        do (setf at (cffi:inc-pointer at 1)))
  at)

;;; The schema

(defmethod db-tables ((db sqlite-database))
  (mapcar (lambda (row) (list :name (aref row 0) :kind (if (string= (aref row 1) "view") :view :table)))
          (db-result-rows
           (db-execute db "SELECT name, type FROM sqlite_schema
WHERE type IN ('table', 'view') AND name NOT LIKE 'sqlite_%' ORDER BY type, name"))))

(defmethod db-columns ((db sqlite-database) table)
  (mapcar (lambda (row)
            (list :name (aref row 1) :type (aref row 2) :not-null (eql 1 (aref row 3))
                  :default (let ((d (aref row 4))) (if (eq d :null) nil d))
                  :primary-key (let ((pk (aref row 5))) (and (plusp pk) pk))))
          (db-result-rows (db-execute db (format nil "PRAGMA table_info(~a)" (quote-sql-identifier table))))))

(defmethod db-indexes ((db sqlite-database) table)
  (loop for row in (db-result-rows (db-execute db (format nil "PRAGMA index_list(~a)" (quote-sql-identifier table))))
        for name = (aref row 1)
        collect (list :name name :unique (eql 1 (aref row 2))
                      :columns (mapcar (lambda (r) (let ((c (aref r 2))) (if (eq c :null) "(expression)" c)))
                                       (db-result-rows
                                        (db-execute db (format nil "PRAGMA index_info(~a)" (quote-sql-identifier name))))))))

(defmethod db-foreign-keys ((db sqlite-database) table)
  (mapcar (lambda (row) (list :column (aref row 3) :table (aref row 2)
                              :to (let ((to (aref row 4))) (if (eq to :null) nil to))))
          (db-result-rows (db-execute db (format nil "PRAGMA foreign_key_list(~a)" (quote-sql-identifier table))))))

(defmethod db-create-statement ((db sqlite-database) name)
  (let ((rows (db-result-rows (db-execute db "SELECT sql FROM sqlite_schema WHERE name = ?" :params (list name)))))
    (and rows (let ((sql (aref (first rows) 0))) (if (eq sql :null) nil sql)))))

(defmethod db-row-key ((db sqlite-database) table)
  "The columns naming a row of TABLE: its primary key, else SQLite's rowid
\(nil for a view, or a table WITHOUT ROWID and no key)."
  (let ((kind (getf (find table (db-tables db) :key (lambda (x) (getf x :name)) :test #'string=) :kind)))
    (when (eq kind :table)
      (let ((key (sort (remove nil (db-columns db table) :key (lambda (c) (getf c :primary-key)))
                       #'< :key (lambda (c) (getf c :primary-key)))))
        (cond (key (mapcar (lambda (c) (getf c :name)) key))
              ((search "WITHOUT ROWID" (string-upcase (or (db-create-statement db table) ""))) nil)
              (t (list :rowid)))))))
