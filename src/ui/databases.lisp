;;;; databases.lisp — the Databases page, database tables in tabs, and SQL consoles
;;;;
;;;; The Databases page (the activity bar's cylinder) lists the saved
;;;; connections: the open folder's (its .cadre/databases.sexp, which can be
;;;; committed) and your own (the config directory's databases.sexp). Each
;;;; can be opened, closed, edited, duplicated and deleted; Add makes a new
;;;; one. An open connection lists its tables and views, and its SQL Console.
;;;;
;;;; A table opens in a tab: its rows a page at a time, a filter above each
;;;; column (text it contains, or =, !=, <, >, <=, >= a value, NULL, !NULL),
;;;; sorted by clicking a column's name. A table with a key can be edited:
;;;; double-click a cell, Add a row, check rows and Delete them. Changes are
;;;; held, shown in color, until Save (Ctrl+S) writes them in one
;;;; transaction, or Discard drops them; closing the tab asks. Structure shows
;;;; the columns, indexes, foreign keys and the CREATE statement.
;;;;
;;;; A SQL console is a SQL buffer tied to a connection (any .sql file can be
;;;; too): Run Statement runs the statement at the cursor, or the selection,
;;;; and Run All SQL the whole file, in a thread; the rows go to the panel's
;;;; Results page.
;;;;
;;;; The core (database/) holds the connections, the backends and the SQL.

(in-package #:cadre-ui)

(define-major-mode db-table-mode (:title "Table")
  "A database table's rows, in a tab.")

(define-option *db-page-size* 100 (integer 10 5000)
  "How many rows of a database table its tab shows at a time."
  :category "Other")

(define-option *db-result-limit* 500 (integer 10 100000)
  "The most rows a SQL query shows on the Results page."
  :category "Other")

(defstruct (db-table-page (:conc-name dbt-))
  connection-id table
  (kind :table)
  (key '())                             ; the key columns (or (:rowid)): the first values of each row
  (columns '())                         ; the column plists (DB-COLUMNS)
  (filters (make-hash-table :test 'equal))
  sort descending
  (offset 0)
  (total 0)
  (rows '())
  (edits (make-hash-table :test 'equal)) ; (row . column) → its new value
  (new-rows '())                        ; rows to add: vectors of values, nil for the column's default
  (deleted (make-hash-table))           ; row → t, for rows to delete
  (checked (make-hash-table))           ; row → t, for rows checked in the first column
  (cells (make-hash-table :test 'eq))   ; cell widget → (row . column)
  buffer grid (shown-rows 0) stack info save-button discard-button delete-button add-button prev next
  structure)

;;; Connections

(defvar *db-connections* '() "The saved connections: the project's, then your own.")
(defvar *open-databases* (make-hash-table :test 'equal) "Connection id → its open DATABASE.")
(defvar *db-connections-project* nil "The project whose connections *db-connections* holds.")

(defun global-db-connections-file () (merge-pathnames "databases.sexp" (config-directory)))

(defun db-project () (and *window* (window-project *window*)))

(defun load-db-connections ()
  (let ((project (db-project)))
    (setf *db-connections-project* project
          *db-connections* (append (and project (read-db-connections (project-db-connections-file project) :project))
                                   (read-db-connections (global-db-connections-file) :global)))))

(defun save-db-connections ()
  (let ((project (db-project)))
    (write-db-connections (global-db-connections-file)
                          (remove :project *db-connections* :key #'db-connection-scope))
    (when project
      (let ((ours (remove :global *db-connections* :key #'db-connection-scope))
            (file (project-db-connections-file project)))
        (when (or ours (probe-file file))
          (write-db-connections file ours))))))

(defun find-db-connection (id)
  (find id *db-connections* :key #'db-connection-id :test #'equal))

(defun connection-database (connection)
  (gethash (db-connection-id connection) *open-databases*))

(defun db-passwords-file ()
  "Where server databases' passwords are kept: outside any project, only you can read it."
  (merge-pathnames "secrets/database-passwords.json" (config-directory)))

(defun ensure-database (connection)
  "CONNECTION's open database, opening it if need be."
  (or (connection-database connection)
      (let ((db (open-database connection :project (db-project)
                                          :password (read-db-password (db-passwords-file) (db-connection-id connection)))))
        (setf (gethash (db-connection-id connection) *open-databases*) db)
        (refresh-databases-page)
        db)))

(defun open-db-connection (connection)
  (ensure-database connection)
  (message "Opened ~a" (db-connection-name connection)))

(defun db-table-buffers (connection)
  (remove-if-not (lambda (b) (let ((page (buffer-local b :db-table)))
                               (and page (equal (dbt-connection-id page) (db-connection-id connection)))))
                 (buffer-list)))

(defun close-db-connection (connection &key quietly)
  "Close CONNECTION's database and its tables' tabs. Refuses while a table has unsaved changes."
  (let ((buffers (db-table-buffers connection)))
    (let ((changed (find-if #'buffer-modified-p buffers)))
      (when changed
        (editor-error "Save or discard the changes to ~a first" (buffer-name changed))))
    (dolist (buffer buffers)
      (dolist (view (buffer-views *window* buffer))
        (close-view *window* view))))
  (let ((db (connection-database connection)))
    (when db
      (db-close db)
      (remhash (db-connection-id connection) *open-databases*)))
  (refresh-databases-page)
  (unless quietly (message "Closed ~a" (db-connection-name connection))))

(defun databases-project-changed ()
  "Another folder opened: show its connections, closing the last one's."
  (unless (equal *db-connections-project* (db-project))
    (dolist (connection *db-connections*)
      (when (and (eq (db-connection-scope connection) :project) (connection-database connection))
        (ignore-errors (close-db-connection connection :quietly t))))
    (load-db-connections)
    (refresh-databases-page)))

;;; The page

(defvar *databases-list* nil)
(defvar *databases-status* nil)

(defun make-databases-widget ()
  (setf *databases-list* (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")
                                                      :margin-start 8 :margin-end 8 :margin-top 4 :valign :start)
        *databases-status* (make-instance 'gtk:label :wrap t :xalign 0.0 :margin-start 12 :margin-end 12
                                                     :margin-top 8 :css-classes '("dim-label")))
  (gtk:build
    (gtk:box :orientation :vertical
      (gtk:box :margin-start 12 :margin-end 6 :margin-top 4
        (gtk:label :label "DATABASES" :xalign 0.0 :hexpand t :css-classes '("caption-heading" "dim-label"))
        (gtk:button :icon-name "view-refresh-symbolic" :tooltip-text "Refresh" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (load-db-connections) (refresh-databases-page)))
        (gtk:button :icon-name "list-add-symbolic" :tooltip-text "Add a connection…" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'db-add-connection))))
      (gtk:scrolled-window :vexpand t :hscrollbar-policy :never
        (gtk:box :orientation :vertical
          *databases-status*
          *databases-list*)))))

(defun actions-menu-button (tooltip items)
  "A ⋮ button whose menu has ITEMS, (label . function) each."
  (let* ((box (make-instance 'gtk:box :orientation :vertical))
         (popover (make-instance 'gtk:popover :child box :has-arrow nil :css-classes '("menu")))
         (button (make-instance 'gtk:menu-button :icon-name "view-more-symbolic" :tooltip-text tooltip
                                                 :valign :center :css-classes '("flat") :popover popover)))
    (loop for (label . action) in items
          do (let ((item (make-instance 'gtk:button :label label :css-classes '("flat")))
                   (action action))
               (gtk:widget-set-halign (gtk:button-get-child item) :start)
               (gobject:connect item :clicked (lambda (b) (declare (ignore b))
                                                (gtk:popover-popdown popover)
                                                (handler-case (funcall action)
                                                  (editor-error (e) (message "~a" (editor-error-message e))))))
               (gtk:box-append box item)))
    button))

(defun guarded (function)
  (lambda (&rest args)
    (declare (ignore args))
    (handler-case (funcall function)
      (editor-error (e) (message "~a" (editor-error-message e))))))

(defun connection-row (connection)
  (let* ((db (connection-database connection))
         (row (make-instance 'adw:expander-row :title (db-connection-name connection) :title-lines 1 :subtitle-lines 1
                                               :subtitle (format nil "~:[~;Open · ~]~a~:[~; · in the project~]"
                                                                 db (db-connection-description connection)
                                                                 (eq (db-connection-scope connection) :project))))
         (toggle (flat-button (if db "Close" "Open") (if db "Close the database" "Open the database")
                              (lambda () (if (connection-database connection)
                                             (close-db-connection connection)
                                             (open-db-connection connection))))))
    (gtk:widget-set-tooltip-text row (let ((file (db-connection-file connection (db-project))))
                                       (and file (uiop:native-namestring file))))
    (adw:expander-row-add-suffix row toggle)
    (adw:expander-row-add-suffix
     row (actions-menu-button
          "More"
          (append (list (cons "New SQL Console" (lambda () (new-sql-console connection))))
                  (and db (list (cons "Refresh Tables" #'refresh-databases-page)))
                  (list (cons "Edit…" (lambda () (edit-db-connection connection)))
                        (cons "Duplicate" (lambda () (duplicate-db-connection connection)))
                        (cons "Delete…" (lambda () (delete-db-connection connection)))))))
    (cond (db
           (let ((console (one-line-row :title "SQL Console" :subtitle "Run SQL against this database" :activatable t)))
             (adw:action-row-add-prefix console (make-instance 'gtk:image :icon-name "cadre-terminal-symbolic"))
             (gobject:connect console :activated (guarded (lambda () (new-sql-console connection))))
             (adw:expander-row-add-row row console))
           (let ((tables (handler-case (db-run-tables db) (editor-error (e) (message "~a" (editor-error-message e)) nil))))
             (unless tables
               (adw:expander-row-add-row row (one-line-row :title "No tables")))
             (dolist (table tables)
               (let ((name (getf table :name))
                     (table-row (one-line-row :title (getf table :name) :activatable t
                                              :subtitle (if (eq (getf table :kind) :view) "view" ""))))
                 (adw:action-row-add-prefix table-row (make-instance 'gtk:image :icon-name (if (eq (getf table :kind) :view)
                                                                                              "view-reveal-symbolic"
                                                                                              "view-grid-symbolic")))
                 (gobject:connect table-row :activated (guarded (lambda () (open-db-table connection name))))
                 (adw:expander-row-add-row row table-row))))
           (adw:expander-row-set-expanded row t))
          (t
           ;; Expanding a closed connection opens it.
           (adw:expander-row-add-row row (one-line-row :title "Opening…"))
           (gobject:connect row "notify::expanded"
                            (lambda (r pspec)
                              (declare (ignore pspec))
                              (when (and (adw:expander-row-get-expanded r) (not (connection-database connection)))
                                (glib:idle-add glib:+priority-default+
                                               (lambda ()
                                                 (handler-case (open-db-connection connection)
                                                   (editor-error (e)
                                                     (message "~a" (editor-error-message e))
                                                     (refresh-databases-page)))
                                                 nil)))))))
    row))

(defun db-run-tables (db)
  (sb-thread:with-mutex ((db-lock db)) (db-tables db)))

(defun refresh-databases-page ()
  (clrhash *db-schema-cache*)
  (when *databases-list*
    (gtk:list-box-remove-all *databases-list*)
    (dolist (connection *db-connections*)
      (gtk:list-box-append *databases-list* (connection-row connection)))
    (let ((none (null *db-connections*)))
      (gtk:label-set-text *databases-status*
                          (if none
                              "No database connections yet. Add a SQLite file with +."
                              ""))
      (gtk:widget-set-visible *databases-status* none))))

(define-command show-databases ()
  "Show the Databases page: the saved database connections."
  (show-sidebar-page *window* "databases" :toggle nil))

;;; Adding, editing and deleting connections

(defvar *db-connection-dialog* nil "The open connection dialog's parts (for the smoke test).")

(defun db-connection-dialog (&optional existing)
  "Ask for a connection's details: a new one, or EXISTING's, edited."
  (let* ((project (db-project))
         (file (and existing (db-connection-file existing project)))
         (dialog (make-instance 'adw:dialog :title (if existing "Edit Connection" "Add Database Connection")
                                            :content-width 520))
         (name (make-instance 'adw:entry-row :title "Name" :text (if existing (db-connection-name existing) "")))
         (kind (combo-row "Kind" *database-kinds*))
         (location (make-instance 'adw:action-row :title "File" :subtitle-lines 2))
         (choose (make-instance 'gtk:button :label "Choose…" :valign :center))
         (create-file (make-instance 'gtk:button :label "New…" :valign :center
                                                 :tooltip-text "Make a new, empty database file"))
         (host (make-instance 'adw:entry-row :title "Host"
                                             :text (or (and existing (db-connection-host existing)) "localhost")))
         (port (make-instance 'adw:entry-row :title "Port"
                                             :text (princ-to-string (or (and existing (db-connection-port existing)) 5432))))
         (database (make-instance 'adw:entry-row :title "Database"
                                                 :text (or (and existing (db-connection-database existing)) "postgres")))
         (user (make-instance 'adw:entry-row :title "User"
                                             :text (or (and existing (db-connection-user existing)) (uiop:getenv "USER") "")))
         (password (make-instance 'adw:password-entry-row :title "Password (kept outside the project)"
                                                          :text (or (and existing (read-db-password (db-passwords-file)
                                                                                                    (db-connection-id existing)))
                                                                    "")))
         (read-only (make-instance 'adw:switch-row :title "Read-only"
                                                   :subtitle "Look, but don't change anything"
                                                   :active (and existing (db-connection-read-only existing))))
         (in-project (make-instance 'adw:switch-row :title "Save with the project"
                                                    :subtitle (if project
                                                                  "In .cadre/databases.sexp, which you can commit"
                                                                  "Open a folder to save connections with it")
                                                    :sensitive (and project t)
                                                    :active (if existing
                                                                (eq (db-connection-scope existing) :project)
                                                                (and project t))))
         (test (make-instance 'gtk:button :label "_Test Connection" :use-underline t :halign :start))
         (note (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label" "caption")))
         (save (make-instance 'gtk:button :label (if existing "_Save" "_Add") :use-underline t :sensitive nil
                                          :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "_Cancel" :use-underline t)))
    (labels ((text (row) (string-trim " " (gtk:editable-get-text row)))
             (say (format &rest args)
               (gtk:label-set-text note (apply #'format nil format args)))
             (server-p () (not (eq (combo-choice kind *database-kinds*) :sqlite)))
             (update ()
               (let ((server (server-p)))
                 (gtk:widget-set-visible location (not server))
                 (dolist (row (list host port database user password)) (gtk:widget-set-visible row server))
                 (adw:action-row-set-subtitle location (if file (uiop:native-namestring file) "None chosen"))
                 (gtk:widget-set-sensitive save (and (plusp (length (text name)))
                                                     (if server
                                                         (and (plusp (length (text host))) (plusp (length (text database))))
                                                         file)))))
             (use-file (path)
               (setf file (pathname path))
               (when (string= (text name) "")
                 (gtk:editable-set-text name (or (pathname-name file) "")))
               (update))
             (made ()
               (let ((scope (if (and project (adw:switch-row-get-active in-project)) :project :global)))
                 (if (server-p)
                     (make-db-connection :id (if existing (db-connection-id existing) (make-uuid))
                                         :name (text name) :kind (combo-choice kind *database-kinds*)
                                         :host (text host) :port (or (ignore-errors (parse-integer (text port))) 5432)
                                         :database (text database) :user (let ((u (text user))) (and (plusp (length u)) u))
                                         :read-only (adw:switch-row-get-active read-only) :scope scope)
                     (make-db-connection :id (if existing (db-connection-id existing) (make-uuid))
                                         :name (text name) :kind (combo-choice kind *database-kinds*)
                                         :path (path-for-connection file scope project)
                                         :read-only (adw:switch-row-get-active read-only) :scope scope))))
             (folder ()
               (gio:file-new-for-path (uiop:native-namestring (or (and file (uiop:pathname-directory-pathname file))
                                                                  project (user-homedir-pathname)))))
             (save-it ()
               (handler-case
                   (let ((connection (made)))
                     (when (and existing (connection-database existing))
                       (close-db-connection existing :quietly t))
                     (setf *db-connections*
                           (if existing
                               (substitute connection existing *db-connections*)
                               (append *db-connections* (list connection))))
                     ;; The project's first, as when loaded.
                     (setf *db-connections* (stable-sort (copy-list *db-connections*) #'string>
                                                         :key (lambda (c) (string (db-connection-scope c)))))
                     (save-db-connections)
                     (when (server-p)
                       (write-db-password (db-passwords-file) (db-connection-id connection)
                                          (gtk:editable-get-text password)))
                     (refresh-databases-page)
                     (adw:dialog-close dialog)
                     (message "~:[Added~;Saved~] ~a" existing (db-connection-name connection)))
                 (error (e) (say "~a" e)))))
      (when existing
        (adw:combo-row-set-selected kind (or (position (db-connection-kind existing) *database-kinds* :key #'car) 0)))
      (adw:action-row-add-suffix location choose)
      (adw:action-row-add-suffix location create-file)
      (gobject:connect choose :clicked
                       (lambda (b) (declare (ignore b))
                         (let ((chooser (gtk:file-dialog-new)))
                           (gtk:file-dialog-set-initial-folder chooser (folder))
                           (gio:async (gtk:file-dialog-open chooser (window-gtk-window *window*))
                                      (lambda (f) (use-file (gio:file-get-path f)))
                                      :error (lambda (e) (declare (ignore e)))))))
      (gobject:connect create-file :clicked
                       (lambda (b) (declare (ignore b))
                         (let ((chooser (gtk:file-dialog-new)))
                           (gtk:file-dialog-set-initial-folder chooser (folder))
                           (gtk:file-dialog-set-initial-name chooser "database.db")
                           (gio:async (gtk:file-dialog-save chooser (window-gtk-window *window*))
                                      (lambda (f)
                                        (let ((path (gio:file-get-path f)))
                                          (handler-case
                                              (progn (db-close (open-sqlite path :create t))
                                                     (use-file path)
                                                     (say "Made ~a" path))
                                            (editor-error (e) (say "~a" (editor-error-message e))))))
                                      :error (lambda (e) (declare (ignore e)))))))
      (gobject:connect test :clicked
                       (lambda (b) (declare (ignore b))
                         (if (and (not (server-p)) (null file))
                             (say "Choose the database file first.")
                             (handler-case
                                 (let ((db (open-database (made) :project project :password (gtk:editable-get-text password))))
                                   (unwind-protect
                                        (say "Connected: ~d table~:p and view~:p (~a)."
                                             (length (db-run-tables db))
                                             (if (server-p)
                                                 (let ((v (aref (first (db-result-rows (db-run db "SELECT version()"))) 0)))
                                                   (subseq v 0 (or (position #\Space v :start (1+ (or (position #\Space v) 0))) (length v))))
                                                 (format nil "SQLite ~a" (sqlite-version))))
                                     (db-close db)))
                               (editor-error (e) (say "~a" (editor-error-message e)))))))
      (gobject:connect name :changed (lambda (e) (declare (ignore e)) (update)))
      (dolist (row (list host database)) (gobject:connect row :changed (lambda (e) (declare (ignore e)) (update))))
      (gobject:connect kind "notify::selected"
                       (lambda (r p) (declare (ignore r p))
                         ;; The kind's usual port and database, unless others were typed in.
                         (let ((k (combo-choice kind *database-kinds*)))
                           (when (member (text port) '("" "5432" "3306") :test #'string=)
                             (gtk:editable-set-text port (case k (:mysql "3306") (t "5432"))))
                           (when (and (eq k :mysql) (string= (text database) "postgres"))
                             (gtk:editable-set-text database ""))
                           (when (and (eq k :postgres) (string= (text database) ""))
                             (gtk:editable-set-text database "postgres")))
                         (update)))
      (gobject:connect name :entry-activated (lambda (e) (declare (ignore e))
                                               (when (gtk:widget-get-sensitive save) (save-it))))
      (gobject:connect save :clicked (lambda (b) (declare (ignore b)) (save-it)))
      (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
      (let ((group (make-instance 'adw:preferences-group))
            (options (make-instance 'adw:preferences-group)))
        (dolist (row (list name kind location host port database user password)) (adw:preferences-group-add group row))
        (dolist (row (list read-only in-project)) (adw:preferences-group-add options row))
        (let ((header (make-instance 'adw:header-bar :show-end-title-buttons nil :show-start-title-buttons nil))
              (toolbar (make-instance 'adw:toolbar-view)))
          (adw:header-bar-pack-start header cancel)
          (adw:header-bar-pack-end header save)
          (adw:toolbar-view-add-top-bar toolbar header)
          (adw:toolbar-view-set-content toolbar
                                        (gtk:build
                                          (gtk:box :orientation :vertical :spacing 18
                                                   :margin-start 18 :margin-end 18 :margin-top 12 :margin-bottom 18
                                            group options test note)))
          (adw:dialog-set-child dialog toolbar)))
      (update)
      (setf *db-connection-dialog* (list :dialog dialog :name name :read-only read-only :in-project in-project
                                         :kind kind :host host :port port :database database :user user :password password
                                         :save save :test test :note note
                                         :use-file #'use-file :save-it #'save-it))
      (adw:dialog-present dialog (window-gtk-window *window*))
      (gtk:widget-grab-focus name))))

(define-command db-add-connection ()
  "Add a database connection (a SQLite file, a PostgreSQL, MySQL or MariaDB server) to the Databases page."
  (show-sidebar-page *window* "databases" :toggle nil)
  (db-connection-dialog))

(defun edit-db-connection (connection)
  (db-connection-dialog connection))

(defun duplicate-db-connection (connection)
  (let ((copy (copy-db-connection connection)))
    (setf (db-connection-id copy) (make-uuid)
          (db-connection-name copy) (format nil "~a copy" (db-connection-name connection)))
    (let ((password (read-db-password (db-passwords-file) (db-connection-id connection))))
      (when password (write-db-password (db-passwords-file) (db-connection-id copy) password)))
    (setf *db-connections* (let ((at (1+ (position connection *db-connections*))))
                             (append (subseq *db-connections* 0 at) (list copy) (nthcdr at *db-connections*))))
    (save-db-connections)
    (refresh-databases-page)
    (message "Made ~a" (db-connection-name copy))))

(defun delete-db-connection (connection)
  (let ((dialog (adw:alert-dialog-new (format nil "Delete the connection “~a”?" (db-connection-name connection))
                                      "The database itself isn't touched: only Cadre's connection to it goes.")))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "delete" "_Delete")
    (adw:alert-dialog-set-response-appearance dialog "delete" :destructive)
    (adw:alert-dialog-set-default-response dialog "cancel")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "delete")
                   (handler-case
                       (progn
                         (when (connection-database connection)
                           (close-db-connection connection :quietly t))
                         (setf *db-connections* (remove connection *db-connections*))
                         (save-db-connections)
                         (when (db-connection-server-p connection)
                           (write-db-password (db-passwords-file) (db-connection-id connection) nil))
                         (refresh-databases-page)
                         (message "Deleted the connection ~a" (db-connection-name connection)))
                     (editor-error (e) (message "~a" (editor-error-message e)))))))))

;;; A table in a tab


(defun dbt-connection (page)
  (or (find-db-connection (dbt-connection-id page))
      (editor-error "The connection is gone")))

(defun dbt-database (page) (ensure-database (dbt-connection page)))

(defun dbt-key-count (page) (length (dbt-key page)))

(defun dbt-column-names (page) (mapcar (lambda (c) (getf c :name)) (dbt-columns page)))

(defun dbt-editable-p (page)
  (and (dbt-key page) (eq (dbt-kind page) :table)
       (not (db-connection-read-only (dbt-connection page)))))

(defun dbt-changed-p (page)
  (or (plusp (hash-table-count (dbt-edits page))) (dbt-new-rows page)
      (plusp (hash-table-count (dbt-deleted page)))))

(defun db-table-buffer (connection table)
  (find-if (lambda (b) (let ((page (buffer-local b :db-table)))
                         (and page (equal (dbt-connection-id page) (db-connection-id connection))
                              (string= (dbt-table page) table))))
           (buffer-list)))

(defun open-db-table (connection table)
  "Show TABLE of CONNECTION's database in a tab."
  (let ((buffer (db-table-buffer connection table)))
    (unless buffer
      (ensure-database connection)
      (let ((page (make-db-table-page :connection-id (db-connection-id connection) :table table)))
        (setf buffer (make-buffer :name (format nil "~a — ~a" table (db-connection-name connection))
                                  :text (make-gtk-text) :major-mode 'db-table-mode)
              (dbt-buffer page) buffer
              (buffer-local buffer :db-table) page
              (buffer-local buffer :tab-widget) (lambda (view) (declare (ignore view)) (make-db-table-widget page))
              (buffer-local buffer :save-function) (lambda (continuation) (save-db-table page continuation)))))
    (show-buffer *window* buffer)))

(defun icon-button (icon tooltip action &rest initargs)
  (let ((button (apply #'make-instance 'gtk:button :icon-name icon :tooltip-text tooltip :css-classes '("flat")
                       initargs)))
    (gobject:connect button :clicked (guarded action))
    button))

(defun make-db-table-widget (page)
  (let* ((data (make-instance 'gtk:toggle-button :label "Data" :active t))
         (structure (make-instance 'gtk:toggle-button :label "Structure" :group data))
         (stack (make-instance 'gtk:stack :vexpand t :hexpand t))
         (info (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :css-classes '("dim-label"))))
    (setf (dbt-stack page) stack
          (dbt-info page) info
          (dbt-add-button page) (icon-button "list-add-symbolic" "Add a row" (lambda () (add-db-row page)))
          (dbt-delete-button page) (icon-button "user-trash-symbolic" "Delete the checked rows"
                                                (lambda () (delete-checked-db-rows page)) :sensitive nil)
          (dbt-discard-button page) (flat-button "Discard" "Drop the changes" (lambda () (discard-db-changes page)))
          (dbt-save-button page) (make-instance 'gtk:button :label "Save" :tooltip-text "Write the changes to the database"
                                                            :css-classes '("suggested-action") :valign :center)
          (dbt-prev page) (icon-button "go-previous-symbolic" "The rows before" (lambda () (db-table-page-step page -1)))
          (dbt-next page) (icon-button "go-next-symbolic" "The rows after" (lambda () (db-table-page-step page 1)))
          (dbt-structure page) (make-instance 'gtk:box :orientation :vertical :margin-bottom 24))
    (gobject:connect (dbt-save-button page) :clicked
                     (guarded (lambda () (save-db-table page (lambda (ok) (declare (ignore ok)))))))
    (gtk:stack-add-named stack (make-instance 'gtk:scrolled-window :vexpand t :hexpand t) "data")
    (gtk:stack-add-named stack (make-instance 'gtk:scrolled-window :vexpand t :hexpand t :child (dbt-structure page))
                         "structure")
    (gobject:connect structure :toggled (lambda (b) (gtk:stack-set-visible-child-name
                                                     stack (if (gtk:toggle-button-get-active b) "structure" "data"))))
    (handler-case (load-db-table page :schema t)
      (editor-error (e) (gtk:label-set-text info (editor-error-message e))))
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 4 :margin-bottom 4
          (gtk:box :css-classes '("linked") data structure)
          info
          (dbt-add-button page) (dbt-delete-button page) (dbt-discard-button page) (dbt-save-button page)
          (gtk:separator :orientation :vertical)
          (icon-button "view-refresh-symbolic" "Read the table again" (lambda () (load-db-table page :schema t)))
          (icon-button "document-save-as-symbolic" "Save these rows as CSV…" (lambda () (export-db-rows-csv page)))
          (dbt-prev page) (dbt-next page))
        (gtk:separator)
        stack))))

(defun load-db-table (page &key schema)
  "Read the table's rows again (and with SCHEMA, its columns and key), from
its offset. Refuses while there are unsaved changes."
  (when (dbt-changed-p page)
    (editor-error "Save or discard the changes first"))
  (let* ((db (dbt-database page))
         (table (dbt-table page))
         (*identifier-quote* (db-identifier-quote db))
         (*text-type* (db-text-type db)))
    (when (or schema (null (dbt-columns page)))
      (sb-thread:with-mutex ((db-lock db))
        (setf (dbt-kind page) (or (getf (find table (db-tables db) :key (lambda (x) (getf x :name)) :test #'string=) :kind)
                                  (editor-error "There is no table ~a" table))
              (dbt-columns page) (db-columns db table)
              (dbt-key page) (db-row-key db table)))
      (unless (member (dbt-sort page) (dbt-column-names page) :test #'equal)
        (setf (dbt-sort page) nil))
      (build-db-grid page)
      (build-db-structure page db))
    (let ((filters (loop for name in (dbt-column-names page)
                         for text = (gethash name (dbt-filters page))
                         when text collect (cons name text))))
      (multiple-value-bind (sql params) (table-count-sql table :filters filters)
        (setf (dbt-total page) (aref (first (db-result-rows (db-run db sql :params params))) 0)))
      (when (>= (dbt-offset page) (max 1 (dbt-total page)))
        (setf (dbt-offset page) (* *db-page-size* (floor (max 0 (1- (dbt-total page))) *db-page-size*))))
      (multiple-value-bind (sql params)
          (table-select-sql table :key (dbt-key page) :filters filters :sort (dbt-sort page)
                                  :descending (dbt-descending page) :limit *db-page-size* :offset (dbt-offset page))
        (setf (dbt-rows page) (db-result-rows (db-run db sql :params params)))))
    (clrhash (dbt-checked page))
    (fill-db-rows page)))

(defun db-table-page-step (page direction)
  (let ((offset (+ (dbt-offset page) (* direction *db-page-size*))))
    (when (and (>= offset 0) (< offset (dbt-total page)))
      (let ((old (dbt-offset page)))
        (setf (dbt-offset page) offset)
        (handler-case (load-db-table page)
          (editor-error (e) (setf (dbt-offset page) old) (error e)))))))

(defun update-db-table-status (page)
  (let ((shown (length (dbt-rows page)))
        (filtered (some (lambda (c) (plusp (length (gethash (getf c :name) (dbt-filters page) ""))))
                        (dbt-columns page)))
        (editable (dbt-editable-p page))
        (changed (dbt-changed-p page)))
    (gtk:label-set-text (dbt-info page)
                        (format nil "~:[No rows~;Rows ~:*~:d–~:d of ~:d~]~:[~; (filtered)~]~@[ · ~a~]"
                                (and (plusp shown) (1+ (dbt-offset page))) (+ (dbt-offset page) shown) (dbt-total page)
                                filtered
                                (cond ((eq (dbt-kind page) :view) "a view: read-only")
                                      ((db-connection-read-only (dbt-connection page)) "read-only")
                                      ((null (dbt-key page)) "no key: read-only"))))
    (gtk:widget-set-visible (dbt-add-button page) editable)
    (gtk:widget-set-visible (dbt-delete-button page) editable)
    (gtk:widget-set-sensitive (dbt-delete-button page) (plusp (hash-table-count (dbt-checked page))))
    (gtk:widget-set-visible (dbt-save-button page) changed)
    (gtk:widget-set-visible (dbt-discard-button page) changed)
    (gtk:widget-set-sensitive (dbt-prev page) (plusp (dbt-offset page)))
    (gtk:widget-set-sensitive (dbt-next page) (< (+ (dbt-offset page) shown) (dbt-total page)))
    (setf (buffer-modified-p (dbt-buffer page)) changed)))

;;; The grid: row 0 the column names, row 1 the filters, then the rows.

(defun build-db-grid (page)
  (let ((grid (make-instance 'gtk:grid :css-classes '("cadre-db-grid") :halign :start :valign :start))
        (columns (dbt-columns page)))
    (setf (dbt-grid page) grid (dbt-shown-rows page) 0)
    (clrhash (dbt-cells page))
    (gtk:grid-attach grid (make-instance 'gtk:label :label "" :css-classes '("cadre-db-head")) 0 0 1 1)
    (gtk:grid-attach grid (make-instance 'gtk:image :icon-name "edit-find-symbolic" :css-classes '("dim-label")
                                                    :tooltip-text "Filters: text the column contains, or =, !=, <, >, <=, >= a value, NULL, !NULL")
                     0 1 1 1)
    (loop for column in columns
          for i from 1
          do (let* ((name (getf column :name))
                    (head (make-instance 'gtk:button :css-classes '("flat" "cadre-db-head")
                                                     :tooltip-text (format nil "~a~@[ ~a~]~:[~; NOT NULL~]~@[ · key ~d~]~%Click to sort"
                                                                           name (let ((type (getf column :type))) (and (plusp (length type)) type))
                                                                           (getf column :not-null) (getf column :primary-key))
                                                     :child (make-instance 'gtk:label :xalign 0.0
                                                                                      :label (db-head-label page name))))
                    (filter (make-instance 'gtk:entry :placeholder-text "filter" :width-chars 4
                                                      :text (gethash name (dbt-filters page) "")))
                    (pending 0))
               (gobject:connect head :clicked (guarded (lambda () (sort-db-table page name))))
               (gobject:connect filter :changed
                                (lambda (e)
                                  (setf (gethash name (dbt-filters page)) (gtk:editable-get-text e))
                                  (let ((mine (incf pending)))
                                    (glib:timeout-add glib:+priority-default+ 400
                                                      (lambda ()
                                                        (when (= mine pending) (filter-db-table page))
                                                        nil)))))
               (gobject:connect filter :activate (lambda (e) (declare (ignore e)) (incf pending) (filter-db-table page)))
               (gtk:grid-attach grid head i 0 1 1)
               (gtk:grid-attach grid filter i 1 1 1)))
    (setup-db-grid-menu page grid)
    (gtk:scrolled-window-set-child (gtk:stack-get-child-by-name (dbt-stack page) "data") grid)))

(defun db-head-label (page name)
  (format nil "~a~:[~; ~:[▲~;▼~]~]" name (equal name (dbt-sort page)) (dbt-descending page)))

(defun sort-db-table (page name)
  "Sort by NAME: ascending, then descending, then not."
  (let ((old (list (dbt-sort page) (dbt-descending page))))
    (cond ((not (equal name (dbt-sort page))) (setf (dbt-sort page) name (dbt-descending page) nil))
          ((not (dbt-descending page)) (setf (dbt-descending page) t))
          (t (setf (dbt-sort page) nil (dbt-descending page) nil)))
    (handler-case (load-db-table page)
      (editor-error (e) (setf (dbt-sort page) (first old) (dbt-descending page) (second old)) (error e)))
    (loop for column in (dbt-columns page)
          for i from 1
          do (gtk:label-set-text (gtk:button-get-child (gtk:grid-get-child-at (dbt-grid page) i 0))
                                 (db-head-label page (getf column :name))))))

(defun filter-db-table (page)
  (setf (dbt-offset page) 0)
  (handler-case (load-db-table page)
    (editor-error (e) (gtk:label-set-text (dbt-info page) (editor-error-message e)))))

(defun db-cell-value (page row column)
  "The value shown at ROW, COLUMN: an edit's, a new row's, or the table's."
  (let ((count (length (dbt-rows page))))
    (if (< row count)
        (multiple-value-bind (edit found) (gethash (cons row column) (dbt-edits page))
          (if found edit (aref (nth row (dbt-rows page)) (+ (dbt-key-count page) column))))
        (aref (nth (- row count) (dbt-new-rows page)) column))))

(defun db-original-value (page row column)
  (aref (nth row (dbt-rows page)) (+ (dbt-key-count page) column)))

(defun simple-db-value-p (value)
  "Whether VALUE can be edited in its cell: a number, NULL, or a line of text."
  (or (eq value :null) (null value) (realp value)
      (and (stringp value) (< (length value) 400) (not (find #\Newline value)))))

(defun fill-db-rows (page)
  "Show the rows (and the rows to add) below the filters."
  (let ((grid (dbt-grid page))
        (count (length (dbt-rows page)))
        (columns (length (dbt-columns page))))
    (dotimes (i (dbt-shown-rows page)) (gtk:grid-remove-row grid 2))
    (clrhash (dbt-cells page))
    (let* ((total (+ count (length (dbt-new-rows page))))
           (widths (db-column-widths page total)))
      (dotimes (row total)
        (gtk:grid-attach grid (db-row-head page row) 0 (+ row 2) 1 1)
        (dotimes (column columns)
          (gtk:grid-attach grid (make-db-cell page row column (aref widths column)) (1+ column) (+ row 2) 1 1)))
      ;; A filter is as wide as its column (an entry is otherwise 150 pixels).
      (dotimes (column columns)
        (gtk:editable-set-max-width-chars (gtk:grid-get-child-at grid (1+ column) 1) (aref widths column)))
      (setf (dbt-shown-rows page) total))
    (update-db-table-status page)))

(defun db-column-widths (page rows)
  "How many characters wide each column's cells are: as its widest value
shown (or its name), from 4 to 40."
  (let ((widths (map 'vector (lambda (c) (+ 2 (length (getf c :name)))) (dbt-columns page))))
    (dotimes (row rows)
      (dotimes (column (length widths))
        (setf (aref widths column)
              (max (aref widths column) (length (db-cell-text (db-cell-value page row column)))))))
    (map 'vector (lambda (w) (max 4 (min 40 w))) widths)))

(defun db-row-head (page row)
  (let ((count (length (dbt-rows page))))
    (if (and (dbt-editable-p page) (< row count))
        (let ((check (make-instance 'gtk:check-button :label (princ-to-string (+ 1 row (dbt-offset page)))
                                                      :active (gethash row (dbt-checked page)))))
          (gtk:widget-add-css-class check "dim-label")
          (gobject:connect check :toggled
                           (lambda (b)
                             (if (gtk:check-button-get-active b)
                                 (setf (gethash row (dbt-checked page)) t)
                                 (remhash row (dbt-checked page)))
                             (update-db-table-status page)))
          check)
        (make-instance 'gtk:label :label (if (< row count) (princ-to-string (+ 1 row (dbt-offset page))) "new")
                                  :xalign 1.0 :css-classes '("dim-label")))))

(defun style-db-cell (page cell row column)
  (let ((count (length (dbt-rows page)))
        (value (db-cell-value page row column)))
    (dolist (class '("cadre-db-null" "cadre-db-changed" "cadre-db-new" "cadre-db-deleted"))
      (gtk:widget-remove-css-class cell class))
    (when (eq value :null) (gtk:widget-add-css-class cell "cadre-db-null"))
    (cond ((>= row count) (gtk:widget-add-css-class cell "cadre-db-new"))
          ((gethash row (dbt-deleted page)) (gtk:widget-add-css-class cell "cadre-db-deleted"))
          ((nth-value 1 (gethash (cons row column) (dbt-edits page)))
           (gtk:widget-add-css-class cell "cadre-db-changed")))))

(defun db-cell-text (value)
  (cond ((null value) "")               ; a new row's default
        (t (substitute #\⏎ #\Newline (db-value-text value)))))

(defun make-db-cell (page row column &optional (width 40))
  (let* ((value (db-cell-value page row column))
         (editable (and (dbt-editable-p page) (not (gethash row (dbt-deleted page))) (simple-db-value-p value)))
         (cell (if editable
                   (make-instance 'gtk:editable-label :text (db-cell-text value)
                                                      :width-chars width :max-width-chars width
                                                      :tooltip-text (if (null value) "The column's default" nil))
                   (make-instance 'gtk:label :label (db-cell-text value) :xalign 0.0 :ellipsize :end
                                             :max-width-chars width))))
    (setf (gethash cell (dbt-cells page)) (cons row column))
    (when editable
      (gobject:connect cell "notify::editing"
                       (lambda (label pspec)
                         (declare (ignore pspec))
                         (let ((current (db-cell-value page row column)))
                           (if (gtk:editable-label-get-editing label)
                               ;; Editing NULL starts from nothing.
                               (when (eq current :null) (gtk:editable-set-text label ""))
                               (let ((text (gtk:editable-get-text label)))
                                 (if (and (member current '(:null nil)) (string= text ""))
                                     (gtk:editable-set-text label (db-cell-text current))
                                     (unless (string= text (db-cell-text current))
                                       (set-db-cell page row column text)))))))))
    (style-db-cell page cell row column)
    cell))

(defun set-db-cell (page row column value)
  "Make VALUE (text, or :NULL) ROW, COLUMN's new value, to be saved."
  (let ((count (length (dbt-rows page))))
    (if (< row count)
        (let ((original (db-original-value page row column)))
          (if (or (equal value original)
                  (and (stringp value) (not (eq original :null)) (string= value (db-value-text original :max nil))))
              (remhash (cons row column) (dbt-edits page))
              (setf (gethash (cons row column) (dbt-edits page)) value)))
        (setf (aref (nth (- row count) (dbt-new-rows page)) column) value))
    (refresh-db-cell page row column)
    (update-db-table-status page)))

(defun refresh-db-cell (page row column)
  (let ((cell (gtk:grid-get-child-at (dbt-grid page) (1+ column) (+ row 2))))
    (when cell
      (let ((value (db-cell-value page row column)))
        (if (typep cell 'gtk:editable-label)
            (gtk:editable-set-text cell (db-cell-text value))
            (gtk:label-set-text cell (db-cell-text value))))
      (style-db-cell page cell row column))))

(defun add-db-row (page)
  (unless (dbt-editable-p page) (editor-error "This table can't be edited here"))
  (setf (dbt-new-rows page) (append (dbt-new-rows page) (list (make-array (length (dbt-columns page)) :initial-element nil))))
  (gtk:stack-set-visible-child-name (dbt-stack page) "data")
  (fill-db-rows page)
  (let ((cell (gtk:grid-get-child-at (dbt-grid page) 1 (+ 1 (dbt-shown-rows page)))))
    (when cell (gtk:widget-grab-focus cell))))

(defun delete-checked-db-rows (page)
  (let ((rows (loop for row being the hash-keys of (dbt-checked page) collect row)))
    (unless rows (editor-error "Check the rows to delete first"))
    (dolist (row rows)
      (setf (gethash row (dbt-deleted page)) t))
    (clrhash (dbt-checked page))
    (fill-db-rows page)
    (message "~d row~:p to delete: Save deletes ~:*~[them~;it~:;them~]" (length rows))))

(defun discard-db-changes (page)
  (clrhash (dbt-edits page))
  (clrhash (dbt-deleted page))
  (setf (dbt-new-rows page) '())
  (fill-db-rows page)
  (message "Discarded the changes"))

(defun save-db-table (page continuation)
  "Write the changes in one transaction, then read the table again. Calls
CONTINUATION with t if they were written."
  (if (not (dbt-changed-p page))
      (progn (message "No changes to save") (funcall continuation t))
      (let* ((db (dbt-database page))
             (*identifier-quote* (db-identifier-quote db))
             (*text-type* (db-text-type db))
             (table (dbt-table page))
             (key (dbt-key page))
             (names (dbt-column-names page))
             (counts (list 0 0 0)))
        (flet ((key-values (row) (subseq (nth row (dbt-rows page)) 0 (length key))))
          (handler-case
              (sb-thread:with-mutex ((db-lock db))
                (db-execute db "BEGIN")
                (handler-case
                    (progn
                      (loop for row being the hash-keys of (dbt-deleted page)
                            do (multiple-value-bind (sql params) (delete-row-sql table key (key-values row))
                                 (db-execute db sql :params params)
                                 (incf (third counts))))
                      (let ((by-row (make-hash-table)))
                        (loop for (row . column) being the hash-keys of (dbt-edits page) using (hash-value value)
                              unless (gethash row (dbt-deleted page))
                                do (push (cons (nth column names) value) (gethash row by-row)))
                        (loop for row being the hash-keys of by-row using (hash-value changes)
                              do (multiple-value-bind (sql params) (update-row-sql table key (key-values row) changes)
                                   (when (zerop (db-result-changes (db-execute db sql :params params)))
                                     (editor-error "Row ~d changed or went away since it was read" (+ 1 row (dbt-offset page))))
                                   (incf (first counts)))))
                      (dolist (new (dbt-new-rows page))
                        (multiple-value-bind (sql params)
                            (insert-row-sql table (loop for value across new
                                                        for name in names
                                                        when value collect (cons name value)))
                          (db-execute db sql :params params)
                          (incf (second counts))))
                      (db-execute db "COMMIT"))
                  (error (e)
                    (ignore-errors (db-execute db "ROLLBACK"))
                    (error e))))
            (error (e)
              (let ((why (if (typep e 'editor-error) (editor-error-message e) (princ-to-string e))))
                (gtk:label-set-text (dbt-info page) (format nil "Not saved: ~a" why))
                (message "Not saved: ~a" why))
              (funcall continuation nil)
              (return-from save-db-table nil))))
        (clrhash (dbt-edits page))
        (clrhash (dbt-deleted page))
        (setf (dbt-new-rows page) '())
        (load-db-table page)
        (destructuring-bind (changed added deleted) counts
          (message "Saved ~a: ~d row~:p changed, ~d added, ~d deleted" table changed added deleted))
        (funcall continuation t))))

;;; Right-clicking a cell

(defun grid-cell-at (page widget)
  "The (row . column) of the cell WIDGET is (or is inside), or nil."
  (loop for w = widget then (gtk:widget-get-parent w)
        while w
        do (let ((place (gethash w (dbt-cells page))))
             (when place (return place)))
        until (eq w (dbt-grid page))))

(defun setup-db-grid-menu (page grid)
  (let ((click (gtk:gesture-click-new)))
    (gtk:gesture-single-set-button click 3)
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore gesture n))
                       (let ((place (grid-cell-at page (gtk:widget-pick grid x y '(:default)))))
                         (when place
                           (show-popover-menu grid x y (db-cell-menu-items page (car place) (cdr place)))))))
    (gtk:widget-add-controller grid click)))

(defun db-cell-menu-items (page row column)
  (let* ((value (db-cell-value page row column))
         (name (nth column (dbt-column-names page)))
         (editable (and (dbt-editable-p page) (not (gethash row (dbt-deleted page)))))
         (existing (< row (length (dbt-rows page)))))
    (append
     (and editable (not (typep value '(vector (unsigned-byte 8))))
          (list (cons "Edit…" (lambda () (edit-db-cell-dialog page row column)))))
     (and editable (not (getf (nth column (dbt-columns page)) :not-null)) (not (eq value :null))
          (list (cons "Set to NULL" (lambda () (set-db-cell page row column :null)))))
     (list (cons "Copy Value" (lambda () (copy-to-clipboard (if (member value '(:null nil)) "" (db-value-text value :max nil))))))
     (and existing
          (list (cons "Filter by This Value"
                      (lambda ()
                        (let ((filter (cond ((eq value :null) "NULL")
                                            ((typep value '(vector (unsigned-byte 8))) (editor-error "Can't filter by a blob"))
                                            (t (format nil "=~a" (db-value-text value :max nil))))))
                          (gtk:editable-set-text (gtk:grid-get-child-at (dbt-grid page) (1+ column) 1) filter)
                          (setf (gethash name (dbt-filters page)) filter)
                          (filter-db-table page))))))
     (and existing (dbt-editable-p page) (gethash row (dbt-deleted page))
          (list (cons "Keep This Row" (lambda () (remhash row (dbt-deleted page)) (fill-db-rows page))))))))

(defun edit-db-cell-dialog (page row column)
  "Edit a cell's value in a dialog: for text of several lines, or long."
  (let* ((value (db-cell-value page row column))
         (dialog (make-instance 'adw:dialog :title (nth column (dbt-column-names page)) :content-width 520 :content-height 360))
         (text (make-instance 'gtk:text-view :monospace t :wrap-mode :word-char :top-margin 8 :left-margin 8 :right-margin 8))
         (set (make-instance 'gtk:button :label "_Set" :use-underline t :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "_Cancel" :use-underline t))
         (header (make-instance 'adw:header-bar :show-end-title-buttons nil :show-start-title-buttons nil))
         (toolbar (make-instance 'adw:toolbar-view)))
    (gtk:text-buffer-set-text (gtk:text-view-get-buffer text)
                              (if (member value '(:null nil)) "" (db-value-text value :max nil)) -1)
    (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
    (gobject:connect set :clicked (lambda (b) (declare (ignore b))
                                    (set-db-cell page row column (text-string (gtk:text-view-get-buffer text)))
                                    (adw:dialog-close dialog)))
    (adw:header-bar-pack-start header cancel)
    (adw:header-bar-pack-end header set)
    (adw:toolbar-view-add-top-bar toolbar header)
    (adw:toolbar-view-set-content toolbar (make-instance 'gtk:scrolled-window :child text :vexpand t))
    (adw:dialog-set-child dialog toolbar)
    (adw:dialog-present dialog (window-gtk-window *window*))
    (gtk:widget-grab-focus text)))

(defun export-db-rows-csv (page)
  "Save the rows shown as a CSV file."
  (let ((chooser (gtk:file-dialog-new))
        (key-count (dbt-key-count page)))
    (gtk:file-dialog-set-initial-name chooser (format nil "~a.csv" (dbt-table page)))
    (when (db-project)
      (gtk:file-dialog-set-initial-folder chooser (gio:file-new-for-path (uiop:native-namestring (db-project)))))
    (gio:async (gtk:file-dialog-save chooser (window-gtk-window *window*))
               (lambda (file)
                 (let ((path (gio:file-get-path file)))
                   (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
                     (write-string (db-result-csv (dbt-column-names page)
                                                  (mapcar (lambda (row) (subseq row key-count)) (dbt-rows page)))
                                   out))
                   (message "Saved ~d row~:p to ~a" (length (dbt-rows page)) path)))
               :error (lambda (e) (declare (ignore e))))))

;;; Structure

(defun structure-grid (headings rows)
  (let ((grid (make-instance 'gtk:grid :css-classes '("cadre-db-grid") :margin-start 8 :halign :start)))
    (loop for heading in headings
          for i from 0
          do (gtk:grid-attach grid (make-instance 'gtk:label :label heading :xalign 0.0 :css-classes '("cadre-db-head"))
                              i 0 1 1))
    (loop for row in rows
          for r from 1
          do (loop for text in row
                   for i from 0
                   do (gtk:grid-attach grid (make-instance 'gtk:label :label (or text "") :xalign 0.0 :selectable t)
                                       i r 1 1)))
    grid))

(defun build-db-structure (page db)
  (let ((box (dbt-structure page))
        (table (dbt-table page)))
    (loop for child = (gtk:widget-get-first-child box) while child do (gtk:box-remove box child))
    (multiple-value-bind (indexes foreign-keys create)
        (sb-thread:with-mutex ((db-lock db))
          (values (db-indexes db table) (db-foreign-keys db table) (db-create-statement db table)))
      (gtk:box-append box (section-heading "COLUMNS"))
      (gtk:box-append box (structure-grid '("Name" "Type" "Not null" "Default" "Key")
                                          (mapcar (lambda (c) (list (getf c :name) (getf c :type)
                                                                    (if (getf c :not-null) "yes" "")
                                                                    (getf c :default)
                                                                    (and (getf c :primary-key) (princ-to-string (getf c :primary-key)))))
                                                  (dbt-columns page))))
      (when indexes
        (gtk:box-append box (section-heading "INDEXES"))
        (gtk:box-append box (structure-grid '("Name" "Columns" "Unique")
                                            (mapcar (lambda (i) (list (getf i :name) (format nil "~{~a~^, ~}" (getf i :columns))
                                                                      (if (getf i :unique) "yes" "")))
                                                    indexes))))
      (when foreign-keys
        (gtk:box-append box (section-heading "FOREIGN KEYS"))
        (gtk:box-append box (structure-grid '("Column" "References")
                                            (mapcar (lambda (f) (list (getf f :column)
                                                                      (format nil "~a~@[ (~a)~]" (getf f :table) (getf f :to))))
                                                    foreign-keys))))
      (when create
        (gtk:box-append box (section-heading "DEFINITION"))
        (gtk:box-append box (make-instance 'gtk:label :label create :xalign 0.0 :selectable t :wrap t
                                                      :margin-start 12 :margin-end 12 :css-classes '("monospace")))))))

;;; Completing SQL: table and column names from the buffer's connection,
;;; when it's open, and keywords

(defparameter *sql-keywords*
  '("SELECT" "FROM" "WHERE" "INSERT" "INTO" "VALUES" "UPDATE" "SET" "DELETE" "JOIN" "LEFT" "RIGHT" "INNER"
    "OUTER" "FULL" "CROSS" "ON" "USING" "GROUP" "BY" "ORDER" "HAVING" "LIMIT" "OFFSET" "AS" "AND" "OR" "NOT"
    "NULL" "IS" "IN" "LIKE" "ILIKE" "BETWEEN" "DISTINCT" "COUNT" "SUM" "AVG" "MIN" "MAX" "COALESCE" "CREATE"
    "TABLE" "VIEW" "INDEX" "UNIQUE" "DROP" "ALTER" "ADD" "COLUMN" "PRIMARY" "KEY" "FOREIGN" "REFERENCES"
    "DEFAULT" "CHECK" "CASE" "WHEN" "THEN" "ELSE" "END" "UNION" "ALL" "EXCEPT" "INTERSECT" "EXISTS" "BEGIN"
    "COMMIT" "ROLLBACK" "RETURNING" "WITH" "ASC" "DESC" "TRUE" "FALSE" "INTEGER" "TEXT" "REAL" "BLOB"
    "VARCHAR" "BOOLEAN" "DATE" "TIMESTAMP" "SERIAL" "EXPLAIN" "ANALYZE" "PRAGMA" "VACUUM")
  "The words SQL completion offers besides the tables and columns.")

(defvar *db-schema-cache* (make-hash-table :test 'equal)
  "Connection id → (tables . table → column names), for completion; cleared when the page refreshes.")

(defun db-schema (connection)
  "CONNECTION's tables and their columns, if it's open (cached)."
  (let ((db (connection-database connection)))
    (when db
      (or (gethash (db-connection-id connection) *db-schema-cache*)
          (setf (gethash (db-connection-id connection) *db-schema-cache*)
                (ignore-errors
                 (sb-thread:with-mutex ((db-lock db))
                   (let ((tables (mapcar (lambda (x) (getf x :name)) (db-tables db))))
                     (cons tables (loop for table in tables
                                        collect (cons table (mapcar (lambda (c) (getf c :name)) (db-columns db table)))))))))))))

(defun sql-completions (prefix buffer)
  "Completions for PREFIX in a SQL BUFFER: after table. that table's columns,
else tables, columns and keywords (in the case PREFIX is typed in)."
  (let* ((connection (find-db-connection (buffer-local buffer :db-connection)))
         (schema (and connection (db-schema connection)))
         (dot (position #\. prefix :from-end t))
         (lower (and (plusp (length prefix)) (lower-case-p (char prefix (if dot (min (1+ dot) (1- (length prefix))) 0))))))
    (if dot
        (let* ((qualifier (subseq prefix 0 dot))
               (columns (cdr (assoc qualifier (cdr schema) :test #'string-equal))))
          (mapcar (lambda (c) (list (format nil "~a.~a" qualifier c) nil nil :column)) columns))
        (let ((seen (make-hash-table :test 'equalp)) (items '()))
          (flet ((add (name kind)
                   (unless (gethash name seen)
                     (setf (gethash name seen) t)
                     (push (list name nil nil kind) items))))
            (dolist (table (car schema)) (add table :table))
            (loop for (nil . columns) in (cdr schema) do (dolist (c columns) (add c :column)))
            (dolist (k *sql-keywords*) (add (if lower (string-downcase k) k) :keyword)))
          (nreverse items)))))

(push (cons 'sql-mode 'sql-completions) *mode-completers*)

;;; SQL consoles and the Results page

(defun sql-console-hint ()
  (if (eq *keybinding-profile* :emacs)
      "-- C-c C-c runs the statement at the cursor (or the selection); C-c C-b runs them all."
      (format nil "-- ~:[Ctrl~;⌘~]+Enter runs the statement at the cursor (or the selection); ~:*~:[Ctrl~;⌘~]+Shift+Enter runs them all."
              (macos-p))))

(defun new-sql-console (connection)
  "A new SQL buffer whose statements run against CONNECTION."
  (let ((buffer (make-buffer :name (format nil "~a.sql" (db-connection-name connection))
                             :text (make-gtk-text) :major-mode 'sql-mode)))
    (setf (buffer-local buffer :db-connection) (db-connection-id connection))
    (buffer-insert buffer (format nil "~a~%~%" (sql-console-hint)) 0)
    (setf (buffer-modified-p buffer) nil)
    (let ((view (show-buffer *window* buffer)))
      (let ((gtk-buffer (view-gtk-buffer view)))
        (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer))))
    buffer))

(defun with-sql-connection (buffer function)
  "Call FUNCTION with the connection BUFFER's SQL runs against: its own, the
only one, or one chosen from a list (which it keeps)."
  (let ((connection (find-db-connection (buffer-local buffer :db-connection))))
    (cond (connection (funcall function connection))
          ((null *db-connections*)
           (editor-error "No database connections: add one on the Databases page"))
          ((null (rest *db-connections*))
           (setf (buffer-local buffer :db-connection) (db-connection-id (first *db-connections*)))
           (funcall function (first *db-connections*)))
          (t (choose-sql-connection buffer function)))))

(defun choose-sql-connection (buffer function)
  (open-picker (window-picker *window*)
               :items *db-connections* :label #'db-connection-name :detail #'db-connection-description
               :placeholder (format nil "The database ~a's SQL runs against" (buffer-name buffer))
               :on-choose (lambda (connection)
                            (setf (buffer-local buffer :db-connection) (db-connection-id connection))
                            (message "~a runs against ~a" (buffer-name buffer) (db-connection-name connection))
                            (when function (funcall function connection)))))

(define-command db-choose-connection ()
  "Choose the database connection this SQL file runs against."
  (:modes sql-mode)
  (when (null *db-connections*)
    (editor-error "No database connections: add one on the Databases page"))
  (choose-sql-connection (view-buffer (current-view)) nil))

(define-command db-run-statement ()
  "Run the SQL statement at the cursor (or the selected SQL) against the file's database; the rows go to the Results page."
  (:modes sql-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (text (buffer-string (view-buffer view))))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (let ((sql (if has
                     (subseq text (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end))
                     (sql-statement-at text (gtk:text-iter-get-offset (cursor-iter gtk-buffer))))))
        (unless (and sql (sql-statement-ranges sql)) (editor-error "No SQL statement here"))
        (with-sql-connection (view-buffer view) (lambda (connection) (run-sql connection sql)))))))

(define-command db-run-all-sql ()
  "Run all the SQL in the file against its database; the last statement's rows go to the Results page."
  (:modes sql-mode)
  (let* ((buffer (view-buffer (current-view)))
         (sql (buffer-string buffer)))
    (unless (sql-statement-ranges sql) (editor-error "There's no SQL to run"))
    (with-sql-connection buffer (lambda (connection) (run-sql connection sql)))))

(defstruct (results-page (:conc-name rp-)) widget label stop copy scroller columns rows running)

(defvar *results-page* nil)

(defun results-page ()
  (or *results-page*
      (let* ((page (make-results-page))
             (label (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :end :selectable t))
             (stop (flat-button "Stop" "Stop the query" (lambda () (let ((db (rp-running page))) (when db (db-interrupt db))))))
             (copy (flat-button "Copy as CSV" "Copy the rows as CSV"
                                (lambda () (copy-to-clipboard (db-result-csv (rp-columns page) (rp-rows page)))
                                  (message "Copied ~d row~:p" (length (rp-rows page))))))
             (scroller (make-instance 'gtk:scrolled-window :vexpand t :hexpand t)))
        (setf (rp-label page) label (rp-stop page) stop (rp-copy page) copy (rp-scroller page) scroller
              (rp-widget page) (gtk:build
                                 (gtk:box :orientation :vertical
                                   (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 2 :margin-bottom 2
                                     label stop copy)
                                   scroller)))
        (gtk:widget-set-visible stop nil)
        (gtk:widget-set-visible copy nil)
        (panel-set-page-child (window-panel *window*) "results" (rp-widget page))
        (setf *results-page* page))))

(defun show-results-page ()
  (panel-show (window-panel *window*) "results")
  (set-panel-visible *window* t))

(defun results-grid (columns rows)
  (let ((grid (make-instance 'gtk:grid :css-classes '("cadre-db-grid") :halign :start :valign :start)))
    (loop for name in columns
          for i from 0
          do (gtk:grid-attach grid (make-instance 'gtk:label :label name :xalign 0.0 :css-classes '("cadre-db-head")) i 0 1 1))
    (loop for row in rows
          for r from 1
          do (loop for value across row
                   for i from 0
                   do (gtk:grid-attach grid (make-instance 'gtk:label :label (db-cell-text value) :xalign 0.0
                                                                      :ellipsize :end :max-width-chars 50 :selectable t
                                                                      :css-classes (if (eq value :null) '("cadre-db-null") '()))
                                       i r 1 1)))
    grid))

(defun run-sql (connection sql)
  "Run SQL against CONNECTION's database in a thread; show what it gives on the Results page."
  (let* ((db (ensure-database connection))
         (page (results-page))
         (limit *db-result-limit*))
    (when (rp-running page) (editor-error "A query is running: Stop it first"))
    (setf (rp-running page) db)
    (gtk:label-set-text (rp-label page) (format nil "Running on ~a…" (db-connection-name connection)))
    (gtk:widget-remove-css-class (rp-label page) "error")
    (gtk:widget-set-visible (rp-stop page) t)
    (show-results-page)
    (sb-thread:make-thread
     (lambda ()
       (let ((result (handler-case (db-run db sql :limit limit)
                       (error (e) e))))
         (glib:call-in-main-thread
          (lambda ()
            (setf (rp-running page) nil)
            (gtk:widget-set-visible (rp-stop page) nil)
            (show-sql-result page connection result limit)))))
     :name "cadre sql")))

(defun show-sql-result (page connection result limit)
  (cond ((typep result 'error)
         (gtk:label-set-text (rp-label page) (if (typep result 'editor-error) (editor-error-message result) (princ-to-string result)))
         (gtk:widget-add-css-class (rp-label page) "error")
         (gtk:widget-set-visible (rp-copy page) nil)
         (gtk:scrolled-window-set-child (rp-scroller page) nil))
        (t
         (let ((columns (db-result-columns result))
               (rows (db-result-rows result)))
           (setf (rp-columns page) columns (rp-rows page) rows)
           (gtk:label-set-text (rp-label page)
                               (if columns
                                   (format nil "~a · ~:d row~:p~@[ (the first ~:d)~] · ~:d ms"
                                           (db-connection-name connection) (length rows) (and (db-result-more result) limit)
                                           (db-result-milliseconds result))
                                   (format nil "~a · ~:d row~:p changed · ~:d ms" (db-connection-name connection)
                                           (db-result-changes result) (db-result-milliseconds result))))
           (gtk:widget-set-visible (rp-copy page) (and columns t))
           (gtk:scrolled-window-set-child (rp-scroller page) (and columns (results-grid columns rows)))
           ;; It may have made or dropped tables, or changed rows a tab shows.
           (unless columns
             (refresh-databases-page)
             (dolist (buffer (db-table-buffers connection))
               (let ((table (buffer-local buffer :db-table)))
                 (unless (dbt-changed-p table)
                   (ignore-errors (load-db-table table :schema t))))))))))
