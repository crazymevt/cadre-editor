;;;; libraries.lisp — the Libraries tab: a project's dependencies, and
;;;; Quicklisp's systems to add to them
;;;;
;;;; The tab shows what a system of the project depends on (its .asd's
;;;; :depends-on), with Remove, and searches the systems Quicklisp offers.
;;;; Add puts one in the .asd (as text, keeping its layout; through the
;;;; .asd's tab if it is open) and loads it with Quicklisp in the REPL,
;;;; which downloads it first if it must.

(in-package #:cadre-ui)

(define-major-mode libraries-mode (:title "Libraries")
  "A project's libraries, in a tab.")

(defstruct (libraries-page (:conc-name lp-))
  root                                  ; the project folder
  entries                               ; (name asd-file) of its systems
  (system nil)                          ; the one shown
  (libraries nil)                       ; LIBRARYs from Quicklisp, :no-quicklisp, or nil before they come
  dependencies results search status heading)

(defun libraries-buffer (root)
  (find-if (lambda (b) (let ((page (buffer-local b :libraries)))
                         (and page (equal (namestring (lp-root page)) (namestring root)))))
           (buffer-list)))

(defun page-asd-file (page)
  (second (assoc (lp-system page) (lp-entries page) :test #'string=)))

(defun asd-current-text (file)
  "FILE's text: its tab's, if it is open (with any unsaved changes), else the file's."
  (let ((buffer (find-file-buffer file)))
    (if buffer (buffer-string buffer) (uiop:read-file-string file))))

(defun change-asd (file edit-function)
  "Apply the edit EDIT-FUNCTION makes of FILE's text (nil: no change): in
its tab if it is open, saving it, else in the file. True if it changed."
  (let* ((buffer (find-file-buffer file))
         (text (asd-current-text file))
         (edit (funcall edit-function text)))
    (when edit
      (if buffer
          (destructuring-bind (start end new) edit
            (replace-text-between (buffer-text buffer) start end new)
            (save-one *window* buffer (lambda (ok) (declare (ignore ok)))))
          (with-open-file (out file :direction :output :if-exists :supersede :external-format :utf-8)
            (write-string (apply-edit text edit) out)))
      t)))

(defun page-dependencies (page)
  (handler-case (asd-dependency-names (asd-current-text (page-asd-file page)) (lp-system page))
    (error () '())))

(defun find-library (page name)
  (and (listp (lp-libraries page))
       (find name (lp-libraries page) :key #'library-name :test #'string-equal)))

;;; The widget

(defun make-libraries-widget (page)
  (let* ((systems (gtk:drop-down-new-from-strings (mapcar #'first (lp-entries page))))
         (search (make-instance 'gtk:search-entry :placeholder-text "Search Quicklisp" :hexpand t))
         (dependencies (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")))
         (results (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label")))
         (heading (make-instance 'gtk:label :xalign 0.0 :css-classes '("heading"))))
    (setf (lp-dependencies page) dependencies (lp-results page) results (lp-search page) search
          (lp-status page) status (lp-heading page) heading)
    (gtk:drop-down-set-selected systems (or (position (lp-system page) (lp-entries page) :key #'first :test #'string=) 0))
    (gobject:connect systems "notify::selected"
                     (lambda (d p) (declare (ignore p))
                       (setf (lp-system page) (first (nth (gtk:drop-down-get-selected d) (lp-entries page))))
                       (fill-libraries-page page)))
    (gobject:connect search :search-changed (lambda (e) (declare (ignore e)) (fill-library-results page)))
    (fill-libraries-page page)
    (fetch-libraries page)
    (let ((content (gtk:build
                     (gtk:box :orientation :vertical :spacing 8
                       (gtk:box :spacing 8
                         (gtk:label :label "System" :css-classes '("dim-label"))
                         systems
                         heading)
                       dependencies
                       (gtk:label :label "ADD A LIBRARY" :xalign 0.0 :margin-top 12 :css-classes '("caption-heading" "dim-label"))
                       search
                       status
                       results))))
      (make-instance 'gtk:scrolled-window :hscrollbar-policy :never :vexpand t
                                          :child (make-instance 'adw:clamp :maximum-size 800 :child content
                                                                           :margin-start 12 :margin-end 12
                                                                           :margin-top 12 :margin-bottom 12)))))

(defun library-button (label tooltip action &key (sensitive t))
  (let ((button (make-instance 'gtk:button :label label :valign :center :tooltip-text tooltip :sensitive sensitive)))
    (gobject:connect button :clicked (lambda (b) (declare (ignore b)) (funcall action)))
    button))

(defun fill-libraries-page (page)
  (let ((list (lp-dependencies page))
        (names (page-dependencies page)))
    (gtk:label-set-text (lp-heading page) (format nil "in ~a" (file-namestring (page-asd-file page))))
    (gtk:list-box-remove-all list)
    (if (null names)
        (gtk:list-box-append list (make-instance 'adw:action-row :title "No dependencies"
                                                                 :subtitle "Search below to add one"))
        (dolist (name names)
          (let* ((library (find-library page name))
                 (row (make-instance 'adw:action-row :title name
                                                     :subtitle (cond ((null library) "")
                                                                     ((library-installed library) "Installed")
                                                                     (t "Not downloaded yet")))))
            (adw:action-row-add-suffix row (library-button "Remove" (format nil "Take ~a out of ~a's :depends-on" name (lp-system page))
                                                           (lambda () (remove-library page name))))
            (gtk:list-box-append list row))))
    (fill-library-results page)))

(defun fill-library-results (page)
  (let ((list (lp-results page))
        (libraries (lp-libraries page)))
    (gtk:list-box-remove-all list)
    (gtk:widget-set-visible list nil)
    (cond ((null libraries) (gtk:label-set-text (lp-status page) "Asking Quicklisp for its systems…"))
          ((eq libraries :no-quicklisp)
           (gtk:label-set-text (lp-status page)
                               "Quicklisp isn't loaded in this Lisp: install it (quicklisp.org) and load its setup.lisp in your init file to search and add libraries."))
          (t
           (let* ((query (gtk:editable-get-text (lp-search page)))
                  (found (and (plusp (length (string-trim " " query))) (search-libraries libraries query :limit 100)))
                  (dependencies (page-dependencies page)))
             (gtk:label-set-text (lp-status page)
                                 (cond ((zerop (length (string-trim " " query)))
                                        (format nil "~:d systems in Quicklisp" (length libraries)))
                                       ((null found) "Nothing in Quicklisp matches")
                                       (t "")))
             (when found
               (gtk:widget-set-visible list t)
               (dolist (library found)
                 (let* ((name (library-name library))
                        (added (member name dependencies :test #'string-equal))
                        (row (make-instance 'adw:action-row
                                            :title name
                                            :subtitle (format nil "~:[~;~:*from ~a · ~]~:[not downloaded~;installed~]"
                                                              (and (string/= (library-project library) name) (library-project library))
                                                              (library-installed library)))))
                   (adw:action-row-add-suffix
                    row (library-button "Load" (format nil "Load ~a into the Lisp (Quicklisp downloads it if needed), without adding it" name)
                                        (lambda () (load-library page name))))
                   (adw:action-row-add-suffix
                    row (library-button (if added "Added" "Add")
                                        (format nil "Add ~a to ~a's :depends-on and load it" name (lp-system page))
                                        (lambda () (add-library page name))
                                        :sensitive (not added)))
                   (gtk:list-box-append list row)))))))))

;;; Asking the Lisp

(defun fetch-libraries (page)
  (with-connection (connection)
    (rex connection (list (remote-symbol "cl:eval")
                          (list (remote-symbol "cl:read-from-string") *quicklisp-systems-form*))
         :package "COMMON-LISP-USER"
         :on-ok (lambda (data)
                  (setf (lp-libraries page) (parse-quicklisp-systems data))
                  (fill-libraries-page page))
         :on-abort (lambda (reason)
                     (setf (lp-libraries page) :no-quicklisp)
                     (fill-libraries-page page)
                     (message "Could not ask Quicklisp: ~a" reason)))))

(defun load-library (page name &optional then)
  (run-in-repl (format nil "Loading ~a with Quicklisp" name)
               (load-system-form nil name)
               :on-done (lambda ()
                          (fetch-libraries page)
                          (when then (funcall then)))))

(defun add-library (page name)
  "Add NAME to the system's :depends-on, then load it."
  (let ((system (lp-system page)))
    (change-asd (page-asd-file page) (lambda (text) (asd-add-dependency text system name)))
    (message "Added ~a to ~a's :depends-on" name system)
    (fill-libraries-page page)
    (load-library page name)))

(defun remove-library (page name)
  (let ((system (lp-system page)))
    (when (change-asd (page-asd-file page) (lambda (text) (asd-remove-dependency text system name)))
      (message "Took ~a out of ~a's :depends-on (it stays loaded until the Lisp restarts)" name system))
    (fill-libraries-page page)))

;;; Commands

(defun show-libraries (root &optional system)
  (let* ((entries (or (project-system-entries root)
                      (editor-error "No .asd in ~a" (uiop:native-namestring root))))
         (buffer (libraries-buffer root)))
    (if buffer
        (let ((page (buffer-local buffer :libraries)))
          (when (and system (assoc system (lp-entries page) :test #'string=) (string/= system (lp-system page)))
            (setf (lp-system page) system)
            (fill-libraries-page page)))
        (let ((page (make-libraries-page :root root :entries entries
                                         :system (if (assoc system entries :test #'string=) system (first (first entries))))))
          (setf buffer (make-buffer :name (format nil "Libraries — ~a"
                                                  (car (last (pathname-directory (uiop:ensure-directory-pathname root)))))
                                    :text (make-gtk-text) :major-mode 'libraries-mode)
                (buffer-local buffer :libraries) page
                (buffer-local buffer :tab-widget) (lambda (view) (declare (ignore view)) (make-libraries-widget page)))))
    (show-buffer *window* buffer)))

(define-command manage-libraries ()
  "Show the project's libraries: what its systems depend on, and Quicklisp's
systems to add."
  (show-libraries (project-root-or-error)))
