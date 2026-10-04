;;;; new-project.lisp — the New Lisp Project dialog
;;;;
;;;; Asks for a name, a description, the author, where to put it, whether it
;;;; is a library or an application, the test framework, the license and
;;;; whether to start a Git repository; then writes the project (core
;;;; new-project.lisp), opens it and shows its main file.

(in-package #:cadre-ui)

(defparameter *project-kinds* '((:library . "Library") (:application . "Application (builds a program)")
                                 (:gtk-application . "GTK application (gtk4)")))
(defparameter *project-test-frameworks* '((:parachute . "Parachute") (:fiveam . "FiveAM")))
(defparameter *project-licenses* '(("MIT" . "MIT") ("BSD-2-Clause" . "BSD 2-Clause") (nil . "None")))

(defun default-project-author ()
  (let ((name (ignore-errors (string-trim '(#\Space #\Newline)
                                          (git (user-homedir-pathname) "config" "--get" "user.name")))))
    (if (plusp (length name)) name (or (uiop:getenv "USER") ""))))

(defun default-project-parent ()
  (let ((remembered (setting :new-project-folder)))
    (cond ((and remembered (uiop:directory-exists-p remembered)) (uiop:ensure-directory-pathname remembered))
          ((and *window* (window-project *window*))
           (uiop:pathname-parent-directory-pathname (window-project *window*)))
          (t (user-homedir-pathname)))))

(defun make-new-project (parent name &key description author license kind tests git (open t))
  "Write the project NAME in PARENT, start its repository if GIT, and open it.
Returns the project's folder."
  (let ((directory (create-lisp-project parent name :description description :author author
                                                    :license license :kind kind :tests tests)))
    (setf (setting :new-project-folder) (uiop:native-namestring (uiop:ensure-directory-pathname parent)))
    (when git
      (handler-case (git-ok directory "init" "-q")
        (error (e) (message "The project is made, but git init failed: ~a" e))))
    (when open
      (open-project directory)
      (open-file-path (merge-pathnames "src/main.lisp" directory)))
    (message "Created ~a in ~a" name (uiop:native-namestring parent))
    directory))

(defvar *new-project-dialog* nil)

(defun combo-row (title choices)
  (make-instance 'adw:combo-row :title title :model (gtk:string-list-new (mapcar #'cdr choices))))

(defun combo-choice (row choices)
  (car (nth (adw:combo-row-get-selected row) choices)))

(define-command new-lisp-project ()
  "Make a new Lisp project: its folder, .asd, src and tests folders, main and
test files, Makefile, README, .gitignore and license; then open it."
  (let* ((parent (default-project-parent))
         (dialog (make-instance 'adw:dialog :title "New Lisp Project" :content-width 520))
         (name (make-instance 'adw:entry-row :title "Name"))
         (description (make-instance 'adw:entry-row :title "Description"))
         (author (make-instance 'adw:entry-row :title "Author" :text (default-project-author)))
         (location (make-instance 'adw:action-row :title "Location" :subtitle (uiop:native-namestring parent)))
         (choose (make-instance 'gtk:button :label "Choose…" :valign :center))
         (kind (combo-row "Kind" *project-kinds*))
         (tests (combo-row "Tests" *project-test-frameworks*))
         (license (combo-row "License" *project-licenses*))
         (git (make-instance 'adw:switch-row :title "Start a Git repository" :active (git-available-p)))
         (where (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label" "caption")))
         (create (make-instance 'gtk:button :label "_Create" :use-underline t :sensitive nil
                                            :css-classes '("suggested-action")))
         (cancel (make-instance 'gtk:button :label "_Cancel" :use-underline t)))
    (labels ((text (row) (string-trim " " (gtk:editable-get-text row)))
             (update ()
               (let* ((n (text name))
                      (ok (valid-project-name-p n))
                      (target (merge-pathnames (make-pathname :directory (list :relative (if ok n "…"))) parent)))
                 (gtk:widget-set-sensitive create ok)
                 (if (or ok (string= n ""))
                     (gtk:widget-remove-css-class name "error")
                     (gtk:widget-add-css-class name "error"))
                 (gtk:label-set-text where
                                     (cond ((string= n "") "Name it with lower-case letters, digits and hyphens.")
                                           ((not ok) "Lower-case letters, digits and hyphens, starting with a letter.")
                                           ((uiop:directory-exists-p target)
                                            (format nil "~a already exists: it must be empty." (uiop:native-namestring target)))
                                           (t (format nil "Creates ~a" (uiop:native-namestring target)))))))
             (create-it ()
               (handler-case
                   (progn
                     (make-new-project parent (text name)
                                       :description (text description) :author (text author)
                                       :license (combo-choice license *project-licenses*)
                                       :kind (combo-choice kind *project-kinds*)
                                       :tests (combo-choice tests *project-test-frameworks*)
                                       :git (adw:switch-row-get-active git))
                     (adw:dialog-close dialog))
                 (editor-error (e) (gtk:label-set-text where (editor-error-message e))))))
      (adw:action-row-add-suffix location choose)
      (gobject:connect choose :clicked
                       (lambda (b) (declare (ignore b))
                         (let ((chooser (gtk:file-dialog-new)))
                           (gtk:file-dialog-set-initial-folder chooser (gio:file-new-for-path (uiop:native-namestring parent)))
                           (gio:async (gtk:file-dialog-select-folder chooser (window-gtk-window *window*))
                                      (lambda (file)
                                        (setf parent (uiop:ensure-directory-pathname (gio:file-get-path file)))
                                        (adw:action-row-set-subtitle location (uiop:native-namestring parent))
                                        (update))
                                      :error (lambda (e) (declare (ignore e)))))))
      (gobject:connect name :changed (lambda (e) (declare (ignore e)) (update)))
      (gobject:connect name :entry-activated (lambda (e) (declare (ignore e))
                                               (when (gtk:widget-get-sensitive create) (create-it))))
      (gobject:connect create :clicked (lambda (b) (declare (ignore b)) (create-it)))
      (gobject:connect cancel :clicked (lambda (b) (declare (ignore b)) (adw:dialog-close dialog)))
      (let ((project (make-instance 'adw:preferences-group))
            (setup (make-instance 'adw:preferences-group)))
        (dolist (row (list name description author location)) (adw:preferences-group-add project row))
        (dolist (row (list kind tests license git)) (adw:preferences-group-add setup row))
        (let ((header (make-instance 'adw:header-bar :show-end-title-buttons nil :show-start-title-buttons nil)))
          (adw:header-bar-pack-start header cancel)
          (adw:header-bar-pack-end header create)
          (let ((toolbar (make-instance 'adw:toolbar-view)))
            (adw:toolbar-view-add-top-bar toolbar header)
            (adw:toolbar-view-set-content toolbar
                                          (gtk:build
                                            (gtk:box :orientation :vertical :spacing 18
                                                     :margin-start 18 :margin-end 18 :margin-top 12 :margin-bottom 18
                                              project setup where)))
            (adw:dialog-set-child dialog toolbar))))
      (update)
      (setf *new-project-dialog* (list :dialog dialog :name name :description description :author author
                                       :kind kind :tests tests :license license :git git :create create :where where))
      (adw:dialog-present dialog (window-gtk-window *window*))
      (gtk:widget-grab-focus name))))
