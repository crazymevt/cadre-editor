;;;; file-ops.lisp — making, renaming and deleting files from the explorer
;;;;
;;;; Names are asked for in the picker. A new name may include folders
;;;; ("src/util.lisp"), which are made as needed. Renaming a file or folder
;;;; carries its open buffers along (their names, files and modes follow).
;;;; Deleting moves to the Trash, after asking; unmodified buffers of what
;;;; was deleted close, and modified ones stay open so nothing is lost.

(in-package #:cadre-ui)

(defun file-op-error (format &rest args)
  (editor-error "~?" format args))

(defun child-path (directory name)
  "NAME (which may contain /) inside DIRECTORY."
  (let ((name (string-trim " " name)))
    (when (or (string= name "") (search ".." name) (char= (char name 0) #\/))
      (file-op-error "Not a name inside ~a: ~a" (file-display-name* directory) name))
    (if (char= (char name (1- (length name))) #\/)
        (uiop:ensure-directory-pathname (merge-pathnames (string-right-trim "/" name) directory))
        (merge-pathnames (uiop:parse-unix-namestring name) directory))))

(defun file-display-name* (path)
  (if (uiop:directory-pathname-p path)
      (car (last (pathname-directory path)))
      (file-namestring path)))

(defun path-exists-p (path)
  (or (probe-file path) (uiop:directory-exists-p path)
      (uiop:directory-exists-p (uiop:ensure-directory-pathname path))))

(defun new-file-in (directory)
  "Ask for a name, make that file in DIRECTORY, and open it."
  (ask-name (format nil "New file in ~a:" (file-display-name* directory))
            (lambda (name)
              (let ((path (child-path directory name)))
                (when (path-exists-p path) (file-op-error "~a already exists" name))
                (ensure-directories-exist path)
                (unless (uiop:directory-pathname-p path)
                  (with-open-file (out path :direction :output :if-does-not-exist :create :if-exists nil)
                    (declare (ignore out)))
                  (open-file-path path))))))

(defun new-folder-in (directory)
  "Ask for a name and make that folder in DIRECTORY."
  (ask-name (format nil "New folder in ~a:" (file-display-name* directory))
            (lambda (name)
              (let ((path (uiop:ensure-directory-pathname (child-path directory (string-right-trim "/" name)))))
                (when (path-exists-p path) (file-op-error "~a already exists" name))
                (ensure-directories-exist path)
                (message "Made ~a" (file-display-name* path))))))

(defun real-path (path)
  "PATH with links resolved (/var and /private/var are one place on macOS)."
  (or (ignore-errors (truename path)) path))

(defun buffers-under (path)
  "The buffers visiting PATH, or files inside it if it is a folder."
  (let ((real (real-path path)))
    (remove-if-not (lambda (b)
                     (let ((file (and (buffer-file b) (real-path (buffer-file b)))))
                       (and file (if (uiop:directory-pathname-p path)
                                     (uiop:subpathp file real)
                                     (equal (namestring file) (namestring real))))))
                   (buffer-list))))

(defun move-buffer-file (buffer file)
  "BUFFER now visits FILE: rename it, set its mode, and watch the new file."
  (setf (buffer-file buffer) file)
  (let ((name (cadre::file-display-name file)))
    (unless (string= name (buffer-name buffer))
      (setf (buffer-name buffer) (cadre::unique-buffer-name name))))
  (let ((mode (major-mode-for-file file)))
    (unless (eq mode (buffer-major-mode buffer))
      (setf (buffer-major-mode buffer) mode)
      (attach-syntax buffer)))
  (watch-buffer-file buffer)
  (when *window* (update-tab-titles *window* buffer)))

(defun rename-path (path new-path)
  "Rename the file or folder PATH to NEW-PATH, carrying open buffers along."
  (let* ((old-directory (and (uiop:directory-pathname-p path) (real-path path)))
         ;; Where each buffer's file will be, worked out while the old paths exist.
         (moves (loop for buffer in (buffers-under path)
                      collect (cons buffer
                                    (if old-directory
                                        (merge-pathnames (enough-namestring (real-path (buffer-file buffer)) old-directory)
                                                         (uiop:ensure-directory-pathname new-path))
                                        new-path)))))
    (handler-case (gio:file-move (gio:file-new-for-path (string-right-trim "/" (uiop:native-namestring path)))
                                 (gio:file-new-for-path (string-right-trim "/" (uiop:native-namestring new-path)))
                                 '(:none))
      (error (e) (file-op-error "Couldn't rename ~a: ~a" (file-display-name* path) e)))
    (loop for (buffer . file) in moves do (move-buffer-file buffer file))
    (when *window* (save-session *window*))
    (message "Renamed ~a to ~a" (file-display-name* path) (file-display-name* new-path))))

(defun rename-in-explorer (path)
  "Ask for a new name for PATH (a file or folder) and rename it."
  (let ((directory (if (uiop:directory-pathname-p path)
                       (uiop:pathname-parent-directory-pathname path)
                       (uiop:pathname-directory-pathname path)))
        (old (file-display-name* path)))
    (ask-name (format nil "Rename ~a to:" old)
              (lambda (name)
                (let* ((name (string-right-trim "/" (string-trim " " name)))
                       (new (child-path directory name))
                       (new (if (uiop:directory-pathname-p path) (uiop:ensure-directory-pathname new) new)))
                  (cond ((string= name old))
                        ((and (path-exists-p new) (not (string-equal name old)))
                         (file-op-error "~a already exists" name))
                        (t (ensure-directories-exist (uiop:pathname-directory-pathname
                                                      (if (uiop:directory-pathname-p new)
                                                          (uiop:pathname-parent-directory-pathname new)
                                                          new)))
                           (rename-path path new)))))
              :text old)))

(defun delete-in-explorer (path)
  "Ask, then move PATH (a file or folder) to the Trash."
  (let* ((name (file-display-name* path))
         (folder (uiop:directory-pathname-p path))
         (dialog (adw:alert-dialog-new (format nil "Move ~a to the Trash?" name)
                                       (if folder
                                           "The folder and everything in it go to the Trash, where you can restore them."
                                           "You can restore it from the Trash."))))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "trash" "_Move to Trash")
    (adw:alert-dialog-set-response-appearance dialog "trash" :destructive)
    (adw:alert-dialog-set-default-response dialog "cancel")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response)
                 (when (string= response "trash")
                   (trash-path path))))))

(defvar *trash-function*
  (lambda (path) (gio:file-trash (gio:file-new-for-path (string-right-trim "/" (uiop:native-namestring path)))))
  "Moves a path to the Trash (tests replace it).")

(defun trash-path (path)
  "Move PATH to the Trash; close the unmodified buffers of what went."
  (let ((buffers (buffers-under path)))
    (handler-case (funcall *trash-function* path)
      (error (e) (file-op-error "Couldn't move ~a to the Trash: ~a" (file-display-name* path) e)))
    (dolist (buffer buffers)
      (if (buffer-modified-p buffer)
          (message "~a was deleted; its unsaved changes are still open" (buffer-name buffer))
          (progn (dolist (view (buffer-views *window* buffer)) (close-view *window* view))
                 (when (member buffer (buffer-list)) (kill-buffer buffer)))))
    (save-session *window*)
    (message "Moved ~a to the Trash" (file-display-name* path))))
