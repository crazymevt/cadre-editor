;;;; explorer.lisp — the navigation tree of a project's files
;;;;
;;;; A gtk:list-view over a gtk:tree-list-model. Each folder's children come
;;;; from a gtk:directory-list, which loads them asynchronously when the
;;;; folder is first expanded and follows changes on disk. Folders sort
;;;; first, then names, ignoring case. Right-clicking offers opening (and a
;;;; preview for Markdown), new files and folders, renaming, moving to the
;;;; Trash (file-ops.lisp) and copying the path.

(in-package #:cadre-ui)

(defparameter *explorer-attributes*
  "standard::name,standard::display-name,standard::type,standard::is-hidden")

(defun info-file (info)
  (gio:file-info-get-attribute-object info "standard::file"))

(defun info-directory-p (info)
  (eq (gio:file-info-get-file-type info) :directory))

(defun info-shown-p (info)
  (let ((name (gio:file-info-get-name info)))
    (not (or (member name *explorer-hidden-names* :test #'string=)
             (let ((dot (position #\. name :from-end t)))
               (and dot (plusp dot)
                    (member (subseq name (1+ dot)) *explorer-hidden-types*
                            :test #'string-equal)))))))

(defun compare-infos (a b)
  "Folders first, then by name ignoring case. Returns -1, 0 or 1 (a GtkOrdering)."
  (let ((da (info-directory-p a))
        (db (info-directory-p b)))
    (cond ((and da (not db)) -1)
          ((and db (not da)) 1)
          (t (let ((na (gio:file-info-get-display-name a))
                   (nb (gio:file-info-get-display-name b)))
               (cond ((string-lessp na nb) -1)
                     ((string-lessp nb na) 1)
                     (t 0)))))))

(defun directory-model (file)
  "A list model of FILE's children: filtered, sorted, and kept up to date."
  (let ((list (gtk:directory-list-new *explorer-attributes* file)))
    (gtk:directory-list-set-monitored list t)
    (gtk:sort-list-model-new
     (gtk:filter-list-model-new list (gtk:custom-filter-new 'info-shown-p))
     (gtk:custom-sorter-new 'compare-infos))))

(defun make-explorer-row ()
  (let ((icon (make-instance 'gtk:image))
        (label (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t))
        (status (make-instance 'gtk:label :margin-end 6 :css-classes '("caption"))))  ; Git's letter
    (make-instance 'gtk:tree-expander
                   :child (gtk:build (gtk:box :spacing 6 :margin-start 2 icon label status)))))

(defun bind-explorer-row (expander row)
  (gtk:tree-expander-set-list-row expander row)
  (let* ((info (gtk:tree-list-row-get-item row))
         (box (gtk:tree-expander-get-child expander))
         (icon (gtk:widget-get-first-child box))
         (label (gtk:widget-get-next-sibling icon)))
    (gtk:image-set-from-icon-name icon (if (info-directory-p info)
                                           "folder-symbolic"
                                           "text-x-generic-symbolic"))
    (gtk:label-set-text label (gio:file-info-get-display-name info))
    (gtk:widget-set-tooltip-text expander (gio:file-get-path (info-file info)))
    (let ((path (pathname (gio:file-get-path (info-file info)))))
      (decorate-explorer-row expander (if (info-directory-p info) (uiop:ensure-directory-pathname path) path)))))

(defun explorer-path-at (list-view x y)
  "The pathname of the file or folder in LIST-VIEW's row at (X, Y), or nil."
  (loop for w = (gtk:widget-pick list-view x y '(:default)) then (gtk:widget-get-parent w)
        while (and w (not (eq w list-view)))
        when (typep w 'gtk:tree-expander)
          return (let* ((row (gtk:tree-expander-get-list-row w))
                        (info (and row (gtk:tree-list-row-get-item row))))
                   (and info
                        (let ((path (pathname (gio:file-get-path (info-file info)))))
                          (if (info-directory-p info) (uiop:ensure-directory-pathname path) path))))))

(defun show-explorer-menu (list-view path x y &key root)
  "A menu for PATH, at (X, Y) in LIST-VIEW; with PATH nil (the empty space
below the files), a menu for ROOT, the project's folder."
  (let* ((box (make-instance 'gtk:box :orientation :vertical))
         (popover (make-instance 'gtk:popover :child box :has-arrow nil :css-classes '("menu"))))
    (flet ((item (label action)
             (let ((button (make-instance 'gtk:button :label label :css-classes '("flat"))))
               (gtk:widget-set-halign (gtk:button-get-child button) :start)
               (gobject:connect button :clicked (lambda (b) (declare (ignore b))
                                                  (gtk:popover-popdown popover)
                                                  (funcall action)))
               (gtk:box-append box button))))
      (let* ((folder (or (null path) (uiop:directory-pathname-p path)))
             (target (or path root))
             (directory (if folder target (uiop:pathname-directory-pathname target))))
        (unless folder
          (item "Open" (lambda () (open-file-path path)))
          (when (eq (major-mode-for-file path) 'markdown-mode)
            (item "Open Preview" (lambda () (open-markdown-preview path))))
          (gtk:box-append box (make-instance 'gtk:separator)))
        (item "New File…" (lambda () (new-file-in directory)))
        (item "New Folder…" (lambda () (new-folder-in directory)))
        (when path
          (gtk:box-append box (make-instance 'gtk:separator))
          (item "Rename…" (lambda () (rename-in-explorer path)))
          (item "Move to Trash" (lambda () (delete-in-explorer path))))
        (gtk:box-append box (make-instance 'gtk:separator))
        (item "Copy Path" (lambda () (gdk:clipboard-set-text (gtk:widget-get-clipboard list-view)
                                                              (uiop:native-namestring target))))))
    (gtk:widget-set-parent popover list-view)
    (gtk:popover-set-pointing-to popover (gdk:make-rectangle :x (round x) :y (round y) :width 1 :height 1))
    (gobject:connect popover :closed (lambda (p)
                                       (glib:idle-add glib:+priority-default-idle+
                                                      (lambda () (gtk:widget-unparent p) nil))))
    (gtk:popover-popup popover)))

(defun make-explorer (directory &key on-open-file)
  "A widget showing the files under DIRECTORY (a pathname). Activating a
file calls ON-OPEN-FILE with its pathname; activating a folder opens or
closes it."
  (let* ((root (gio:file-new-for-path (namestring directory)))
         (tree (gtk:tree-list-model-new
                (directory-model root) nil nil
                (lambda (info)
                  (and (info-directory-p info) (directory-model (info-file info))))))
         (list-view (gtk:make-list-view tree :setup 'make-explorer-row
                                             :bind 'bind-explorer-row)))
    (gtk:list-view-set-single-click-activate list-view t)
    (let ((click (gtk:gesture-click-new)))
      (gtk:gesture-single-set-button click 3)
      (gobject:connect click :pressed
                       (lambda (gesture n x y)
                         (declare (ignore gesture n))
                         (show-explorer-menu list-view (explorer-path-at list-view x y) x y
                                             :root (uiop:ensure-directory-pathname directory))))
      (gtk:widget-add-controller list-view click))
    (gtk:widget-add-css-class list-view "navigation-sidebar")
    (gobject:connect list-view :activate
                     (lambda (lv position)
                       (declare (ignore lv))
                       (let* ((row (gtk:tree-list-model-get-row tree position))
                              (info (gtk:tree-list-row-get-item row)))
                         (if (info-directory-p info)
                             (gtk:tree-list-row-set-expanded
                              row (not (gtk:tree-list-row-get-expanded row)))
                             (when on-open-file
                               (funcall on-open-file
                                        (pathname (gio:file-get-path (info-file info)))))))))
    (make-instance 'gtk:scrolled-window :child list-view :vexpand t
                                        :hscrollbar-policy :never)))
