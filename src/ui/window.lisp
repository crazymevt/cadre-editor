;;;; window.lisp — the main window
;;;;
;;;;   header bar
;;;;   activity bar | sidebar ‖ editor area (tabs) ‖ panel
;;;;   status bar
;;;;
;;;; ‖ are gtk:paned dividers. The editor area and the panel share the main
;;;; paned; its orientation is the layout (layout.lisp).

(in-package #:cadre-ui)

(defvar *window* nil
  "The Cadre window. M0 has one.")

(defclass cadre-window ()
  ((window :reader window-gtk-window)
   (project :initform nil :accessor window-project
            :documentation "The open folder, a directory pathname, or nil.")
   (title :reader window-title)
   (side-paned :reader window-side-paned)
   (sidebar :reader window-sidebar)
   (explorer-holder :reader window-explorer-holder)
   (main-paned :reader window-main-paned)
   (editor-stack :reader window-editor-stack :documentation "The empty page, or the tabs.")
   (tab-view :reader window-tab-view)
   (views :initform (make-hash-table :test 'eq) :reader window-views
          :documentation "Tab page child widget → editor-view.")
   (panel :reader window-panel)
   (status-message :reader window-status-message)
   (status-keys :reader window-status-keys)
   (status-position :reader window-status-position)
   (status-mode :reader window-status-mode)
   (layout-button :reader window-layout-button)
   (message-timer :initform nil :accessor window-message-timer)
   (dispatcher :initform (make-key-dispatcher) :reader window-dispatcher)
   (sidebar-toggles :initform '() :accessor window-sidebar-toggles
                    :documentation "Toggle buttons that show the sidebar's state.")
   ;; Layout state (layout.lisp)
   (layout :initform nil :accessor window-layout)
   (wide :initform nil :accessor window-wide-p)
   (panel-sizes :initform (list :horizontal 220 :vertical 560) :accessor window-panel-sizes)))

;;; As the editor's frontend

(defmethod frontend-current-buffer ((window cadre-window))
  (let ((view (selected-view window)))
    (and view (view-buffer view))))

(defmethod frontend-message ((window cadre-window) string)
  (gtk:label-set-text (window-status-message window) string)
  (gtk:widget-set-tooltip-text (window-status-message window) string)
  (panel-log (window-panel window) string)
  (when (window-message-timer window)
    (glib:source-remove (window-message-timer window)))
  (setf (window-message-timer window)
        (glib:timeout-add-seconds glib:+priority-default+ 8
                                  (lambda ()
                                    (setf (window-message-timer window) nil)
                                    (gtk:label-set-text (window-status-message window) "")
                                    nil))))

;;; Building it

(defparameter *css*
  '(("textview.cadre-editor" :font-size "inherit")
    (".cadre-gutter" :opacity "1")
    (".cadre-status" :padding ("2px" "10px") :font-size "smaller")
    (".cadre-status label" :margin ("0" "6px"))
    (".cadre-activity" :padding "4px")
    (".cadre-panel" :background-color "@view_bg_color")))

(defun install-icons ()
  "Add Cadre's own icons to the icon theme."
  (gtk:icon-theme-add-search-path (gtk:icon-theme-get-for-display (gdk:display-get-default))
                                  (namestring (asdf:system-relative-pathname :cadre "icons/"))))

(defun install-css ()
  (gtk:add-css *css*)
  ;; The editor font comes from an option, so it is written separately.
  (let* ((font *editor-font*)
         (space (position #\Space font :from-end t)))
    (gtk:add-css (format nil "textview.cadre-editor, textview.cadre-editor text { font-family: ~a; font-size: ~a; }"
                         (subseq font 0 space) (subseq font (1+ space))))))

(defun app-menu ()
  (let ((menu (gio:menu-new))
        (files (gio:menu-new))
        (view (gio:menu-new))
        (app (gio:menu-new)))
    (flet ((item (section label command)
             (gio:menu-append section label (format nil "app.command('~(~a~)')" command))))
      (item files "New File" 'new-file)
      (item files "Open File…" 'open-file)
      (item files "Open Folder…" 'open-folder)
      (item files "Save" 'save-buffer)
      (item files "Save As…" 'save-buffer-as)
      (item files "Close Tab" 'close-tab)
      (item view "Toggle Sidebar" 'toggle-sidebar)
      (item view "Toggle Panel" 'toggle-panel)
      (item view "Toggle Layout" 'toggle-layout)
      (item view "Automatic Layout" 'use-automatic-layout)
      (item app "Keyboard Shortcuts: Standard" 'use-standard-keys)
      (item app "Keyboard Shortcuts: Emacs" 'use-emacs-keys)
      (item app "Quit" 'quit))
    (gio:menu-append-section menu nil files)
    (gio:menu-append-section menu nil view)
    (gio:menu-append-section menu nil app)
    menu))

(defvar *named-widgets* (make-hash-table :test 'eq)
  "Widgets built outside gtk:build's :id, by name, for the window to find.")

(defun command-button (icon tooltip command &key id label)
  "A button that runs COMMAND through the app.command action, showing ICON
and, if given, LABEL."
  (let ((button (make-instance 'gtk:button :tooltip-text tooltip
                                           :action-name "app.command"
                                           :action-target (glib:variant-new-string
                                                           (string-downcase command)))))
    (if label
        (gtk:button-set-child button (make-instance 'adw:button-content
                                                    :icon-name icon :label label))
        (gtk:button-set-icon-name button icon))
    (when id (setf (gethash id *named-widgets*) button))
    button))

(defun make-empty-page ()
  (gtk:build
    (adw:status-page :icon-name "text-x-generic-symbolic" :title "No file open"
                     :description "Open a file from the explorer, or use the buttons below."
      (gtk:box :spacing 12 :halign :center
        (command-button "document-open-symbolic" "Open a file" 'open-file :label "Open File…")
        (command-button "folder-symbolic" "Open a folder" 'open-folder :label "Open Folder…")
        (command-button "cadre-document-new-symbolic" "New file" 'new-file :label "New File")))))

(defun make-no-folder-page ()
  (gtk:build
    (adw:status-page :icon-name "folder-symbolic" :title "No folder"
                     :css-classes '("compact")
      (gtk:button :label "Open Folder…" :halign :center
                  :css-classes '("pill" "suggested-action")
                  :action-name "app.command"
                  :action-target (glib:variant-new-string "open-folder")))))

(defun make-cadre-window (app)
  (let* ((win (make-instance 'cadre-window))
         (tab-view (make-instance 'adw:tab-view))
         (panel (make-panel :on-hide (lambda () (set-panel-visible win nil))))
         (title (make-instance 'adw:window-title :title "Cadre" :subtitle "")))
    (multiple-value-bind (window ids)
        (gtk:build
          (adw:application-window :application app :title "Cadre"
                                  :default-width 1280 :default-height 820
                                  :width-request 640 :height-request 420
            (adw:toolbar-view
              (adw:header-bar :child-type "top" :title-widget title
                (gtk:toggle-button :id :sidebar-button :icon-name "cadre-sidebar-symbolic"
                                   :tooltip-text "Show or hide the sidebar" :active t
                                   :child-type "start"
                                   :on-clicked (lambda (b) (declare (ignore b))
                                                 (call-command 'toggle-sidebar)))
                (gtk:box :child-type "end" :spacing 6
                  (command-button "document-save-symbolic" "Save" 'save-buffer)
                  (command-button "cadre-layout-horizontal-symbolic"
                                  "Switch between the panel below and beside the editor"
                                  'toggle-layout :id :layout-button)
                  (gtk:menu-button :icon-name "open-menu-symbolic" :menu-model (app-menu)
                                   :tooltip-text "Menu" :primary t)))
              (gtk:box
                (gtk:box :orientation :vertical :css-classes '("cadre-activity")
                  (gtk:toggle-button :id :explorer-button :icon-name "folder-symbolic"
                                     :tooltip-text "Explorer" :active t :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (call-command 'toggle-sidebar))))
                (gtk:separator :orientation :vertical)
                (gtk:paned :id :side-paned :orientation :horizontal :position 260
                           :shrink-start-child nil :resize-start-child nil :hexpand t
                  (gtk:box :id :sidebar :orientation :vertical :width-request 160
                    (gtk:label :label "EXPLORER" :xalign 0.0 :margin-start 12 :margin-top 8
                               :margin-bottom 4 :css-classes '("caption-heading" "dim-label"))
                    (adw:bin :id :explorer-holder :vexpand t))
                  (gtk:paned :id :main-paned :orientation :vertical
                             :shrink-end-child nil :resize-end-child nil
                    (gtk:stack :id :editor-stack :hexpand t :vexpand t)
                    (panel-widget panel))))
              (gtk:box :child-type "bottom" :css-classes '("cadre-status" "toolbar")
                (gtk:label :id :status-message :xalign 0.0 :hexpand t :ellipsize :end)
                (gtk:label :id :status-keys :css-classes '("accent"))
                (gtk:label :id :status-position)
                (gtk:label :id :status-mode)))))
      (flet ((id (key) (gethash key ids)))
        (setf (slot-value win 'window) window
              (slot-value win 'title) title
              (slot-value win 'side-paned) (id :side-paned)
              (slot-value win 'sidebar) (id :sidebar)
              (slot-value win 'explorer-holder) (id :explorer-holder)
              (slot-value win 'main-paned) (id :main-paned)
              (slot-value win 'editor-stack) (id :editor-stack)
              (slot-value win 'tab-view) tab-view
              (slot-value win 'panel) panel
              (slot-value win 'status-message) (id :status-message)
              (slot-value win 'status-keys) (id :status-keys)
              (slot-value win 'status-position) (id :status-position)
              (slot-value win 'status-mode) (id :status-mode)
              (slot-value win 'layout-button) (gethash :layout-button *named-widgets*))
        (setf (window-sidebar-toggles win) (list (id :sidebar-button) (id :explorer-button))))
      (let ((stack (window-editor-stack win)))
        (gtk:stack-add-named stack (make-empty-page) "empty")
        (gtk:stack-add-named stack (gtk:build
                                     (gtk:box :orientation :vertical
                                       (adw:tab-bar :view tab-view :autohide nil)
                                       tab-view))
                             "tabs"))
      (adw:bin-set-child (window-explorer-holder win) (make-no-folder-page))
      (setup-tabs win)
      (setup-layout win)
      (setup-keys win)
      (gobject:connect window :close-request (lambda (w) (declare (ignore w))
                                               (request-close win)))
      win)))

(defun set-window-project (win directory)
  "Show DIRECTORY (a pathname) in WIN's explorer."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (setf (window-project win) directory)
    (adw:bin-set-child (window-explorer-holder win)
                       (make-explorer directory :on-open-file #'open-file-path))
    (let ((name (car (last (pathname-directory directory)))))
      (adw:window-title-set-title (window-title win) name)
      (adw:window-title-set-subtitle (window-title win)
                                     (uiop:native-namestring directory))
      (gtk:window-set-title (window-gtk-window win) (format nil "~a — Cadre" name)))))

(defun update-status (win)
  "Show the current view's position and mode in the status bar."
  (let ((view (selected-view win)))
    (if view
        (multiple-value-bind (line column) (view-cursor-line-column view)
          (gtk:label-set-text (window-status-position win) (format nil "Ln ~d, Col ~d" line column))
          (gtk:label-set-text (window-status-mode win)
                              (major-mode-title (find-major-mode (buffer-major-mode (view-buffer view))))))
        (progn
          (gtk:label-set-text (window-status-position win) "")
          (gtk:label-set-text (window-status-mode win) "")))))
