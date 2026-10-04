;;;; window.lisp — the main window
;;;;
;;;;   header bar
;;;;   activity bar | sidebar ‖ editor area (tabs) ‖ panel
;;;;   status bar
;;;;
;;;; ‖ are gtk:paned dividers. The editor area and the panel share the main
;;;; paned; its orientation is the layout (layout.lisp).

(in-package #:cadre-ui)

(defclass cadre-window ()
  ((window :reader window-gtk-window)
   (project :initform nil :accessor window-project
            :documentation "The open folder, a directory pathname, or nil.")
   (title :reader window-title)
   (side-paned :reader window-side-paned)
   (sidebar :reader window-sidebar)
   (sidebar-stack :reader window-sidebar-stack :documentation "Explorer and Systems.")
   (activity-buttons :initform '() :accessor window-activity-buttons
                     :documentation "(page-name . toggle button) for the activity bar.")
   (explorer-holder :reader window-explorer-holder)
   (main-paned :reader window-main-paned)
   (editor-stack :reader window-editor-stack :documentation "The empty page, or the tabs.")
   (groups :initform '() :accessor window-groups :documentation "The editor groups (tabs.lisp).")
   (active-group :initform nil :accessor window-active-group)
   (groups-holder :reader window-groups-holder)
   (views :initform (make-hash-table :test 'eq) :reader window-views
          :documentation "Tab page child widget → editor-view.")
   (panel :reader window-panel)
   (status-message :reader window-status-message)
   (status-keys :reader window-status-keys)
   (status-macro :reader window-status-macro)
   (status-position :reader window-status-position)
   (status-mode :reader window-status-mode)
   (status-connection :reader window-status-connection)
   (status-arglist :reader window-status-arglist)
   (layout-button :reader window-layout-button)
   (message-timer :initform nil :accessor window-message-timer)
   (dispatcher :initform (make-key-dispatcher) :reader window-dispatcher)
   (picker :initform nil :accessor window-picker-object)
   (find-bar :initform (make-find-bar) :reader window-find-bar)
   (sidebar-toggles :initform '() :accessor window-sidebar-toggles
                    :documentation "Toggle buttons that show the sidebar's state.")
   ;; Layout state (layout.lisp)
   (layout :initform nil :accessor window-layout)
   (wide :initform nil :accessor window-wide-p)
   (panel-sizes :initform (list :horizontal 220 :vertical 560) :accessor window-panel-sizes)))

;;; As the editor's frontend

(defmethod frontend-current-buffer ((window cadre-window))
  "The buffer of the view with the focus (a tab or the REPL), else of the selected tab."
  (let ((view (or (focused-view window) (selected-view window))))
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
    (".cadre-panel" :background-color "@view_bg_color")
    (".cadre-panel-switcher button" :padding ("2px" "10px") :min-width "0")
    (".cadre-review-bar" :background-color "alpha(@accent_bg_color, 0.12)")
    (".cadre-conflict-bar" :background-color "alpha(@warning_bg_color, 0.2)")
    (".cadre-search-match label" :font-weight "normal")
    ;; Editor tabs (tab-strip.lisp)
    (".cadre-tabs" :background-color "alpha(@view_fg_color, 0.04)"
                   :border-bottom "1px solid alpha(@view_fg_color, 0.1)" :padding ("0" "2px"))
    (".cadre-tab-box" :margin-top "3px")
    (".cadre-tab" :padding ("1px" "2px" "1px" "10px") :border-radius ("6px" "6px" "0" "0")
                  :min-height "22px" :font-size "smaller")
    (".cadre-tab:hover" :background-color "alpha(@view_fg_color, 0.06)")
    (".cadre-tab.selected" :background-color "@view_bg_color" :box-shadow "inset 0 2px @accent_color")
    (".cadre-tab-close" :min-height "16px" :min-width "16px" :padding "1px" :margin-left "2px")
    (".cadre-tab:not(.selected):not(:hover) .cadre-tab-close:not(.cadre-tab-pin)" :opacity "0")
    (".cadre-tab-pin" :opacity "0.6")
    (".cadre-tabs > button" :min-height "22px" :min-width "22px" :padding "2px" :margin "2px")
    (".cadre-search-match" :padding ("2px" "4px"))
    (".cadre-git-modified" :color "@warning_color")
    (".cadre-git-added, .cadre-git-untracked, .cadre-git-renamed" :color "@success_color")
    (".cadre-git-deleted, .cadre-git-conflict" :color "@error_color")
    (".cadre-git-folder" :color "alpha(@warning_color, 0.8)")
    (".cadre-sc-button" :min-height "20px" :min-width "20px" :padding ("0" "4px"))
    (".cadre-trace-part" :min-height "18px" :min-width "0" :padding ("0" "2px") :margin ("0" "0" "0" "4px"))
    (".cadre-conflict-actions" :background-color "alpha(@view_bg_color, 0.9)" :border-radius "6px")
    (".cadre-conflict-actions button" :min-height "0" :padding ("0" "6px") :margin "0")
    (".cadre-operation-banner" :background-color "alpha(@warning_bg_color, 0.25)" :border-radius "8px"
                               :padding ("4px" "8px"))
    (".cadre-chat-user" :background-color "alpha(@accent_bg_color, 0.15)" :border-radius "8px"
                        :padding ("6px" "10px"))
    (".cadre-chat-code" :background-color "alpha(@view_fg_color, 0.06)" :border-radius "6px"
                        :padding "6px")
    (".cadre-chat-approval" :background-color "alpha(@warning_bg_color, 0.25)" :border-radius "8px"
                            :padding "8px")
    (".cadre-chat-tool" :opacity "0.8")
    (".cadre-chat-plan" :background-color "alpha(@accent_bg_color, 0.08)" :border-radius "8px" :padding "8px")
    (".cadre-inline-result" :background-color "alpha(@accent_bg_color, 0.18)" :border-radius "4px"
                            :padding ("0" "6px") :font-family "monospace")))

(defun install-icons ()
  "Add Cadre's own icons to the icon theme."
  (gtk:icon-theme-add-search-path (gtk:icon-theme-get-for-display (gdk:display-get-default))
                                  (namestring (asdf:system-relative-pathname :cadre "icons/"))))

(defvar *font-provider* nil)

(defun install-font-css ()
  "Use *editor-font* in editors (again, after it changes)."
  (when *font-provider*
    (gtk:style-context-remove-provider-for-display (gdk:display-get-default) *font-provider*))
  (let* ((font *editor-font*)
         ;; "Iosevka 13pt": a family, then a size, which zooming adds to.
         (family (if (font-size-points font) (subseq font 0 (position #\Space font :from-end t)) font))
         (size (zoomed-font-size)))
    (setf *font-provider*
          (gtk:add-css (format nil "textview.cadre-editor, textview.cadre-editor text, .cadre-gutter { font-family: ~a;~@[ font-size: ~a;~] }"
                               family size)))))

(defun install-css ()
  (gtk:add-css *css*)
  (install-font-css))

(defun edit-menu ()
  (let ((edit (gio:menu-new)))
    (flet ((item (label command)
             (gio:menu-append edit label (format nil "app.command('~(~a~)')" command))))
      (item "Find…" 'find-text)
      (item "Find and Replace…" 'find-replace)
      (item "Find in Project…" 'find-in-project)
      (item "Replace in Project…" 'replace-in-project)
      (item "Rename Symbol…" 'rename-symbol))
    edit))

(defun app-menu ()
  (let ((menu (gio:menu-new))
        (files (gio:menu-new))
        (view (gio:menu-new))
        (lisp (gio:menu-new))
        (app (gio:menu-new)))
    (flet ((item (section label command)
             (gio:menu-append section label (format nil "app.command('~(~a~)')" command))))
      (item files "New File" 'new-file)
      (item files "Open File…" 'open-file)
      (item files "Open Folder…" 'open-folder)
      (item files "New Lisp Project…" 'new-lisp-project)
      (item files "Open Recent Folder…" 'open-recent-project)
      (item files "Open Recent File…" 'open-recent-file)
      (item files "Save" 'save-buffer)
      (item files "Save As…" 'save-buffer-as)
      (item files "Close Tab" 'close-tab)
      (item view "Toggle Sidebar" 'toggle-sidebar)
      (item view "Toggle Panel" 'toggle-panel)
      (item view "Toggle Layout" 'toggle-layout)
      (item view "Zoom In" 'zoom-in)
      (item view "Zoom Out" 'zoom-out)
      (item view "Word Wrap" 'toggle-word-wrap)
      (item view "Fold All" 'fold-all)
      (item view "Unfold All" 'unfold-all)
      (item view "Automatic Layout" 'use-automatic-layout)
      (item view "ASDF Systems" 'show-systems)
      (item lisp "Load Project" 'load-project)
      (item lisp "Load System…" 'load-system)
      (item lisp "Build Project" 'build-project)
      (item lisp "Run Tests" 'run-tests)
      (item lisp "Run Tests in a New Lisp" 'run-tests-in-new-lisp)
      (item lisp "Run GTK App" 'run-gtk-app)
      (item lisp "Stop GTK App" 'stop-gtk-app)
      (item lisp "Inspect…" 'inspect-value)
      (item lisp "Find References" 'find-references)
      (item lisp "Macroexpand" 'expand-macro-once)
      (item lisp "Trace Function…" 'trace-function)
      (item lisp "Show Traces" 'show-traces)
      (item lisp "Restart Lisp" 'restart-lisp)
      (item lisp "Chat with Claude" 'claude)
      (item app "Settings…" 'settings)
      (item app "Color Theme…" 'choose-theme)
      (item app "Keyboard Shortcuts: Standard" 'use-standard-keys)
      (item app "Keyboard Shortcuts: Emacs" 'use-emacs-keys)
      (item app "Quit" 'quit))
    (gio:menu-append-section menu nil files)
    (gio:menu-append-section menu nil (edit-menu))
    (gio:menu-append-section menu nil view)
    (gio:menu-append-section menu nil lisp)
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
         (groups-holder (make-instance 'adw:bin :hexpand t :vexpand t))
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
                (gtk:box :orientation :vertical :spacing 4 :css-classes '("cadre-activity")
                  (gtk:toggle-button :id :explorer-button :icon-name "folder-symbolic"
                                     :tooltip-text "Explorer" :active t :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (show-sidebar-page win "explorer")))
                  (gtk:toggle-button :id :search-button :icon-name "cadre-search-symbolic"
                                     :tooltip-text "Search the project" :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (show-sidebar-page win "search")))
                  (gtk:toggle-button :id :git-button :icon-name "cadre-git-symbolic"
                                     :tooltip-text "Source Control" :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (show-sidebar-page win "git")))
                  (gtk:toggle-button :id :outline-button :icon-name "cadre-outline-symbolic"
                                     :tooltip-text "Outline" :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (show-sidebar-page win "outline")))
                  (gtk:toggle-button :id :systems-button :icon-name "cadre-system-symbolic"
                                     :tooltip-text "ASDF Systems" :css-classes '("flat")
                                     :on-clicked (lambda (b) (declare (ignore b))
                                                   (show-sidebar-page win "systems"))))
                (gtk:separator :orientation :vertical)
                (gtk:paned :id :side-paned :orientation :horizontal :position 260
                           :shrink-start-child nil :resize-start-child nil
                           :shrink-end-child nil :hexpand t
                  (gtk:box :id :sidebar :orientation :vertical :width-request 160
                    (gtk:stack :id :sidebar-stack :vexpand t :hhomogeneous nil :vhomogeneous nil
                               :transition-type :crossfade))
                  ;; Neither child may shrink below its minimum size, so the
                  ;; divider stops there instead of clipping the editor.
                  (gtk:paned :id :main-paned :orientation :vertical
                             :shrink-start-child nil :shrink-end-child nil :resize-end-child nil
                    (gtk:stack :id :editor-stack :hexpand t :vexpand t
                               :hhomogeneous nil :vhomogeneous nil)
                    (panel-widget panel))))
              (gtk:box :child-type "bottom" :css-classes '("cadre-status" "toolbar")
                (gtk:button :id :status-connection :label "○ No Lisp" :css-classes '("flat")
                            :action-name "app.command"
                            :action-target (glib:variant-new-string "show-repl"))
                (gtk:button :id :status-branch :visible nil :css-classes '("flat")
                            :tooltip-text "Source Control" :action-name "app.command"
                            :action-target (glib:variant-new-string "show-source-control")
                  (gtk:box :spacing 4
                    (gtk:image :icon-name "cadre-git-symbolic")
                    (gtk:label :id :status-branch-label)))
                (gtk:label :id :status-arglist :xalign 0.0 :ellipsize :end :max-width-chars 90
                           :css-classes '("monospace"))
                (gtk:label :id :status-message :xalign 1.0 :hexpand t :ellipsize :end)
                (gtk:label :id :status-macro :label "● Recording macro" :visible nil
                           :css-classes '("error") :tooltip-text "Defining a keyboard macro: C-x ) ends it")
                (gtk:label :id :status-keys :css-classes '("accent"))
                (gtk:label :id :status-position)
                (gtk:label :id :status-mode)))))
      (flet ((id (key) (gethash key ids)))
        (setf (slot-value win 'window) window
              (slot-value win 'title) title
              (slot-value win 'side-paned) (id :side-paned)
              (slot-value win 'sidebar) (id :sidebar)
              (slot-value win 'sidebar-stack) (id :sidebar-stack)
              (slot-value win 'explorer-holder) (make-instance 'adw:bin :vexpand t)
              (slot-value win 'main-paned) (id :main-paned)
              (slot-value win 'editor-stack) (id :editor-stack)
              (slot-value win 'groups-holder) groups-holder
              (slot-value win 'panel) panel
              (slot-value win 'status-message) (id :status-message)
              (slot-value win 'status-keys) (id :status-keys)
              (slot-value win 'status-macro) (id :status-macro)
              (slot-value win 'status-position) (id :status-position)
              (slot-value win 'status-mode) (id :status-mode)
              (slot-value win 'status-connection) (id :status-connection)
              (slot-value win 'status-arglist) (id :status-arglist)
              (slot-value win 'layout-button) (gethash :layout-button *named-widgets*))
        (setf (gethash :status-branch *named-widgets*) (id :status-branch)
              (gethash :status-branch-label *named-widgets*) (id :status-branch-label))
        (setf (window-sidebar-toggles win) (list (id :sidebar-button))
              (window-activity-buttons win) (list (cons "explorer" (id :explorer-button))
                                                  (cons "search" (id :search-button))
                                                  (cons "outline" (id :outline-button))
                                                  (cons "git" (id :git-button))
                                                  (cons "systems" (id :systems-button)))))
      (let ((stack (window-sidebar-stack win)))
        (gtk:stack-add-named stack (gtk:build
                                     (gtk:box :orientation :vertical
                                       (gtk:label :label "EXPLORER" :xalign 0.0 :margin-start 12 :margin-top 8
                                                  :margin-bottom 4 :css-classes '("caption-heading" "dim-label"))
                                       (window-explorer-holder win)))
                             "explorer")
        (gtk:stack-add-named stack (make-project-search-widget) "search")
        (gtk:stack-add-named stack (make-outline-widget) "outline")
        (gtk:stack-add-named stack (make-source-control-widget) "git")
        (gtk:stack-add-named stack (make-systems-widget) "systems")
        (gtk:stack-set-visible-child-name stack "explorer"))
      (let ((stack (window-editor-stack win)))
        (gtk:stack-add-named stack (make-empty-page) "empty")
        (gtk:stack-add-named stack (gtk:build
                                     (gtk:box :orientation :vertical
                                       (make-review-bar)
                                       (make-conflict-bar)
                                       (find-bar-widget (window-find-bar win))
                                       groups-holder))
                             "tabs"))
      (adw:bin-set-child (window-explorer-holder win) (make-no-folder-page))
      (setup-tabs win)
      (setup-layout win)
      (setup-keys win)
      ;; Coming back to Cadre: files may have changed in Git meanwhile.
      (gobject:connect window "notify::is-active"
                       (lambda (w pspec) (declare (ignore pspec))
                         (when (gtk:window-is-active w) (git-changed))))
      (gobject:connect window :close-request (lambda (w) (declare (ignore w))
                                               (request-close win)))
      win)))

(defun set-window-project (win directory)
  "Show DIRECTORY (a pathname) in WIN's explorer."
  (let ((directory (uiop:ensure-directory-pathname directory)))
    (remember-recent-project directory)
    (setf (window-project win) directory)
    ;; After the project changes, so Git looks at the new one.
    (git-project-opened)
    (adw:bin-set-child (window-explorer-holder win)
                       (make-explorer directory :on-open-file #'open-file-path))
    (refresh-systems)
    (let ((name (car (last (pathname-directory directory)))))
      (adw:window-title-set-title (window-title win) name)
      (adw:window-title-set-subtitle (window-title win)
                                     (uiop:native-namestring directory))
      (gtk:window-set-title (window-gtk-window win) (format nil "~a — Cadre" name)))))

(defun update-status (win)
  "Show the current view's position and mode in the status bar."
  (refresh-outline)
  (let ((view (selected-view win)))
    (if view
        (multiple-value-bind (line column) (view-cursor-line-column view)
          (gtk:label-set-text (window-status-position win) (format nil "Ln ~d, Col ~d" line column))
          (gtk:label-set-text (window-status-mode win)
                              (major-mode-title (find-major-mode (buffer-major-mode (view-buffer view))))))
        (progn
          (gtk:label-set-text (window-status-position win) "")
          (gtk:label-set-text (window-status-mode win) "")))))
