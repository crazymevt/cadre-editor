;;;; layout.lisp — the horizontal and vertical layouts (design doc 6.1)
;;;;
;;;; The editor area and the panel are the two children of the main paned.
;;;; The horizontal layout stacks them (a :vertical paned); the vertical
;;;; layout puts them side by side (a :horizontal paned). Switching only
;;;; changes the orientation, so nothing is rebuilt. Each layout remembers
;;;; its own panel size.

(in-package #:cadre-ui)

(defun effective-layout (win)
  (if (eq *layout* :auto)
      (if (window-wide-p win) :vertical :horizontal)
      *layout*))

(defun paned-extent (win layout)
  (let ((paned (window-main-paned win)))
    (if (eq layout :vertical) (gtk:widget-get-width paned) (gtk:widget-get-height paned))))

(defun panel-visible-p (win)
  (gtk:widget-get-visible (panel-widget (window-panel win))))

(defun remember-panel-size (win)
  (let* ((layout (window-layout win))
         (extent (and layout (paned-extent win layout))))
    (when (and extent (plusp extent) (panel-visible-p win))
      (setf (getf (window-panel-sizes win) layout)
            (max 80 (- extent (gtk:paned-get-position (window-main-paned win))))))))

(defun restore-panel-size (win &optional (tries 40))
  "Set the divider so the panel has its remembered size. Before the window
has a size, try again shortly."
  (let* ((layout (window-layout win))
         (extent (paned-extent win layout)))
    (if (plusp extent)
        (gtk:paned-set-position (window-main-paned win)
                                (max 100 (- extent (getf (window-panel-sizes win) layout))))
        (when (plusp tries)
          (glib:timeout-add glib:+priority-default+ 25
                            (lambda () (restore-panel-size win (1- tries)) nil))))))

(defun apply-layout (win &optional (layout (effective-layout win)))
  "Show WIN in LAYOUT, :horizontal or :vertical."
  (unless (eq layout (window-layout win))
    (remember-panel-size win)
    (gtk:orientable-set-orientation (window-main-paned win)
                                    (if (eq layout :vertical) :horizontal :vertical))
    (setf (window-layout win) layout)
    (gtk:button-set-icon-name (window-layout-button win)
                              (if (eq layout :vertical)
                                  "cadre-layout-vertical-symbolic"
                                  "cadre-layout-horizontal-symbolic"))
    (restore-panel-size win)))

(defun setup-layout (win)
  ;; :auto follows a breakpoint on the window's width.
  (let ((breakpoint (adw:breakpoint-new
                     (adw:breakpoint-condition-parse
                      (format nil "min-width: ~dpx" *auto-vertical-min-width*)))))
    (gobject:connect breakpoint :apply (lambda (b) (declare (ignore b))
                                         (setf (window-wide-p win) t)
                                         (apply-layout win)))
    (gobject:connect breakpoint :unapply (lambda (b) (declare (ignore b))
                                           (setf (window-wide-p win) nil)
                                           (apply-layout win)))
    (adw:application-window-add-breakpoint (window-gtk-window win) breakpoint))
  (apply-layout win))

(defun set-panel-visible (win visible)
  (unless visible (remember-panel-size win))
  (gtk:widget-set-visible (panel-widget (window-panel win)) visible)
  (when visible (restore-panel-size win)))

(defun sync-toggle (toggle active)
  (unless (eq (gtk:toggle-button-get-active toggle) active)
    (gtk:toggle-button-set-active toggle active)))

(defun sidebar-page (win)
  (gtk:stack-get-visible-child-name (window-sidebar-stack win)))

(defun set-sidebar-visible (win visible)
  (gtk:widget-set-visible (window-sidebar win) visible)
  (dolist (toggle (window-sidebar-toggles win))
    (sync-toggle toggle visible))
  (loop for (name . toggle) in (window-activity-buttons win)
        do (sync-toggle toggle (and visible (string= name (sidebar-page win))))))

(defun show-sidebar-page (win name &key (toggle t))
  "Show the sidebar's NAME page. With TOGGLE, if it is already showing, hide the sidebar."
  (let ((visible (gtk:widget-get-visible (window-sidebar win))))
    (if (and toggle visible (string= name (sidebar-page win)))
        (set-sidebar-visible win nil)
        (progn
          (gtk:stack-set-visible-child-name (window-sidebar-stack win) name)
          (set-sidebar-visible win t)
          (when (string= name "systems") (refresh-systems))
          (when (string= name "outline") (refresh-outline :force t))
          (when (string= name "git") (git-changed) (refresh-source-control))))))
