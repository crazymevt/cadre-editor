;;;; tabs.lisp — tabs in editor groups, and splitting the editor area
;;;;
;;;; The editor area holds one or more editor groups. Each group is an
;;;; adw:tab-view of editor views with its own tab bar. Splitting a group
;;;; puts it and a new group in a gtk:paned where it was, so the groups form
;;;; a tree of paned widgets inside the window's groups holder. One group is
;;;; active: the one whose view last had the focus. Commands act on it, and
;;;; files open in it. Tabs can be dragged between groups.
;;;;
;;;; A buffer can show in several groups at once, each in its own view.
;;;; Closing a buffer's last view kills the buffer, so a group being closed
;;;; first hands its tabs to a neighbour.

(in-package #:cadre-ui)

(defclass editor-group ()
  ((tab-view :initform (make-instance 'adw:tab-view) :reader group-tab-view)
   (strip :accessor group-strip :documentation "The group's tabs (tab-strip.lisp).")
   (widget :reader group-widget)))

(defmethod print-object ((group editor-group) stream)
  (print-unreadable-object (group stream :type t :identity t)))

(defun make-editor-group (win)
  (let* ((group (make-instance 'editor-group))
         (tab-view (group-tab-view group))
         (strip (make-tab-strip win group)))
    (setf (group-strip group) strip
          (slot-value group 'widget)
          (gtk:build
            (gtk:box :orientation :vertical :hexpand t :vexpand t :width-request 160 :height-request 100
              (strip-widget strip)
              tab-view)))
    (setup-group-signals win group)
    group))

(defun window-tab-view (win)
  "The active group's adw:tab-view."
  (group-tab-view (window-active-group win)))

(defun group-pages (group)
  (let ((tabs (group-tab-view group)))
    (loop for i below (adw:tab-view-get-n-pages tabs)
          collect (adw:tab-view-get-nth-page tabs i))))

(defun window-pages (win)
  "The pages of the active group."
  (group-pages (window-active-group win)))

(defun all-pages (win)
  (loop for group in (window-groups win) append (group-pages group)))

(defun group-selected-view (win group)
  (let ((page (adw:tab-view-get-selected-page (group-tab-view group))))
    (and page (gethash (adw:tab-page-get-child page) (window-views win)))))

(defun selected-view (win)
  "The view in the active group's selected tab, or nil."
  (group-selected-view win (window-active-group win)))

(defun page-view (win page)
  (gethash (adw:tab-page-get-child page) (window-views win)))

(defun view-page (win view)
  (declare (ignore win))
  (adw:tab-view-get-page (group-tab-view (view-group view)) (view-widget view)))

(defun buffer-views (win buffer)
  "The views in WIN showing BUFFER."
  (loop for view being the hash-values of (window-views win)
        when (eq (view-buffer view) buffer) collect view))

(defun tab-title (buffer)
  (format nil "~a~:[~; ●~]" (buffer-display-name buffer) (buffer-needs-saving-p buffer)))

(defun update-tab-titles (win buffer)
  (dolist (view (buffer-views win buffer))
    (let ((page (view-page win view)))
      (when page
        (adw:tab-page-set-title page (tab-title buffer))
        (adw:tab-page-set-tooltip page (if (buffer-file buffer)
                                           (uiop:native-namestring (buffer-file buffer))
                                           (buffer-name buffer)))))))

(defun show-empty-or-tabs (win)
  (gtk:stack-set-visible-child-name (window-editor-stack win)
                                    (if (all-pages win) "tabs" "empty")))

(defun activate-group (win group)
  (unless (eq group (window-active-group win))
    (setf (window-active-group win) group)
    (update-status win)
    (when (find-bar-open-p (window-find-bar win))
      (find-update (window-find-bar win)))))

(defun show-buffer (win buffer &key (focus t) group)
  "Select a tab showing BUFFER in GROUP (default: the active group), adding
one if it has none. Returns the view."
  (let* ((group (or group (window-active-group win)))
         (view (or (find group (buffer-views win buffer) :key #'view-group)
                   (add-view win buffer group))))
    (activate-group win group)
    (adw:tab-view-set-selected-page (group-tab-view group) (view-page win view))
    (when focus (focus-view view))
    view))

(defun add-view (win buffer &optional (group (window-active-group win)))
  (let* ((preview (buffer-local buffer :preview-of))
         (image (image-buffer-p buffer))
         (view (make-editor-view buffer :gutter (not (or preview image)) :on-cursor-moved
                                 (lambda (view)
                                   (update-cursor-decorations view)
                                   (schedule-autodoc view)
                                   (completion-cursor-moved view)
                                   (when (eq view (selected-view win)) (update-status win)))))
         (gtk-buffer (buffer-text buffer)))
    (setf (gethash (view-widget view) (window-views win)) view
          (view-group view) group)
    (attach-syntax buffer)
    (cond
      (preview (setup-preview-view view))
      (image (setup-image-view view))
      (t (setup-note-tooltips view)
         (setup-symbol-hover view)
         (setup-context-menu view)
         (setup-git-gutter-clicks view)
         (setup-fold-gutter view)))
    (gobject:connect (gtk:scrolled-window-get-vadjustment (view-widget view)) :value-changed
                     (lambda (adjustment) (declare (ignore adjustment))
                       (schedule-highlight buffer)
                       (when (buffer-local buffer :preview) (preview-source-scrolled view))))
    ;; The group of the view with the focus is the active one.
    (let ((focus (gtk:event-controller-focus-new)))
      (gobject:connect focus :enter (lambda (&rest args) (declare (ignore args))
                                      (activate-group win (view-group view))))
      (gtk:widget-add-controller (view-text-view view) focus))
    (adw:tab-view-append (group-tab-view group) (view-widget view))
    (unless (buffer-local buffer :tab-title-handler)
      (setf (buffer-local buffer :tab-title-handler)
            (gobject:connect gtk-buffer :modified-changed
                             (lambda (b) (declare (ignore b)) (update-tab-titles win buffer)))))
    (update-tab-titles win buffer)
    (show-empty-or-tabs win)
    view))

(defun setup-group-signals (win group)
  (let ((tabs (group-tab-view group)))
    (gobject:connect tabs "notify::selected-page"
                     (lambda (tv pspec) (declare (ignore tv pspec))
                       (when (eq group (window-active-group win))
                         (update-status win)
                         (when (find-bar-open-p (window-find-bar win))
                           (find-update (window-find-bar win))))))
    (gobject:connect tabs :close-page
                     (lambda (tv page)
                       (declare (ignore tv))
                       (close-page-request win page)
                       t))
    (gobject:connect tabs :page-attached
                     (lambda (tv page position)
                       (declare (ignore tv position))
                       ;; Dragged here from another group.
                       (let ((view (page-view win page)))
                         (when view (setf (view-group view) group)))))
    (gobject:connect tabs :page-detached
                     (lambda (tv page position)
                       (declare (ignore tv position))
                       (page-detached win group page)))))

(defun group-has-widget-p (group widget)
  (find widget (group-pages group) :key #'adw:tab-page-get-child))

(defun page-detached (win group page)
  "PAGE left GROUP: closed, or moved to another group. Once that is settled,
forget a closed page's view and remove the group if it is now empty."
  (let ((view (page-view win page)))
    (glib:idle-add glib:+priority-default+
                   (lambda ()
                     (when (and view (not (find-if (lambda (g) (group-has-widget-p g (view-widget view)))
                                                   (window-groups win))))
                       (page-removed win view))
                     (when (and (null (group-pages group)) (rest (window-groups win))
                                (member group (window-groups win)))
                       (remove-group win group))
                     (show-empty-or-tabs win)
                     nil))))

(defun page-removed (win view)
  (remhash (view-widget view) (window-views win))
  (let ((buffer (view-buffer view)))
    (unless (buffer-views win buffer)
      (kill-buffer buffer)))
  (update-status win)
  (let ((next (selected-view win)))
    (when next (focus-view next))))

(defun close-page-request (win page)
  "Close PAGE, first asking about unsaved changes if this is the buffer's last view."
  (let* ((view (page-view win page))
         (tabs (group-tab-view (view-group view)))
         (buffer (and view (view-buffer view))))
    (if (and buffer (buffer-needs-saving-p buffer) (= 1 (length (buffer-views win buffer))))
        (ask-to-save win (list buffer)
                     (lambda (proceed) (adw:tab-view-close-page-finish tabs page proceed)))
        (adw:tab-view-close-page-finish tabs page t))))

(defun close-view (win view)
  "Close VIEW's tab without asking: another view shows its buffer."
  (adw:tab-view-close-page (group-tab-view (view-group view)) (view-page win view)))

;;; The tree of groups

(defun widget-replace (old new)
  "Put NEW where OLD is (in a gtk:paned or an adw:bin), taking OLD out."
  (let ((parent (gtk:widget-get-parent old)))
    (cond ((typep parent 'gtk:paned)
           (if (eq (gtk:paned-get-start-child parent) old)
               (progn (gtk:paned-set-start-child parent nil) (gtk:paned-set-start-child parent new))
               (progn (gtk:paned-set-end-child parent nil) (gtk:paned-set-end-child parent new))))
          ((typep parent 'adw:bin)
           (adw:bin-set-child parent nil)
           (adw:bin-set-child parent new))
          (t (error "Can't replace a widget in ~a" parent)))))

(defun groups-in-order (win)
  "The groups, left to right and top to bottom."
  (let ((order '()))
    (labels ((walk (widget)
               (cond ((null widget))
                     ((typep widget 'gtk:paned)
                      (walk (gtk:paned-get-start-child widget))
                      (walk (gtk:paned-get-end-child widget)))
                     (t (let ((g (find widget (window-groups win) :key #'group-widget)))
                          (when g (push g order)))))))
      (walk (adw:bin-get-child (window-groups-holder win))))
    (nreverse order)))

(defun split-group (win group direction)
  "Add a group beside GROUP (DIRECTION :right or :below), showing GROUP's
current buffer. Returns the new group."
  (let* ((new (make-editor-group win))
         (old-widget (group-widget group))
         (horizontal (eq direction :right))
         (extent (if horizontal (gtk:widget-get-width old-widget) (gtk:widget-get-height old-widget)))
         (paned (make-instance 'gtk:paned :orientation (if horizontal :horizontal :vertical)
                                          :shrink-start-child nil :shrink-end-child nil
                                          :hexpand t :vexpand t)))
    (widget-replace old-widget paned)
    (gtk:paned-set-start-child paned old-widget)
    (gtk:paned-set-end-child paned (group-widget new))
    (when (plusp extent) (gtk:paned-set-position paned (floor extent 2)))
    (setf (window-groups win) (append (window-groups win) (list new)))
    (let ((view (group-selected-view win group)))
      (when view
        (let ((copy (add-view win (view-buffer view) new)))
          ;; Show the same place.
          (let ((gtk-buffer (view-gtk-buffer view)))
            (glib:idle-add glib:+priority-default-idle+
                           (lambda ()
                             (gtk:text-view-scroll-to-mark (view-text-view copy) (gtk:text-buffer-get-insert gtk-buffer)
                                                           0.2d0 nil 0d0 0d0)
                             nil))))))
    new))

(defun remove-group (win group)
  "Take GROUP (which should have no tabs) out of the tree."
  (let* ((widget (group-widget group))
         (parent (gtk:widget-get-parent widget)))
    (setf (window-groups win) (remove group (window-groups win)))
    (when (typep parent 'gtk:paned)
      (let ((other (if (eq (gtk:paned-get-start-child parent) widget)
                       (gtk:paned-get-end-child parent)
                       (gtk:paned-get-start-child parent))))
        (gtk:paned-set-start-child parent nil)
        (gtk:paned-set-end-child parent nil)
        (widget-replace parent other)))
    (when (eq group (window-active-group win))
      (setf (window-active-group win) (first (groups-in-order win)))
      (update-status win)
      (let ((view (selected-view win)))
        (when view (focus-view view))))))

(defun merge-group-into (win from to)
  "Move FROM's tabs to TO (closing those whose buffer TO already shows), then remove FROM."
  (dolist (page (group-pages from))
    (let ((view (page-view win page)))
      (if (and view (find to (buffer-views win (view-buffer view)) :key #'view-group))
          (close-view win view)
          (adw:tab-view-transfer-page (group-tab-view from) page (group-tab-view to)
                                      (adw:tab-view-get-n-pages (group-tab-view to))))))
  ;; FROM goes once its pages are detached (PAGE-DETACHED); if it had none, now.
  (when (and (null (group-pages from)) (member from (window-groups win)))
    (remove-group win from)))

(defun setup-tabs (win)
  (let ((group (make-editor-group win)))
    (setf (window-groups win) (list group)
          (window-active-group win) group)
    (adw:bin-set-child (window-groups-holder win) (group-widget group))))

;;; Commands

(defun focus-group (win group)
  (activate-group win group)
  (let ((view (group-selected-view win group)))
    (if view (focus-view view) (gtk:widget-grab-focus (group-tab-view group)))))

(define-command split-below ()
  "Split the editor: the current file also shows in a new group below."
  (split-group *window* (window-active-group *window*) :below))

(define-command split-right ()
  "Split the editor: the current file also shows in a new group to the right."
  (split-group *window* (window-active-group *window*) :right))

(define-command split-editor ()
  "Split the editor to the right and move to the new group."
  (focus-group *window* (split-group *window* (window-active-group *window*) :right)))

(define-command other-group ()
  "Move to the next editor group."
  (:repeat t)
  (let* ((order (groups-in-order *window*))
         (next (or (second (member (window-active-group *window*) order)) (first order))))
    (focus-group *window* next)))

(define-command delete-group ()
  "Close this editor group; its tabs move to the group beside it."
  (let* ((win *window*)
         (order (groups-in-order win))
         (group (window-active-group win)))
    (unless (rest order) (editor-error "This is the only editor group"))
    (let ((neighbour (or (second (member group (reverse order))) (second order))))
      (merge-group-into win group neighbour)
      (focus-group win neighbour))))

(define-command delete-other-groups ()
  "Make this the only editor group; the other groups' tabs move into it."
  (let* ((win *window*)
         (group (window-active-group win)))
    (dolist (other (remove group (window-groups win)))
      (merge-group-into win other group))
    (focus-group win group)))

(defun focus-group-number (n)
  (let ((group (nth n (groups-in-order *window*))))
    (if group (focus-group *window* group) (message "There is no group ~d" (1+ n)))))

(define-command focus-first-group () "Move to the first editor group." (focus-group-number 0))
(define-command focus-second-group () "Move to the second editor group." (focus-group-number 1))
(define-command focus-third-group () "Move to the third editor group." (focus-group-number 2))

(define-command move-tab-to-next-group ()
  "Move the current tab to the next editor group, splitting if there is only one."
  (let* ((win *window*)
         (group (window-active-group win))
         (view (or (selected-view win) (editor-error "No file is open.")))
         (order (groups-in-order win))
         (target (or (second (member group order))
                     (and (rest order) (first order)))))
    (if target
        (progn
          (if (find target (buffer-views win (view-buffer view)) :key #'view-group)
              (close-view win view)
              (adw:tab-view-transfer-page (group-tab-view group) (view-page win view) (group-tab-view target)
                                          (adw:tab-view-get-n-pages (group-tab-view target))))
          (focus-group win target))
        (let ((new (split-group win group :right)))
          (close-view win view)
          (focus-group win new)))))

;;; Asking about unsaved changes

(defun buffer-needs-saving-p (buffer)
  "Whether closing BUFFER would lose work: it has unsaved changes, and is a
file or an untitled buffer, not one of Cadre's own (*repl*, *Help*, …)."
  (and (buffer-modified-p buffer)
       (or (buffer-file buffer)
           (let ((name (buffer-name buffer)))
             (not (and (plusp (length name)) (char= (char name 0) #\*)))))))

(defun ask-to-save (win buffers continuation)
  "Ask whether to save BUFFERS, which have unsaved changes. Calls
CONTINUATION with t once they are saved or discarded, or nil if the user cancels."
  (let ((dialog (adw:alert-dialog-new
                 (if (rest buffers)
                     (format nil "Save changes to ~d files?" (length buffers))
                     (format nil "Save changes to ~a?" (buffer-name (first buffers))))
                 (format nil "~:[It has~;They have~] unsaved changes, which will be lost if you don't save."
                         (rest buffers)))))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "discard" "_Don't Save")
    (adw:alert-dialog-add-response dialog "save" "_Save")
    (adw:alert-dialog-set-response-appearance dialog "discard" :destructive)
    (adw:alert-dialog-set-response-appearance dialog "save" :suggested)
    (adw:alert-dialog-set-default-response dialog "save")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window win))
               (lambda (response)
                 (cond ((string= response "save")
                        (save-buffers win buffers (lambda (ok) (funcall continuation ok))))
                       ((string= response "discard") (funcall continuation t))
                       (t (funcall continuation nil)))))))

(defun request-close (win)
  "Handle the window's close button: ask about unsaved buffers. Returns t
to stop GTK closing the window now."
  (save-session win)
  (let ((modified (remove-if-not #'buffer-needs-saving-p (buffer-list))))
    (cond ((null modified) nil)
          (t (ask-to-save win modified
                          (lambda (proceed)
                            (when proceed
                              (dolist (b modified) (setf (buffer-modified-p b) nil))
                              (gtk:window-destroy (window-gtk-window win)))))
             t))))
