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

(defun add-widget-view (win buffer group)
  "A tab for BUFFER showing the widget its :tab-widget function makes (a
database table): the view's text view stays out of sight."
  (let ((view (make-editor-view buffer :gutter nil)))
    (setf (slot-value view 'widget) (funcall (buffer-local buffer :tab-widget) view)
          (gethash (view-widget view) (window-views win)) view
          (view-group view) group)
    (let ((focus (gtk:event-controller-focus-new)))
      (gobject:connect focus :enter (lambda (&rest args) (declare (ignore args))
                                      (activate-group win (view-group view))))
      (gtk:widget-add-controller (view-widget view) focus))
    (adw:tab-view-append (group-tab-view group) (view-widget view))
    (unless (buffer-local buffer :tab-title-handler)
      (setf (buffer-local buffer :tab-title-handler)
            (gobject:connect (buffer-text buffer) :modified-changed
                             (lambda (b) (declare (ignore b)) (update-tab-titles win buffer)))))
    (update-tab-titles win buffer)
    (show-empty-or-tabs win)
    view))

(defun widget-view-p (view)
  (and (buffer-local (view-buffer view) :tab-widget) t))

(defun source-editor-view-p (view)
  "Whether VIEW shows source, not a rendered preview: a Markdown tab switched
to its preview keeps the source view's controllers, which then stand aside."
  (not (buffer-local (view-buffer view) :preview-of)))

(defun setup-source-editor-view (view)
  (unless (view-source-controllers-p view)
    (setup-note-tooltips view)
    (setup-symbol-hover view)
    (setup-context-menu view)
    (setup-git-gutter-clicks view)
    (setup-fold-gutter view)
    (setf (view-source-controllers-p view) t)))

(defun add-view (win buffer &optional (group (window-active-group win)))
  (when (buffer-local buffer :tab-widget)
    (return-from add-view (add-widget-view win buffer group)))
  (let* ((preview (buffer-local buffer :preview-of))
         (image (image-buffer-p buffer))
         (hyperspec (hyperspec-buffer-p buffer))
         (view (make-editor-view buffer :gutter (not (or preview image hyperspec)) :on-cursor-moved
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
      (hyperspec (setup-hyperspec-view view))
      (t (setup-source-editor-view view)))
    (gobject:connect (gtk:scrolled-window-get-vadjustment (view-widget view)) :value-changed
                     (lambda (adjustment) (declare (ignore adjustment))
                       (schedule-highlight (view-buffer view))
                       (when (buffer-local (view-buffer view) :preview) (preview-source-scrolled view))))
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
  (dolist (handler (view-buffer-handlers view))
    (gobject:disconnect (view-gtk-buffer view) handler))
  (setf (view-buffer-handlers view) nil)
  (remhash (view-widget view) (window-views win))
  (let* ((buffer (view-buffer view))
         (source (buffer-local buffer :preview-of)))
    (unless (buffer-views win buffer)
      (kill-buffer buffer)
      (when (and source (member source (buffer-list)) (null (buffer-views win source)))
        (kill-buffer source))))
  (update-status win)
  (let ((next (selected-view win)))
    (when next (focus-view next))))

(defun close-page-request (win page)
  "Close PAGE, first asking about unsaved changes if this is the buffer's last view."
  (let* ((view (page-view win page))
         (tabs (group-tab-view (view-group view)))
         (buffer (and view (view-buffer view))))
    (if (and buffer (= 1 (length (buffer-views win buffer))))
        (confirm-close-buffers win
                     (list (let ((source (buffer-local buffer :preview-of)))
                             (if (and source (null (buffer-views win source))) source buffer)))
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
file or an untitled buffer, not one of Cadre's own (*repl*, *Help*, …) or
the scratch buffer, which saves itself."
  (and (buffer-modified-p buffer)
       (not (buffer-local buffer :scratch))
       (or (buffer-file buffer)
           (let ((name (buffer-name buffer)))
             (not (and (plusp (length name)) (char= (char name 0) #\*)))))))

(defun confirm-close-buffers (win buffers continuation)
  "Wait, offer save/discard/cancel, and deliver one final decision for a close."
  (let ((completed nil))
    (labels ((finish (proceed)
               ;; Mark completion before calling GTK: an error in the caller must
               ;; never cause a second close-page-finish or window destruction.
               (unless completed
                 (setf completed t)
                 (funcall continuation proceed)))
             (guard (function)
               ;; Only callbacks entering from an async operation need a handler.
               (lambda (&rest arguments)
                 (unless completed
                   (handler-case (apply function arguments)
                     (error (e)
                       (finish nil)
                       (error e))))))
             (prompt (&optional retry-p)
               (let ((remaining (remove-if-not #'buffer-needs-saving-p buffers)))
                 (if remaining
                     (let ((responded nil))
                       (choose-save-response
                        win remaining retry-p
                        (guard (lambda (response)
                                 (unless responded
                                   (setf responded t)
                                   (cond ((string= response "save")
                                          (save-buffers win remaining (guard #'saved)
                                                        :on-cancel (lambda () (finish nil))
                                                        :on-error (lambda () (finish nil))))
                                         ((string= response "discard") (finish t))
                                         (t (finish nil))))))
                        (lambda (e)
                          (declare (ignore e))
                          (unless responded
                            (setf responded t)
                            (finish nil)))))
                     (finish t))))
             (saved (ok)
               (if (some #'buffer-needs-saving-p buffers)
                   (prompt t)
                   (finish ok))))
      ;; Idle buffers can proceed synchronously; otherwise the waiter owns the
      ;; callback until the current writes finish.
      (funcall (guard (lambda ()
                        (if (some #'buffer-save-busy-p buffers)
                            (wait-for-buffer-writes buffers (guard #'prompt))
                            (prompt))))))))

(defun choose-save-response (win buffers retry-p success failure)
  "Present the save dialog and deliver either a response or a dialog error."
  (let ((dialog (adw:alert-dialog-new
                 (if (rest buffers)
                     (format nil "Save changes to ~d files?" (length buffers))
                     (format nil "Save changes to ~a?" (buffer-name (first buffers))))
                 (if retry-p
                     "Changes remain unsaved because a save did not finish or the file changed while saving. Save again, discard the changes, or cancel closing."
                     (format nil "~:[It has~;They have~] unsaved changes, which will be lost if you don't save."
                             (rest buffers))))))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "discard" "_Don't Save")
    (adw:alert-dialog-add-response dialog "save" "_Save")
    (adw:alert-dialog-set-response-appearance dialog "discard" :destructive)
    (adw:alert-dialog-set-response-appearance dialog "save" :suggested)
    (adw:alert-dialog-set-default-response dialog "save")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window win)) success :error failure)))

(defun request-close (win)
  "Handle the window's close button. Returns t to stop GTK closing now;
repeated requests share the pending decision."
  (when (window-close-pending-p win)
    (return-from request-close t))
  (setf (window-close-pending-p win) t)
  (let ((waiting nil) (accepted nil))
    (unwind-protect
         (progn
           (save-session win)
           (save-scratch-buffers)
           (let* ((buffers (buffer-list))
                  (modified (remove-if-not #'buffer-needs-saving-p buffers)))
             (when (or modified
                       (some #'buffer-save-busy-p buffers))
               (confirm-close-buffers
                win buffers
                (lambda (proceed)
                  (unwind-protect
                       (when proceed
                         (dolist (b modified) (setf (buffer-modified-p b) nil))
                         (discard-session-checkpoints)
                         (gtk:window-destroy (window-gtk-window win)))
                    (setf (window-close-pending-p win) nil))))
               (setf waiting t)))
           (unless waiting (setf accepted t))
           waiting)
      (unless waiting
        (when accepted (discard-session-checkpoints))
        (setf (window-close-pending-p win) nil)))))
