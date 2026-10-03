;;;; tabs.lisp — tabs: an adw:tab-view of editor views

(in-package #:cadre-ui)

(defun selected-view (win)
  "The view in WIN's selected tab, or nil."
  (let ((page (adw:tab-view-get-selected-page (window-tab-view win))))
    (and page (gethash (adw:tab-page-get-child page) (window-views win)))))

(defun window-pages (win)
  (let ((tabs (window-tab-view win)))
    (loop for i below (adw:tab-view-get-n-pages tabs)
          collect (adw:tab-view-get-nth-page tabs i))))

(defun page-view (win page)
  (gethash (adw:tab-page-get-child page) (window-views win)))

(defun view-page (win view)
  (adw:tab-view-get-page (window-tab-view win) (view-widget view)))

(defun buffer-views (win buffer)
  "The views in WIN showing BUFFER."
  (loop for view being the hash-values of (window-views win)
        when (eq (view-buffer view) buffer) collect view))

(defun tab-title (buffer)
  (format nil "~a~:[~; ●~]" (buffer-display-name buffer) (buffer-modified-p buffer)))

(defun update-tab-titles (win buffer)
  (dolist (view (buffer-views win buffer))
    (let ((page (view-page win view)))
      (adw:tab-page-set-title page (tab-title buffer))
      (adw:tab-page-set-tooltip page (if (buffer-file buffer)
                                         (uiop:native-namestring (buffer-file buffer))
                                         (buffer-name buffer))))))

(defun show-empty-or-tabs (win)
  (gtk:stack-set-visible-child-name (window-editor-stack win)
                                    (if (window-pages win) "tabs" "empty")))

(defun show-buffer (win buffer &key (focus t))
  "Select a tab showing BUFFER in WIN, adding one if there is none. Returns the view."
  (let ((view (or (first (buffer-views win buffer))
                  (add-view win buffer))))
    (adw:tab-view-set-selected-page (window-tab-view win) (view-page win view))
    (when focus (focus-view view))
    view))

(defun add-view (win buffer)
  (let* ((view (make-editor-view buffer :on-cursor-moved
                                 (lambda (view)
                                   (update-cursor-decorations view)
                                   (when (eq view (selected-view win)) (update-status win)))))
         (gtk-buffer (buffer-text buffer)))
    (setf (gethash (view-widget view) (window-views win)) view)
    (attach-syntax buffer)
    (gobject:connect (gtk:scrolled-window-get-vadjustment (view-widget view)) :value-changed
                     (lambda (adjustment) (declare (ignore adjustment))
                       (schedule-highlight buffer)))
    (adw:tab-view-append (window-tab-view win) (view-widget view))
    (unless (buffer-local buffer :tab-title-handler)
      (setf (buffer-local buffer :tab-title-handler)
            (gobject:connect gtk-buffer :modified-changed
                             (lambda (b) (declare (ignore b)) (update-tab-titles win buffer)))))
    (update-tab-titles win buffer)
    (show-empty-or-tabs win)
    view))

(defun setup-tabs (win)
  (let ((tabs (window-tab-view win)))
    (gobject:connect tabs "notify::selected-page"
                     (lambda (tv pspec) (declare (ignore tv pspec))
                       (update-status win)
                       (when (find-bar-open-p (window-find-bar win))
                         (find-update (window-find-bar win)))))
    (gobject:connect tabs :close-page
                     (lambda (tv page)
                       (declare (ignore tv))
                       (close-page-request win page)
                       t))
    (gobject:connect tabs :page-detached
                     (lambda (tv page position)
                       (declare (ignore tv position))
                       (page-removed win page)))))

(defun page-removed (win page)
  (let ((view (page-view win page)))
    (when view
      (remhash (view-widget view) (window-views win))
      (let ((buffer (view-buffer view)))
        (unless (buffer-views win buffer)
          (kill-buffer buffer))))
    (show-empty-or-tabs win)
    (update-status win)
    (let ((next (selected-view win)))
      (when next (focus-view next)))))

(defun close-page-request (win page)
  "Close PAGE, first asking about unsaved changes if this is the buffer's last view."
  (let* ((tabs (window-tab-view win))
         (view (page-view win page))
         (buffer (and view (view-buffer view))))
    (if (and buffer (buffer-modified-p buffer) (= 1 (length (buffer-views win buffer))))
        (ask-to-save win (list buffer)
                     (lambda (proceed) (adw:tab-view-close-page-finish tabs page proceed)))
        (adw:tab-view-close-page-finish tabs page t))))

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
  (let ((modified (remove-if-not #'buffer-modified-p (buffer-list))))
    (cond ((null modified) nil)
          (t (ask-to-save win modified
                          (lambda (proceed)
                            (when proceed
                              (dolist (b modified) (setf (buffer-modified-p b) nil))
                              (gtk:window-destroy (window-gtk-window win)))))
             t))))
