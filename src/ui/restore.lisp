;;;; restore.lisp — saving the session on exit and restoring it at startup
;;;;
;;;; For each project folder Cadre remembers the open files in each editor
;;;; group (with the cursor in each), how the groups are split, which group
;;;; was active, whether the panel and sidebar were shown and their sizes,
;;;; and whether a Lisp was running. It lives in
;;;; ~/.local/state/cadre/session.sexp ($XDG_STATE_HOME), written when the
;;;; window closes, and is restored when the project is next opened
;;;; without files named on the command line.

(in-package #:cadre-ui)

(define-option *restore-session* t boolean
  "Reopen a project's files, splits and panel as they were when Cadre last closed."
  :category "Session")

(define-option *restore-lisp* t boolean
  "When restoring a session, start the Lisp again if one was running."
  :category "Session")

(defparameter *remembered-projects* 30)

(defun state-directory ()
  (let ((xdg (uiop:getenv "XDG_STATE_HOME")))
    (merge-pathnames "cadre/" (if (and xdg (plusp (length xdg)))
                                  (uiop:ensure-directory-pathname xdg)
                                  (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defun session-file () (merge-pathnames "session.sexp" (state-directory)))

(defun read-sessions ()
  (or (ignore-errors
       (with-open-file (in (session-file) :if-does-not-exist nil)
         (and in (with-standard-io-syntax
                   (let ((*read-eval* nil) (*package* (find-package :keyword)))
                     (read in nil nil))))))
      '()))

(defun write-sessions (sessions)
  (handler-case
      (progn
        (ensure-directories-exist (session-file))
        (with-open-file (out (session-file) :direction :output :if-exists :supersede)
          (with-standard-io-syntax
            (let ((*package* (find-package :keyword)))
              (format out ";;; Written by Cadre: the files and layout to restore for each project.~%")
              (prin1 sessions out)
              (terpri out)))))
    (error (e) (message "Could not save the session: ~a" e))))

;;; Describing the session

(defun view-state (view)
  "What to remember about VIEW: its file and cursor, (:preview file) for a
Markdown preview, or nil if it has no file."
  (let* ((buffer (view-buffer view))
         (file (buffer-file buffer))
         (source (buffer-local buffer :preview-of)))
    (cond (file
           (multiple-value-bind (line column) (view-cursor-line-column view)
             (list (uiop:native-namestring file) (1- line) (1- column))))
          ((and source (buffer-file source))
           (list :preview (uiop:native-namestring (buffer-file source)))))))

(defun group-state (win group)
  (let* ((views (loop for page in (group-pages group)
                      for view = (page-view win page)
                      when view collect view))
         (selected (group-selected-view win group))
         (files (remove nil (mapcar #'view-state views))))
    (list :group :files files
               :selected (and selected (position (view-state selected) files :test #'equal)))))

(defun paned-fraction (paned)
  (let ((extent (if (eq (gtk:orientable-get-orientation paned) :horizontal)
                    (gtk:widget-get-width paned)
                    (gtk:widget-get-height paned))))
    (if (plusp extent) (/ (gtk:paned-get-position paned) (float extent)) 0.5)))

(defun layout-state (win widget)
  (if (typep widget 'gtk:paned)
      (list :split (if (eq (gtk:orientable-get-orientation widget) :horizontal) :right :below)
            (paned-fraction widget)
            (layout-state win (gtk:paned-get-start-child widget))
            (layout-state win (gtk:paned-get-end-child widget)))
      (group-state win (find widget (window-groups win) :key #'group-widget))))

(defun session-state (win)
  (remember-panel-size win)
  (list :layout (layout-state win (adw:bin-get-child (window-groups-holder win)))
        :active (position (window-active-group win) (groups-in-order win))
        :panel-visible (panel-visible-p win)
        :panel-sizes (window-panel-sizes win)
        :panel-page (gtk:stack-get-visible-child-name (panel-stack (window-panel win)))
        :sidebar-visible (gtk:widget-get-visible (window-sidebar win))
        :sidebar-width (gtk:paned-get-position (window-side-paned win))
        :sidebar-page (sidebar-page win)
        :lisp (cond ((and (connected-p) *inferior*) :started)
                    ((connected-p) (list :connected (connection-host *connection*)
                                         (connection-port *connection*))))
        :size (let ((w (window-gtk-window win)))
                (list (gtk:widget-get-width w) (gtk:widget-get-height w)
                      (gtk:window-is-maximized w)))))

(defun save-session (win)
  "Remember WIN's session for its project."
  (when (and *restore-session* (window-project win))
    (let ((key (uiop:native-namestring (window-project win))))
      (write-sessions
       (let ((others (remove key (read-sessions) :key #'car :test #'equal)))
         (cons (cons key (ignore-errors (session-state win)))
               (subseq others 0 (min (length others) (1- *remembered-projects*)))))))))

;;; Restoring it

(defun load-file-buffer (path)
  "The buffer visiting PATH, reading the file now if no buffer does. Nil if it can't be read."
  (or (find-file-buffer path)
      (let ((octets (ignore-errors
                     (with-open-file (in path :element-type '(unsigned-byte 8))
                       (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                         (read-sequence v in)
                         v)))))
        (and octets (make-buffer :file (pathname path) :text (make-gtk-text (decode-file-contents octets)))))))

(defun restore-group (win group state)
  (destructuring-bind (&key files selected &allow-other-keys) (rest state)
    (let ((views (loop for (path line column) in files
                       for preview = (eq path :preview)
                       for file = (if preview line path)
                       for buffer = (and (stringp file) (probe-file file) (load-file-buffer file))
                       collect (cond ((null buffer) nil)
                                     (preview
                                      (add-view win (or (let ((p (buffer-local buffer :preview)))
                                                          (and p (member p (buffer-list)) p))
                                                        (make-preview-buffer buffer))
                                                group))
                                     (t (let ((view (add-view win buffer group)))
                                          (gtk:text-buffer-place-cursor (view-gtk-buffer view)
                                                                        (line-iter (view-gtk-buffer view) line column))
                                          view))))))
      (let ((view (or (and selected (nth selected views)) (find-if #'identity views))))
        (when view
          (adw:tab-view-set-selected-page (group-tab-view group) (view-page win view))))
      (dolist (view (remove-if (lambda (v) (or (null v) (buffer-local (view-buffer v) :preview-of))) views))
        (let ((view view))
          (glib:idle-add glib:+priority-default-idle+ (lambda () (scroll-to-cursor view) nil)))))))

(defun set-paned-fraction (paned fraction &optional (tries 40))
  (let ((extent (if (eq (gtk:orientable-get-orientation paned) :horizontal)
                    (gtk:widget-get-width paned)
                    (gtk:widget-get-height paned))))
    (if (plusp extent)
        (gtk:paned-set-position paned (round (* extent fraction)))
        (when (plusp tries)
          (glib:timeout-add glib:+priority-default+ 25
                            (lambda () (set-paned-fraction paned fraction (1- tries)) nil))))))

(defun restore-layout (win group state)
  "Fill GROUP, which is where STATE goes, splitting it as STATE says."
  (ecase (first state)
    (:group (restore-group win group state))
    (:split (destructuring-bind (direction fraction a b) (rest state)
              (let ((new (split-group win group direction)))
                (restore-layout win group a)
                (restore-layout win new b)
                (set-paned-fraction (gtk:widget-get-parent (group-widget group)) fraction))))))

(defun restore-session (win)
  "Restore the session saved for WIN's project. Returns t if there was one."
  (let ((state (and *restore-session* (window-project win)
                    (cdr (assoc (uiop:native-namestring (window-project win)) (read-sessions)
                                :test #'equal)))))
    (when state
      (handler-case
          (destructuring-bind (&key layout active panel-visible panel-sizes panel-page
                                    sidebar-visible sidebar-width sidebar-page lisp size
                               &allow-other-keys)
              state
            (when size
              (destructuring-bind (w h &optional maximized) size
                (gtk:window-set-default-size (window-gtk-window win) w h)
                (when maximized (gtk:window-maximize (window-gtk-window win)))))
            (when layout (restore-layout win (window-active-group win) layout))
            (let ((group (and active (nth active (groups-in-order win)))))
              (when group (activate-group win group)))
            (when panel-sizes (setf (window-panel-sizes win) panel-sizes))
            (when (and panel-page (not (member panel-page *panel-pages-shown-when-used* :test #'equal)))
              (gtk:stack-set-visible-child-name (panel-stack (window-panel win)) panel-page))
            (set-panel-visible win panel-visible)
            (when sidebar-width (gtk:paned-set-position (window-side-paned win) sidebar-width))
            (when sidebar-page (gtk:stack-set-visible-child-name (window-sidebar-stack win) sidebar-page))
            (set-sidebar-visible win sidebar-visible)
            (show-empty-or-tabs win)
            (when *restore-lisp*
              (cond ((eq lisp :started) (unless (or (connected-p) *connecting*) (start-lisp)))
                    ((and (consp lisp) (eq (first lisp) :connected))
                     (connect-to (second lisp) (third lisp)))))
            t)
        (error (e) (message "Could not restore the session: ~a" e) nil)))))
