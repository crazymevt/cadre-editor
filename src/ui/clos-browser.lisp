;;;; CLOS Browser: an editor tab backed by the connected Common Lisp image.
(in-package #:cadre-ui)

(define-major-mode clos-browser-mode (:title "CLOS Browser")
  "Classes and generic functions in the connected Lisp.")

(defstruct (clos-page (:conc-name cb-))
  buffer widget packages search list title status details inspect source back list-scroller detail-scroller
  (package "COMMON-LISP-USER") catalog selected description connection
  (rows (make-hash-table :test 'eq)) (updating nil) (serial 0) (catalog-serial 0)
  (section "Slots") (history '()) pending-restore)

(defvar *clos-helper-states* (make-hash-table :test 'eq))

(defun clos-pages ()
  (loop for buffer in (buffer-list) append (buffer-local buffer :clos-pages)))

(defun clos-page-live-p (page connection)
  (and (member (cb-buffer page) (buffer-list))
       (eq connection *connection*) (eq connection (cb-connection page)) (connected-p)))

(defun clos-update-back-button (page)
  (when (cb-back page)
    (gtk:widget-set-sensitive (cb-back page) (and (cb-history page) (connected-p)))))

(defun clos-remember-current (page)
  (when (cb-selected page)
    (push (list :entry (copy-list (cb-selected page)) :package (cb-package page)
                :section (cb-section page)
                :filter (if (cb-search page) (gtk:editable-get-text (cb-search page)) "")
                :details-y (when (cb-detail-scroller page)
                             (gtk:adjustment-get-value (gtk:scrolled-window-get-vadjustment (cb-detail-scroller page))))
                :list-y (when (cb-list-scroller page)
                          (gtk:adjustment-get-value (gtk:scrolled-window-get-vadjustment (cb-list-scroller page)))))
          (cb-history page)))
  (clos-update-back-button page))

(defun clos-restore-scroll (page snapshot connection serial)
  ;; Let GTK allocate the restored content before applying its scroll offsets.
  (when (or (getf snapshot :details-y) (getf snapshot :list-y))
    (glib:timeout-add glib:+priority-default+ 50
     (lambda ()
       (when (and (clos-page-live-p page connection) (= serial (cb-serial page)))
         (loop for (scroller key) in (list (list (cb-detail-scroller page) :details-y)
                                         (list (cb-list-scroller page) :list-y))
               when (and scroller (getf snapshot key)) do
                 (gtk:adjustment-set-value (gtk:scrolled-window-get-vadjustment scroller) (getf snapshot key))))
       nil))))

(defun clos-go-back (page)
  (unless (clos-page-live-p page (cb-connection page)) (editor-error "Connect to a Common Lisp image first"))
  (let ((snapshot (pop (cb-history page))))
    (when snapshot
      (let ((package-changed (not (equal (cb-package page) (getf snapshot :package)))))
        ;; Invalidate catalog replies for the navigation we are leaving too.
        (incf (cb-catalog-serial page))
        (setf (cb-package page) (getf snapshot :package)
              (cb-section page) (getf snapshot :section)
              (cb-selected page) (getf snapshot :entry)
              (cb-pending-restore page) snapshot)
        (when (cb-search page) (gtk:editable-set-text (cb-search page) (getf snapshot :filter)))
        (if package-changed
            (refresh-clos-page page)
            (progn
              (when (cb-list page) (clos-fill-list page))
              (clos-select-entry page (getf snapshot :entry) :remember nil))))))
  (clos-update-back-button page))

(defun ensure-clos-helper (connection then failure)
  (let ((state (gethash connection *clos-helper-states*)))
    (cond ((eq state :ready) (funcall then))
          ((consp state) (setf (gethash connection *clos-helper-states*)
                               (append state (list (cons then failure)))))
          (t
           (setf (gethash connection *clos-helper-states*) (list (cons then failure)))
           (rex connection (swank-call "swank:interactive-eval-region" +clos-browser-helper+ 3 120)
                :package "COMMON-LISP-USER"
                :on-ok (lambda (reply)
                         (declare (ignore reply))
                         (let ((pending (gethash connection *clos-helper-states*)))
                           (setf (gethash connection *clos-helper-states*) :ready)
                           (dolist (pair pending) (funcall (car pair)))))
                :on-abort (lambda (reason)
                            (let ((pending (gethash connection *clos-helper-states*)))
                              (remhash connection *clos-helper-states*)
                              (dolist (pair pending) (funcall (cdr pair) reason)))))))))

(defun clos-label (text &optional heading)
  (make-instance 'gtk:label :label text :xalign 0.0 :wrap t :selectable t
                            :wrap-mode :word-char
                            :css-classes (if heading '("heading") '("monospace"))))

(defun clos-display-name (text)
  "Compact display only; requests and tooltips keep the original qualified name."
  (labels ((symbol-text (name)
             (let ((start 0) (quoted nil) (escaped nil))
               (loop for c across name for i from 0 do
                 (cond (escaped (setf escaped nil))
                       ((char= c #\\) (setf escaped t))
                       ((char= c #\|) (setf quoted (not quoted)))
                       ((and (not quoted) (char= c #\:)) (setf start (1+ i)))))
               (let ((short (subseq name start)))
                 (if (or (find #\| short) (find #\\ short)) short (string-downcase short)))))
           (walk (value)
             (typecase value
               (remote-symbol (symbol-text (remote-symbol-name value)))
               (null "nil")
               (keyword (format nil ":~(~a~)" value))
               (string (prin1-to-string value))
               (cons (format nil "(~{~a~^ ~})" (mapcar #'walk value)))
               (t (string-downcase (princ-to-string value))))))
    (handler-case (walk (read-sexp text)) (error () text))))

(defun clos-caption (text)
  (make-instance 'gtk:label :label text :xalign 0.0 :wrap t :wrap-mode :word-char
                            :css-classes '("dim-label" "caption")))

(defun clos-name-label (name)
  (make-instance 'gtk:label :label (clos-display-name name) :tooltip-text name :xalign 0.0
                            :selectable t :ellipsize :end :max-width-chars 28
                            :css-classes '("monospace")))

(defun clos-button (text action &optional tooltip)
  (let ((button (make-instance 'gtk:button :label text :halign :start :css-classes '("flat")
                                          :tooltip-text (or tooltip text))))
    (gobject:connect button :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (funcall action)
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    button))

(defun clos-open-class (page name)
  (clos-select-entry page (list :kind :class :name name)))

(defun clos-name-button (name action)
  (let ((button (clos-button (clos-display-name name) action name)))
    (gtk:widget-add-css-class button "cadre-clos-link")
    (gtk:widget-add-css-class button "monospace")
    (let ((label (gtk:button-get-child button)))
      (gtk:label-set-xalign label 0.0)
      (gtk:label-set-ellipsize label :end)
      (gtk:label-set-max-width-chars label 28))
    button))

(defun clos-class-buttons (page names)
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 4)))
    (dolist (name names)
      (let ((name name))
        (gtk:box-append box (clos-name-button name (lambda () (clos-open-class page name))))))
    box))

(defun clos-table (headers rows)
  "A compact table whose cells share column widths and row separators."
  (let ((grid (make-instance 'gtk:grid :hexpand t :valign :start :css-classes '("cadre-clos-table"))))
    (loop for header in headers for column from 0 do
      (let ((cell (clos-caption header)))
        (gtk:widget-add-css-class cell "cadre-clos-table-head")
        (gtk:grid-attach grid cell column 0 1 1)))
    (loop for cells in rows for row from 1 do
      (loop for cell in cells for column from 0 do
        (let ((holder (make-instance 'gtk:box :orientation :vertical :hexpand t :valign :fill)))
          (gtk:box-append holder cell)
          (gtk:grid-attach grid holder column row 1 1))))
    grid))

(defun clos-slots-table (page description)
  (clos-table (list (if (getf description :finalized) "Effective slot" "Direct slot") "Type" "Defined by")
   (loop for slot in (getf description :slots) collect
     (let* ((extra (make-instance 'gtk:box :orientation :vertical :spacing 3 :margin-top 6))
            (name (clos-name-label (getf slot :name)))
            (expander (make-instance 'gtk:expander :label-widget name :child extra)))
       (gtk:box-append extra (clos-caption (format nil "Allocation: ~a" (clos-display-name (getf slot :allocation)))))
       (gtk:box-append extra (clos-caption (format nil "Initargs: ~a"
                                                     (if (getf slot :initargs)
                                                         (format nil "~{~a~^, ~}" (mapcar #'clos-display-name (getf slot :initargs)))
                                                         "None"))))
       (list expander (clos-name-label (getf slot :type)) (clos-class-buttons page (getf slot :defined-by)))))))

(defun clos-methods-table (page methods)
  (clos-table '("Generic function" "Qualifier" "Specializers" "")
   (loop for method in methods collect
     (let* ((method method)
            (name (getf method :name))
            (function (make-instance 'gtk:box :orientation :vertical :spacing 3))
            (specializers (make-instance 'gtk:box :orientation :vertical :spacing 4))
            (source (clos-button "Source" (lambda () (clos-goto-source (getf method :source)))
                                 "Go to this method's definition")))
       (gtk:box-append function
                       (clos-name-button name (lambda () (clos-select-entry page (list :kind :generic :name name)))))
       (when (getf method :inherited) (gtk:box-append function (clos-caption "Inherited")))
       (gtk:widget-set-tooltip-text function (getf method :lambda-list))
       (dolist (specializer (getf method :specializers))
         (let ((name (getf specializer :name)))
           (gtk:box-append specializers (if (getf specializer :class)
                                               (clos-name-button name (lambda () (clos-open-class page name)))
                                               (clos-name-label name)))))
       (gtk:widget-add-css-class source "cadre-clos-source")
       (gtk:widget-set-sensitive source (not (getf (parse-location (getf method :source)) :error)))
       (list function
             (clos-caption (if (getf method :qualifiers)
                               (format nil "~{~a~^ ~}" (mapcar #'clos-display-name (getf method :qualifiers)))
                               "Primary"))
             specializers source)))))

(defun clos-hierarchy-level (page heading names &optional selected)
  (let ((box (make-instance 'gtk:box :orientation :vertical :spacing 6 :halign :center))
        (flow (make-instance 'gtk:flow-box :selection-mode :none :homogeneous nil
                                         :column-spacing 8 :row-spacing 6 :halign :center
                                         :min-children-per-line 1 :max-children-per-line 6)))
    (gtk:box-append box (clos-caption heading))
    (if names
        (dolist (name names)
          (let* ((name name)
                 (node (if selected (clos-name-label name)
                           (clos-name-button name (lambda () (clos-open-class page name))))))
            (gtk:widget-add-css-class node (if selected "cadre-clos-current" "cadre-clos-node"))
            (gtk:flow-box-append flow node)))
        (gtk:flow-box-append flow (clos-caption "None")))
    (gtk:box-append box flow)
    box))

(defun clos-render-hierarchy (page description)
  (let ((box (cb-details page))
        (precedence (getf description :precedence)))
    (gtk:box-append box (clos-hierarchy-level page "Direct superclasses" (getf description :superclasses)))
    (gtk:box-append box (make-instance 'gtk:label :label "↓" :css-classes '("dim-label")))
    (gtk:box-append box (clos-hierarchy-level page "Selected class" (list (getf description :name)) t))
    (gtk:box-append box (make-instance 'gtk:label :label "↓" :css-classes '("dim-label")))
    (gtk:box-append box (clos-hierarchy-level page "Direct subclasses" (getf description :subclasses)))
    (gtk:box-append box
     (make-instance 'gtk:expander :label (format nil "Class precedence order (~d)" (length precedence))
                    :margin-top 16
                    :child (clos-table '("Order" "Class")
                            (loop for name in precedence for index from 1 collect
                              (let ((name name))
                                (list (clos-caption (princ-to-string index))
                                      (clos-name-button name (lambda () (clos-open-class page name)))))))))))

(defun clos-goto-source (source)
  (goto-location (parse-location source)))

(defun clos-render-detail (page)
  (let ((description (cb-description page)) (box (cb-details page)))
    (clear-box box)
    (when (or (null description) (getf description :error))
      (gtk:box-append box (clos-label (or (getf description :error) "Choose a class or generic function.")))
      (return-from clos-render-detail))
    (let* ((class (eq (getf description :kind) :class))
           (sections (if class '("Slots" "Methods" "Hierarchy" "Source") '("Methods" "Source")))
           (tabs (make-instance 'gtk:box :spacing 4)))
      (unless (member (cb-section page) sections :test #'equal) (setf (cb-section page) "Methods"))
      (gtk:box-append box (clos-caption (format nil "~a · ~a~@[ · ~a~]"
                                                              (getf description :package)
                                                              (clos-display-name (getf description :metaclass))
                                                              (when (getf description :lambda-list)
                                                                (clos-display-name (getf description :lambda-list))))))
      (dolist (section sections)
        (let* ((section section)
               (button (make-instance 'gtk:toggle-button :label section
                                       :active (equal section (cb-section page))
                                       :css-classes '("flat" "cadre-clos-tab"))))
          (gobject:connect button :clicked
                           (lambda (b) (declare (ignore b))
                             (setf (cb-section page) section) (clos-render-detail page)))
          (gtk:box-append tabs button)))
      (gtk:widget-add-css-class tabs "cadre-clos-tabs")
      (gtk:box-append box (sideways-scroller tabs))
      (when (plusp (length (getf description :documentation "")))
        (let ((doc (clos-label (getf description :documentation))))
          (gtk:widget-remove-css-class doc "monospace")
          (gtk:box-append box doc)))
      (dolist (warning (getf description :warnings)) (gtk:box-append box (clos-caption warning)))
      (cond
        ((equal (cb-section page) "Slots")
         (if (getf description :slots)
             (progn
               (gtk:box-append box (clos-slots-table page description))
               (gtk:box-append box (clos-caption "Includes inherited slots. Expand a slot for allocation and initargs.")))
             (gtk:box-append box (clos-caption "No slots available."))))
        ((equal (cb-section page) "Methods")
         (if (getf description :methods)
             (progn
               (gtk:box-append box (clos-methods-table page (getf description :methods)))
               (gtk:box-append box (clos-caption (if class
                                                     "Includes methods specializing superclasses. Select a generic function to see all its methods."
                                                     "Select a specializer to browse its class."))))
             (gtk:box-append box (clos-caption "No methods found."))))
        ((equal (cb-section page) "Hierarchy")
         (clos-render-hierarchy page description))
        (t
         (let ((location (parse-location (getf description :source))))
           (gtk:box-append box (clos-label (location-place-string location)))
           (unless (getf location :error)
             (gtk:box-append box (clos-button "Open definition in editor"
                                             (lambda () (clos-goto-source (getf description :source))))))))))))

(defun clos-fill-list (page)
  (let* ((entries (clos-browser-entries (cb-catalog page)
                                       :filter (gtk:editable-get-text (cb-search page))))
         (list (cb-list page)))
    (setf (cb-updating page) t)
    (unwind-protect
         (progn
           (gtk:list-box-remove-all list)
           (clrhash (cb-rows page))
           (dolist (kind '(:class :generic))
             (gtk:list-box-append list
                                  (make-instance 'gtk:list-box-row :selectable nil :activatable nil
                                   :child (make-instance 'gtk:label :label (if (eq kind :class) "Classes" "Generic functions")
                                                         :xalign 0.0 :margin-start 10 :margin-top 16 :margin-bottom 6
                                                         :css-classes '("dim-label" "caption-heading"))))
             (dolist (entry (remove-if-not (lambda (e) (eq kind (getf e :kind))) entries))
               (let ((row (make-instance 'gtk:list-box-row
                           :child (make-instance 'gtk:label :label (clos-display-name (getf entry :name)) :xalign 0.0 :ellipsize :end
                                                           :tooltip-text (getf entry :name) :margin-start 8
                                                           :margin-end 8 :margin-top 5 :margin-bottom 5))))
                 (setf (gethash row (cb-rows page)) entry)
                 (gtk:list-box-append list row)
                 (when (equal entry (cb-selected page)) (gtk:list-box-select-row list row)))))
           (unless entries (gtk:list-box-append list (clos-label "No matching definitions."))))
      (setf (cb-updating page) nil))))

(defun clos-select-entry (page entry &key (remember t))
  (let* ((connection (cb-connection page)) (serial (incf (cb-serial page)))
         (restore (and (not remember) (cb-pending-restore page))))
    (unless (clos-page-live-p page connection) (editor-error "Connect to a Common Lisp image first"))
    (when (and remember (not (equal entry (cb-selected page)))) (clos-remember-current page))
    (setf (cb-pending-restore page) nil)
    (setf (cb-selected page) entry (cb-description page) nil)
    (gtk:label-set-text (cb-title page) (clos-display-name (getf entry :name)))
    (gtk:widget-set-tooltip-text (cb-title page) (getf entry :name))
    (gtk:label-set-text (cb-status page) "Loading definition…")
    (gtk:widget-set-sensitive (cb-inspect page) nil)
    (gtk:widget-set-sensitive (cb-source page) nil)
    (clos-render-detail page)
    (rex connection (clos-browser-call "describe-entry" (getf entry :kind) (getf entry :name) (cb-package page))
         :on-ok (lambda (reply)
                  (when (and (clos-page-live-p page connection) (= serial (cb-serial page)))
                    (setf (cb-description page) reply)
                    (when (getf reply :name)
                      (setf (cb-selected page) (list :kind (getf reply :kind) :name (getf reply :name)))
                      (gtk:label-set-text (cb-title page) (clos-display-name (getf reply :name)))
                      (gtk:widget-set-tooltip-text (cb-title page) (getf reply :name))
                      ;; The initial name may have been typed without a package.
                      ;; Match the canonical reply to its sidebar row as well.
                      (when (cb-list page)
                        (setf (cb-updating page) t)
                        (unwind-protect
                             (progn
                               (gtk:list-box-unselect-all (cb-list page))
                               (maphash (lambda (row candidate)
                                          (when (equal candidate (cb-selected page))
                                            (gtk:list-box-select-row (cb-list page) row)))
                                        (cb-rows page)))
                          (setf (cb-updating page) nil))))
                    (gtk:label-set-text (cb-status page) (or (getf reply :error) "Definitions from the connected image"))
                    (gtk:widget-set-sensitive (cb-inspect page) (not (getf reply :error)))
                    (gtk:widget-set-sensitive (cb-source page) (not (getf (parse-location (getf reply :source)) :error)))
                    (clos-render-detail page)
                    (when restore (clos-restore-scroll page restore connection serial))))
         :on-abort (lambda (reason)
                     (when (and (clos-page-live-p page connection) (= serial (cb-serial page)))
                       (gtk:label-set-text (cb-status page) (format nil "Could not browse definition: ~a" reason)))))))

(defun refresh-clos-page (page)
  (let* ((connection *connection*) (serial (incf (cb-catalog-serial page))))
    (incf (cb-serial page))
    (unless (and connection (eq connection (cb-connection page)))
      (setf (cb-history page) nil (cb-pending-restore page) nil))
    (setf (cb-connection page) connection)
    (clos-update-back-button page)
    (gtk:widget-set-sensitive (cb-inspect page) nil)
    (gtk:widget-set-sensitive (cb-source page) nil)
    (unless (connected-p)
      (setf (cb-catalog page) nil (cb-description page) nil)
      (gtk:widget-set-sensitive (cb-inspect page) nil)
      (gtk:widget-set-sensitive (cb-source page) nil)
      (gtk:label-set-text (cb-status page) "No Common Lisp connected. Start or connect a Lisp, then refresh.")
      (clos-fill-list page) (clos-render-detail page)
      (return-from refresh-clos-page))
    (gtk:label-set-text (cb-status page) "Reading the connected image…")
    (labels ((current-p () (and (clos-page-live-p page connection) (= serial (cb-catalog-serial page))))
             (failed (reason) (when (current-p) (gtk:label-set-text (cb-status page) (format nil "Could not browse CLOS: ~a" reason)))))
      (ensure-clos-helper connection
       (lambda ()
         (when (current-p)
           (rex connection (clos-browser-call "catalog" (cb-package page))
                :on-ok (lambda (reply)
                         (when (current-p)
                           (if (getf reply :error) (failed (getf reply :error))
                               (progn
                                 (setf (cb-catalog page) reply (cb-updating page) t)
                                 (unwind-protect
                                      (progn
                                        (gtk:drop-down-set-model (cb-packages page) (gtk:string-list-new (getf reply :packages)))
                                        (gtk:drop-down-set-selected (cb-packages page)
                                         (or (position (cb-package page) (getf reply :packages) :test #'equal) 0)))
                                   (setf (cb-updating page) nil))
                                 (clos-fill-list page)
                                 (gtk:label-set-text (cb-status page) "Definitions from the connected image")
                                 (when (cb-selected page) (clos-select-entry page (cb-selected page) :remember nil))))))
                :on-abort #'failed)))
       #'failed))))

(defun clos-inspect-selected (page)
  (when (and (cb-description page) (clos-page-live-p page (cb-connection page)))
    (inspect-string
     (sexp-to-string (clos-browser-call "object" (getf (cb-selected page) :kind)
                                       (getf (cb-selected page) :name) (cb-package page)))
     "COMMON-LISP-USER" :common-lisp)))

(defun make-clos-widget (page)
  (let* ((packages (gtk:drop-down-new-from-strings (list (cb-package page))))
         (search (make-instance 'gtk:search-entry :placeholder-text "Filter classes and generic functions" :hexpand t))
         (list (make-instance 'gtk:list-box :selection-mode :single :css-classes '("navigation-sidebar")))
         (title (clos-label "CLOS Browser" t))
         (status (clos-caption "Connecting…"))
         (details (make-instance 'gtk:box :orientation :vertical :spacing 8 :margin-start 16 :margin-end 16
                                          :margin-top 12 :margin-bottom 12))
         (inspect (clos-button "Inspect" (lambda () (clos-inspect-selected page))))
         (back (clos-button "Back" (lambda () (clos-go-back page)) "Back to the previous definition (Alt+Left)"))
         (source (clos-button "Go to source" (lambda () (clos-goto-source (getf (cb-description page) :source)))))
         (paned (make-instance 'gtk:paned :orientation :horizontal :position 260
                                         :shrink-start-child t :shrink-end-child t :vexpand t)))
    (setf (cb-packages page) packages (cb-search page) search (cb-list page) list
          (cb-title page) title (cb-status page) status (cb-details page) details
          (cb-inspect page) inspect (cb-source page) source (cb-back page) back)
    (gtk:label-set-wrap title nil)
    (gtk:label-set-ellipsize title :end)
    (gtk:widget-set-hexpand title t)
    (gtk:widget-remove-css-class title "heading")
    (gtk:widget-add-css-class title "title-2")
    (gtk:widget-set-sensitive inspect nil) (gtk:widget-set-sensitive source nil)
    (gtk:widget-set-sensitive back nil)
    (setf (cb-list-scroller page) (make-instance 'gtk:scrolled-window :hscrollbar-policy :never :child list)
          (cb-detail-scroller page) (make-instance 'gtk:scrolled-window :hscrollbar-policy :automatic :vexpand t :child details))
    (gtk:paned-set-start-child paned (cb-list-scroller page))
    (gtk:paned-set-end-child paned
     (gtk:build
       (gtk:box :orientation :vertical
         (sideways-scroller (gtk:build (gtk:box :spacing 8 :margin-start 20 :margin-end 16
                                                :margin-top 16 :margin-bottom 4 title inspect source)))
         (cb-detail-scroller page))))
    (gobject:connect packages "notify::selected"
                     (lambda (dropdown pspec)
                       (declare (ignore pspec))
                       (unless (cb-updating page)
                         (let ((name (nth (gtk:drop-down-get-selected dropdown) (getf (cb-catalog page) :packages))))
                           (when name
                             (clos-remember-current page)
                             (setf (cb-package page) name (cb-selected page) nil (cb-description page) nil)
                             (gtk:label-set-text title "CLOS Browser")
                             (clos-render-detail page)
                             (refresh-clos-page page))))))
    (gobject:connect search :search-changed (lambda (entry) (declare (ignore entry)) (clos-fill-list page)))
    (gobject:connect list :row-selected
                     (lambda (box row) (declare (ignore box))
                       (unless (cb-updating page)
                         (let ((entry (gethash row (cb-rows page))))
                           (when entry (clos-select-entry page entry))))))
    (setf (cb-widget page)
          (gtk:build
            (gtk:box :orientation :vertical
              (sideways-scroller
               (gtk:build (gtk:box :spacing 8 :margin-start 10 :margin-end 10 :margin-top 8 :margin-bottom 8
                           back packages search
                           (gtk:button :label "Refresh" :on-clicked
                            (lambda (b) (declare (ignore b)) (refresh-clos-page page))))))
              (gtk:separator)
              paned
              (gtk:separator)
              (gtk:box :margin-start 12 :margin-end 12 :margin-top 6 :margin-bottom 6 status))))
    (clos-render-detail page)
    (refresh-clos-page page)
    (cb-widget page)))

(defun open-clos-browser (&key name package (kind :class))
  (with-connection (connection :language :common-lisp)
    (let* ((buffer (or (find-if (lambda (b) (eq (buffer-major-mode b) 'clos-browser-mode)) (buffer-list))
                       (let ((buffer (make-buffer :name "CLOS Browser" :text (make-gtk-text) :major-mode 'clos-browser-mode)))
                         (setf (buffer-local buffer :tab-widget)
                               (lambda (view)
                                 (declare (ignore view))
                                 (let ((page (make-clos-page :buffer buffer :package (or package (connection-package connection) "COMMON-LISP-USER")
                                                            :selected (when name (list :kind kind :name name)))))
                                   (push page (buffer-local buffer :clos-pages))
                                   (make-clos-widget page))))
                         buffer)))
           (view (show-buffer *window* buffer))
           (page (find (view-widget view) (buffer-local buffer :clos-pages) :key #'cb-widget)))
      (when page
        (when (and name (not (equal (cb-selected page) (list :kind kind :name name))))
          (clos-remember-current page))
        (when package (setf (cb-package page) package))
        (when name (setf (cb-selected page) (list :kind kind :name name)))
        (refresh-clos-page page)))))

(defun refresh-clos-browsers ()
  (unless (connected-p) (clrhash *clos-helper-states*))
  (dolist (page (clos-pages)) (refresh-clos-page page)))

(add-hook '*image-changed-hook* 'refresh-clos-browsers)

(define-command clos-browser ()
  "Browse classes and generic functions in the connected Common Lisp image."
  (let ((package (and (selected-view *window*) (view-package (selected-view *window*)))))
    (open-clos-browser :package package)))

(define-command browse-class ()
  "Browse the class named at the cursor in the connected Common Lisp image."
  (call-with-symbol-name "Browse class" (lambda (name package) (open-clos-browser :name name :package package))))

(define-command clos-browser-back ()
  "Return to the previous CLOS definition, restoring its detail tab and filter."
  (let* ((view (current-view))
         (page (find (view-widget view) (buffer-local (view-buffer view) :clos-pages) :key #'cb-widget)))
    (when page (clos-go-back page))))

(defun browse-inspector-class (expression thread)
  (with-connection (connection :language :common-lisp)
    (ensure-clos-helper connection
     (lambda ()
       (rex connection (clos-browser-call "class-of-expression" expression) :thread thread
            :on-ok (lambda (reply)
                     (when (eq connection *connection*)
                       (open-clos-browser :name (getf reply :name) :package (getf reply :package))))))
     (lambda (reason) (message "Could not browse class: ~a" reason)))))
