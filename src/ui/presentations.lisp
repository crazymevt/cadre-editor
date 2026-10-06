;;;; presentations.lisp — REPL results you can inspect and reuse
;;;;
;;;; With swank-presentations, the Lisp marks each REPL result with an id
;;;; (:presentation-start id, the text, :presentation-end id) and keeps the
;;;; object. Cadre tags the text so it can be clicked: a click inspects the
;;;; object; right-click offers Inspect, Copy Value, Copy to Input and Open
;;;; in New Tab (the value's text: a string's own characters, anything else
;;;; printed in full, fetched from the Lisp; in a tab whose mode suits it). A
;;;; result copied into the input keeps its identity: when the input is
;;;; sent, it is read as #.(swank:lookup-presented-object-or-lose id), the
;;;; object itself rather than its printed text.

(in-package #:cadre-ui)

(defun repl-presentations () (buffer-local (repl-buffer *repl*) :presentations))

(defun presentation-start (id)
  (when *repl*
    (push (cons id (gtk:text-iter-get-offset (mark-iter (repl-output-mark *repl*))))
          (buffer-local (repl-buffer *repl*) :open-presentations))))

(defun presentation-end (id)
  (when *repl*
    (let* ((buffer (repl-buffer *repl*))
           (open (assoc id (buffer-local buffer :open-presentations)))
           (gtk-buffer (repl-gtk-buffer)))
      (when open
        (setf (buffer-local buffer :open-presentations) (remove open (buffer-local buffer :open-presentations)))
        (let* ((start (iter-at gtk-buffer (cdr open)))
               (end (mark-iter (repl-output-mark *repl*)))
               (start-mark (gtk:text-buffer-create-mark gtk-buffer nil start t))
               (end-mark (gtk:text-buffer-create-mark gtk-buffer nil end t)))
          (ensure-face-tag gtk-buffer "cadre-repl-presentation" :repl-presentation)
          (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-repl-presentation" start end)
          (push (list id start-mark end-mark) (buffer-local buffer :presentations)))))))

(defun presentation-at (offset)
  "The presentation (id start-mark end-mark) whose text is at OFFSET in the REPL, or nil."
  (let ((gtk-buffer (repl-gtk-buffer)))
    (find-if (lambda (p)
               (destructuring-bind (id start end) p
                 (declare (ignore id))
                 (and (not (gtk:text-mark-get-deleted start))
                      (<= (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer start)) offset)
                      (< offset (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer end))))))
             (repl-presentations))))

(defun presentation-text (presentation)
  (destructuring-bind (id start end) presentation
    (declare (ignore id))
    (let ((gtk-buffer (repl-gtk-buffer)))
      (gtk:text-buffer-get-text gtk-buffer (gtk:text-buffer-get-iter-at-mark gtk-buffer start)
                                (gtk:text-buffer-get-iter-at-mark gtk-buffer end) t))))

(defun forget-presentations ()
  "After the REPL is cleared: the presentations' text is gone."
  (when *repl*
    (let ((gtk-buffer (repl-gtk-buffer)))
      (dolist (p (repl-presentations))
        (dolist (mark (rest p))
          (unless (gtk:text-mark-get-deleted mark) (gtk:text-buffer-delete-mark gtk-buffer mark)))))
    (setf (buffer-local (repl-buffer *repl*) :presentations) nil)))

;;; Acting on a presentation

(defun inspect-presentation (presentation)
  (with-connection (connection)
    (rex connection (swank-call "swank:inspect-presentation" (first presentation) t)
         :on-ok (lambda (reply) (show-inspection reply))
         :on-abort (lambda (reason) (declare (ignore reason))
                     (message "That value is gone (the REPL's results were cleared, or the Lisp restarted)")))))

(defun presentation-input-tag (id)
  "The tag marking a copy of presentation ID in the REPL's input."
  (let ((gtk-buffer (repl-gtk-buffer)))
    (or (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) (format nil "cadre-pres-~d" id))
        (let ((tag (make-instance 'gtk:text-tag :name (format nil "cadre-pres-~d" id))))
          (gtk:text-tag-table-add (gtk:text-buffer-get-tag-table gtk-buffer) tag)
          (restyle-tag tag :repl-presentation)
          tag))))

(defun copy-presentation-to-input (presentation)
  "Put PRESENTATION in the input at the cursor (or the end), as the object itself."
  (let* ((gtk-buffer (repl-gtk-buffer))
         (input-start (gtk:text-iter-get-offset (mark-iter (repl-input-mark *repl*))))
         (cursor (gtk:text-iter-get-offset (cursor-iter gtk-buffer)))
         (at (if (>= cursor input-start) cursor (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter gtk-buffer))))
         (text (presentation-text presentation))
         (before (and (> at input-start)
                      (let ((c (char (gtk:text-buffer-get-text gtk-buffer (iter-at gtk-buffer (1- at)) (iter-at gtk-buffer at) t) 0)))
                        (not (or (whitespace-char-p c) (char= c #\())))))
         (text (if before (concatenate 'string " " text) text))
         (start (+ at (if before 1 0))))
    (gtk:text-buffer-insert gtk-buffer (iter-at gtk-buffer at) text -1)
    (gtk:text-buffer-apply-tag gtk-buffer (presentation-input-tag (first presentation))
                               (iter-at gtk-buffer start) (iter-at gtk-buffer (+ at (length text))))
    (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (+ at (length text))))
    (let ((view (repl-view))) (when view (focus-view view)))))

(defun repl-input-for-lisp ()
  "The REPL's input as the Lisp should read it: copied presentations become
#.(swank:lookup-presented-object-or-lose id)."
  (let* ((gtk-buffer (repl-gtk-buffer))
         (start (gtk:text-iter-get-offset (mark-iter (repl-input-mark *repl*))))
         (end (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter gtk-buffer))))
    (with-output-to-string (out)
      (loop with i = start
            while (< i end)
            do (let* ((iter (iter-at gtk-buffer i))
                      (id (loop for tag in (gtk:text-iter-get-tags iter)
                                for name = (gobject:property tag :name)
                                when (and name (> (length name) 11) (string= "cadre-pres-" name :end2 11))
                                  return (parse-integer name :start 11 :junk-allowed t))))
                 (if id
                     (let ((run-end (loop for j from i below end
                                          while (gtk:text-iter-has-tag (iter-at gtk-buffer j) (presentation-input-tag id))
                                          finally (return j))))
                       (format out "#.(swank:lookup-presented-object-or-lose ~d)" id)
                       (setf i run-end))
                     (progn (write-string (gtk:text-buffer-get-text gtk-buffer iter (iter-at gtk-buffer (1+ i)) t) out)
                            (incf i))))))))

;;; A value's text, for copying or a new tab

(defun value-text-form (expression)
  "Source that writes the value of EXPRESSION (source) as text: a string's
characters, or anything else printed in full."
  (format nil "(let ((o ~a)) (write-string (if (stringp o) o (let ((*print-length* nil) (*print-level* nil) (*print-lines* nil) (*print-circle* t)) (prin1-to-string o)))) (values))"
          expression))

(defun fetch-value-text (expression on-text &key (thread t))
  "Ask the Lisp for the text of EXPRESSION's value (value-text-form), then
call ON-TEXT with it."
  (with-connection (connection)
    (rex connection (swank-call "swank:eval-and-grab-output" (value-text-form expression)) :thread thread
         :on-ok (lambda (reply) (funcall on-text (first reply)))
         :on-abort (lambda (reason) (declare (ignore reason))
                     (message "That value is gone (the REPL's results were cleared, or the Lisp restarted)")))))

(defun copy-value-text (expression &key (thread t))
  (fetch-value-text expression
                    (lambda (text)
                      (gdk:clipboard-set-text (gdk:display-get-clipboard (gdk:display-get-default)) text)
                      (message "Copied ~:d character~:p" (length text)))
                    :thread thread))

(defun open-text-in-new-tab (text &key (name "value"))
  "Show TEXT in a new, unsaved tab, in the mode its start suggests (XML, JSON, Lisp…)."
  (let ((buffer (make-buffer :name name :text (make-gtk-text) :major-mode (major-mode-for-text text))))
    (text-replace-contents (buffer-text buffer) text)
    (show-buffer *window* buffer)
    buffer))

(defun open-value-in-new-tab (expression &key (thread t))
  (fetch-value-text expression (lambda (text) (open-text-in-new-tab text)) :thread thread))

(defun presentation-expression (presentation)
  (format nil "(swank:lookup-presented-object-or-lose ~d)" (first presentation)))

;;; Clicking

(defun repl-offset-at (view x y)
  (let ((text-view (view-text-view view)))
    (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
      (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
        (and ok (gtk:text-iter-get-offset iter))))))

(defun show-popover-menu (widget x y items)
  "A menu at (X, Y) in WIDGET of ITEMS, (label . function) each."
  (let* ((box (make-instance 'gtk:box :orientation :vertical))
         (popover (make-instance 'gtk:popover :child box :has-arrow nil :css-classes '("menu"))))
    (loop for (label . action) in items
          do (let ((button (make-instance 'gtk:button :label label :css-classes '("flat")))
                   (action action))
               (gtk:widget-set-halign (gtk:button-get-child button) :start)
               (gobject:connect button :clicked (lambda (b) (declare (ignore b))
                                                  (gtk:popover-popdown popover)
                                                  (handler-case (funcall action)
                                                    (editor-error (e) (message "~a" (editor-error-message e))))))
               (gtk:box-append box button)))
    (gtk:widget-set-parent popover widget)
    (gtk:popover-set-pointing-to popover (gdk:make-rectangle :x (round x) :y (round y) :width 1 :height 1))
    (gobject:connect popover :closed (lambda (p)
                                       (glib:idle-add glib:+priority-default-idle+
                                                      (lambda () (gtk:widget-unparent p) nil))))
    (gtk:popover-popup popover)
    popover))

(defun presentation-menu-items (view presentation)
  (flet ((in-repl (function)
           (lambda () (let ((*repl* (buffer-local (view-buffer view) :repl))) (funcall function)))))
    (list (cons "Inspect" (in-repl (lambda () (inspect-presentation presentation))))
          (cons "Copy Value" (in-repl (lambda () (copy-value-text (presentation-expression presentation)))))
          (cons "Copy to Input" (in-repl (lambda () (copy-presentation-to-input presentation))))
          (cons "Open in New Tab" (in-repl (lambda () (open-value-in-new-tab (presentation-expression presentation))))))))

(defun show-presentation-menu (view presentation x y)
  (show-popover-menu (view-text-view view) x y (presentation-menu-items view presentation)))

(defun setup-presentation-clicks (view)
  "In VIEW (a REPL's), clicking a result inspects it; right-clicking offers a menu."
  (let ((text-view (view-text-view view)))
    (flet ((presentation-under (x y)
             (let ((*repl* (buffer-local (view-buffer view) :repl))
                   (offset (repl-offset-at view x y)))
               (and *repl* offset (presentation-at offset)))))
      (let ((click (gtk:gesture-click-new)))
        (gtk:gesture-single-set-button click 1)
        (gobject:connect click :released
                         (lambda (gesture n x y)
                           (declare (ignore gesture))
                           (let ((presentation (and (= n 1) (presentation-under x y))))
                             (when (and presentation
                                        (not (gtk:text-buffer-get-has-selection (view-gtk-buffer view))))
                               (let ((*repl* (buffer-local (view-buffer view) :repl)))
                                 (inspect-presentation presentation))))))
        (gtk:widget-add-controller text-view click))
      (let ((menu (gtk:gesture-click-new)))
        (gtk:gesture-single-set-button menu 3)
        (gtk:event-controller-set-propagation-phase menu :capture)
        (gobject:connect menu :pressed
                         (lambda (gesture n x y)
                           (declare (ignore n))
                           (let ((presentation (presentation-under x y)))
                             (when presentation
                               (gtk:gesture-set-state gesture :claimed)
                               (show-presentation-menu view presentation x y)))))
        (gtk:widget-add-controller text-view menu))
      (let ((motion (gtk:event-controller-motion-new)))
        (gobject:connect motion :motion
                         (lambda (controller x y)
                           (declare (ignore controller))
                           (gtk:widget-set-cursor-from-name text-view (if (presentation-under x y) "pointer" "text"))))
        (gtk:widget-add-controller text-view motion)))))
