;;;; search.lisp — the find bar: find in the current buffer as you type
;;;;
;;;; Every match is highlighted; the current one more strongly. Enter goes
;;;; to the next match, Shift+Enter to the previous one, Escape closes the
;;;; bar and leaves the current match selected. Matching ignores case unless
;;;; the text typed has an upper-case letter.

(in-package #:cadre-ui)

(defclass find-bar ()
  ((bar :reader find-bar-widget)
   (entry :reader find-bar-entry)
   (status :reader find-bar-status)
   (matches :initform #() :accessor find-bar-matches
            :documentation "Start and end offsets of each match, as conses.")
   (current :initform nil :accessor find-bar-current)
   (buffer :initform nil :accessor find-bar-buffer)))

(defparameter *max-search-matches* 10000)

(defun make-find-bar ()
  (let* ((fb (make-instance 'find-bar))
         (entry (make-instance 'gtk:search-entry :search-delay 0 :hexpand t
                                                 :placeholder-text "Find"))
         (status (make-instance 'gtk:label :css-classes '("dim-label") :ellipsize :end
                                           :width-chars 4 :max-width-chars 12)))
    (setf (slot-value fb 'entry) entry
          (slot-value fb 'status) status
          (slot-value fb 'bar)
          (gtk:build
            (gtk:search-bar :show-close-button t
              (gtk:box :spacing 6 :width-request 120 :hexpand t
                entry
                (gtk:button :icon-name "go-up-symbolic" :tooltip-text "Previous match"
                            :on-clicked (lambda (b) (declare (ignore b)) (find-step fb -1)))
                (gtk:button :icon-name "go-down-symbolic" :tooltip-text "Next match"
                            :on-clicked (lambda (b) (declare (ignore b)) (find-step fb 1)))
                status))))
    (gtk:search-bar-connect-entry (find-bar-widget fb) entry)
    (gobject:connect entry :search-changed (lambda (e) (declare (ignore e)) (find-update fb)))
    (gobject:connect entry :activate (lambda (e) (declare (ignore e)) (find-step fb 1)))
    (gobject:connect entry :stop-search (lambda (e) (declare (ignore e)) (find-close fb)))
    (let ((keys (gtk:event-controller-key-new)))
      (gobject:connect keys :key-pressed
                       (lambda (c keyval keycode state)
                         (declare (ignore c keycode))
                         (cond ((and (member (gdk:keyval-name keyval) '("Return" "KP_Enter") :test #'string=)
                                     (member :shift-mask (modifier-list state)))
                                (find-step fb -1) t)
                               (t nil))))
      (gtk:widget-add-controller entry keys))
    (gobject:connect (find-bar-widget fb) "notify::search-mode-enabled"
                     (lambda (bar pspec) (declare (ignore pspec))
                       (unless (gtk:search-bar-get-search-mode bar)
                         (clear-matches fb))))
    fb))

(defun find-bar-open-p (fb)
  (gtk:search-bar-get-search-mode (find-bar-widget fb)))

(defun clear-matches (fb)
  (let ((buffer (find-bar-buffer fb)))
    (when (and buffer (member buffer (buffer-list)))
      (let ((gtk-buffer (buffer-text buffer)))
        (dolist (face '(:search :search-current))
          (let ((tag (face-tag gtk-buffer face)))
            (when tag
              (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                          (gtk:text-buffer-get-end-iter gtk-buffer))))))))
  (setf (find-bar-matches fb) #() (find-bar-current fb) nil (find-bar-buffer fb) nil))

(defun find-matches (gtk-buffer pattern)
  "Start and end offsets of each occurrence of PATTERN in GTK-BUFFER."
  (let ((flags (if (some #'upper-case-p pattern) '(:text-only) '(:text-only :case-insensitive)))
        (iter (gtk:text-buffer-get-start-iter gtk-buffer))
        (matches '()))
    (loop repeat *max-search-matches*
          do (multiple-value-bind (found start end) (gtk:text-iter-forward-search iter pattern flags nil)
               (unless found (return))
               (push (cons (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end)) matches)
               (setf iter end)
               (when (gtk:text-iter-equal start end) (return))))
    (coerce (nreverse matches) 'vector)))

(defun find-update (fb)
  "Search again for the entry's text in the current buffer."
  (clear-matches fb)
  (let ((view (and *window* (selected-view *window*)))
        (pattern (gtk:editable-get-text (find-bar-entry fb))))
    (cond
      ((or (null view) (string= pattern ""))
       (gtk:label-set-text (find-bar-status fb) ""))
      (t
       (let* ((buffer (view-buffer view))
              (gtk-buffer (buffer-text buffer))
              (matches (find-matches gtk-buffer pattern))
              (tag (face-tag gtk-buffer :search)))
         (setf (find-bar-buffer fb) buffer
               (find-bar-matches fb) matches)
         (loop for (start . end) across matches
               do (gtk:text-buffer-apply-tag gtk-buffer tag (iter-at gtk-buffer start) (iter-at gtk-buffer end)))
         ;; The current match: the first at or after the cursor.
         (let* ((cursor (text-point gtk-buffer))
                (index (or (position-if (lambda (m) (>= (car m) cursor)) matches)
                           (and (plusp (length matches)) 0))))
           (select-match fb index)))))))

(defun select-match (fb index)
  (let* ((matches (find-bar-matches fb))
         (buffer (find-bar-buffer fb))
         (view (and *window* (selected-view *window*))))
    (setf (find-bar-current fb) index)
    (gtk:label-set-text (find-bar-status fb)
                        (cond ((zerop (length matches)) "No matches")
                              (t (format nil "~d of ~d" (1+ index) (length matches)))))
    (when (and buffer index view (eq (view-buffer view) buffer))
      (let* ((gtk-buffer (buffer-text buffer))
             (tag (face-tag gtk-buffer :search-current))
             (match (aref matches index)))
        (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                    (gtk:text-buffer-get-end-iter gtk-buffer))
        (gtk:text-buffer-apply-tag gtk-buffer tag (iter-at gtk-buffer (car match)) (iter-at gtk-buffer (cdr match)))
        (gtk:text-view-scroll-to-iter (view-text-view view) (iter-at gtk-buffer (car match))
                                      0.2d0 nil 0d0 0d0)))))

(defun find-step (fb delta)
  (let ((n (length (find-bar-matches fb))))
    (if (zerop n)
        (find-update fb)
        (select-match fb (mod (+ (or (find-bar-current fb) -1) delta) n)))))

(defun find-open (fb)
  (gtk:search-bar-set-search-mode (find-bar-widget fb) t)
  ;; Start from the selection, if there is a short one on one line.
  (let ((view (and *window* (selected-view *window*))))
    (when view
      (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
        (when has
          (let ((text (gtk:text-buffer-get-text (view-gtk-buffer view) start end nil)))
            (when (and (< (length text) 200) (not (find #\Newline text)))
              (gtk:editable-set-text (find-bar-entry fb) text)))))))
  (gtk:widget-grab-focus (find-bar-entry fb))
  (gtk:editable-select-region (find-bar-entry fb) 0 -1)
  (find-update fb))

(defun find-close (fb)
  "Close the bar, leaving the current match selected in the editor."
  (let ((buffer (find-bar-buffer fb))
        (index (find-bar-current fb))
        (view (and *window* (selected-view *window*))))
    (when (and buffer index view (eq (view-buffer view) buffer)
               (< index (length (find-bar-matches fb))))
      (let ((match (aref (find-bar-matches fb) index))
            (gtk-buffer (buffer-text buffer)))
        (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer (cdr match)) (iter-at gtk-buffer (car match)))))
    (gtk:search-bar-set-search-mode (find-bar-widget fb) nil)
    (when view (focus-view view))))
