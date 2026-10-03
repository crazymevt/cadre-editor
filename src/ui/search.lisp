;;;; search.lisp — the find bar: find and replace in the current buffer as
;;;; you type, and Emacs's incremental search and query-replace
;;;;
;;;; Every match is highlighted; the current one more strongly. Matching
;;;; ignores case unless the text typed has an upper-case letter.
;;;;
;;;; The bar works two ways. Opened with Find (Ctrl+F), Enter goes to the
;;;; next match and Shift+Enter to the previous one; the editor's cursor
;;;; stays put until the bar closes. Opened with isearch (C-s, C-r), the
;;;; cursor follows the current match: C-s and C-r move to the next and
;;;; previous, RET (or any other command) ends the search there with the
;;;; mark where the search began, and C-g goes back to where it began.

(in-package #:cadre-ui)

(defclass find-bar ()
  ((bar :reader find-bar-widget)
   (entry :reader find-bar-entry)
   (status :reader find-bar-status)
   (replace-row :reader find-bar-replace-row)
   (replace-entry :reader find-bar-replace-entry)
   (matches :initform #() :accessor find-bar-matches
            :documentation "Start and end offsets of each match, as conses.")
   (current :initform nil :accessor find-bar-current)
   (buffer :initform nil :accessor find-bar-buffer)
   (mode :initform :find :accessor find-bar-mode :documentation ":find or :isearch.")
   (direction :initform 1 :accessor find-bar-direction)
   (origin :initform nil :accessor find-bar-origin
           :documentation "In isearch, the cursor offset where the search began.")))

(defparameter *max-search-matches* 10000)
(defvar *last-search* "" "The last text searched for, for C-s C-s.")

(defun make-find-bar ()
  (let* ((fb (make-instance 'find-bar))
         (entry (make-instance 'gtk:search-entry :search-delay 0 :hexpand t
                                                 :placeholder-text "Find"))
         (replace-entry (make-instance 'gtk:entry :hexpand t :placeholder-text "Replace"))
         (status (make-instance 'gtk:label :css-classes '("dim-label") :ellipsize :end
                                           :width-chars 4 :max-width-chars 12))
         (replace-row (gtk:build
                        (gtk:revealer :reveal-child nil :transition-type :slide-down
                          (gtk:box :spacing 6 :margin-top 4
                            replace-entry
                            (gtk:button :label "Replace" :tooltip-text "Replace this match and go to the next"
                                        :on-clicked (lambda (b) (declare (ignore b)) (find-replace-one fb)))
                            (gtk:button :label "All" :tooltip-text "Replace every match"
                                        :on-clicked (lambda (b) (declare (ignore b)) (find-replace-all fb))))))))
    (setf (slot-value fb 'entry) entry
          (slot-value fb 'status) status
          (slot-value fb 'replace-entry) replace-entry
          (slot-value fb 'replace-row) replace-row
          (slot-value fb 'bar)
          (gtk:build
            (gtk:search-bar :show-close-button t
              (gtk:box :orientation :vertical :width-request 120 :hexpand t
                (gtk:box :spacing 6 :hexpand t
                  entry
                  (gtk:button :icon-name "go-up-symbolic" :tooltip-text "Previous match"
                              :on-clicked (lambda (b) (declare (ignore b)) (find-step fb -1)))
                  (gtk:button :icon-name "go-down-symbolic" :tooltip-text "Next match"
                              :on-clicked (lambda (b) (declare (ignore b)) (find-step fb 1)))
                  (gtk:toggle-button :icon-name "cadre-replace-symbolic" :tooltip-text "Replace"
                                     :css-classes '("flat")
                                     :on-clicked (lambda (b)
                                                   (gtk:revealer-set-reveal-child
                                                    replace-row (gtk:toggle-button-get-active b))))
                  status)
                replace-row))))
    (gtk:search-bar-connect-entry (find-bar-widget fb) entry)
    (gobject:connect entry :search-changed (lambda (e) (declare (ignore e)) (find-update fb)))
    (gobject:connect entry :activate (lambda (e) (declare (ignore e))
                                       (if (eq (find-bar-mode fb) :isearch)
                                           (isearch-exit)
                                           (find-step fb 1))))
    (gobject:connect entry :stop-search (lambda (e) (declare (ignore e))
                                          (if (eq (find-bar-mode fb) :isearch)
                                              (isearch-abort)
                                              (find-close fb))))
    (gobject:connect replace-entry :activate (lambda (e) (declare (ignore e)) (find-replace-one fb)))
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

(defun clear-search-tags (gtk-buffer)
  (dolist (face '(:search :search-current))
    (let ((tag (face-tag gtk-buffer face)))
      (when tag
        (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                    (gtk:text-buffer-get-end-iter gtk-buffer))))))

(defun clear-matches (fb)
  (let ((buffer (find-bar-buffer fb)))
    (when (and buffer (member buffer (buffer-list)))
      (clear-search-tags (buffer-text buffer))))
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
       (gtk:label-set-text (find-bar-status fb) "")
       (when (and view (eq (find-bar-mode fb) :isearch) (find-bar-origin fb))
         (set-point view (find-bar-origin fb))))
      (t
       (let* ((buffer (view-buffer view))
              (gtk-buffer (buffer-text buffer))
              (matches (find-matches gtk-buffer pattern))
              (tag (face-tag gtk-buffer :search)))
         (setf (find-bar-buffer fb) buffer
               (find-bar-matches fb) matches)
         (loop for (start . end) across matches
               do (gtk:text-buffer-apply-tag gtk-buffer tag (iter-at gtk-buffer start) (iter-at gtk-buffer end)))
         ;; The current match: the first after the cursor (in isearch, after
         ;; where the search began, or the last before it searching backwards).
         (let* ((from (if (eq (find-bar-mode fb) :isearch) (find-bar-origin fb) (text-point gtk-buffer)))
                (index (if (and (eq (find-bar-mode fb) :isearch) (minusp (find-bar-direction fb)))
                           (or (position-if (lambda (m) (<= (cdr m) from)) matches :from-end t)
                               (and (plusp (length matches)) (1- (length matches))))
                           (or (position-if (lambda (m) (>= (car m) from)) matches)
                               (and (plusp (length matches)) 0)))))
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
        (if (eq (find-bar-mode fb) :isearch)
            (progn
              (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (if (minusp (find-bar-direction fb))
                                                                                  (car match)
                                                                                  (cdr match))))
              (scroll-to-cursor view))
            (gtk:text-view-scroll-to-iter (view-text-view view) (iter-at gtk-buffer (car match))
                                          0.2d0 nil 0d0 0d0))))))

(defun find-step (fb delta)
  (let ((n (length (find-bar-matches fb))))
    (setf (find-bar-direction fb) (if (minusp delta) -1 1))
    (if (zerop n)
        (find-update fb)
        (select-match fb (mod (+ (or (find-bar-current fb) -1) delta) n)))))

(defun find-open (fb &key (mode :find) (direction 1) replace)
  (let ((view (and *window* (selected-view *window*))))
    (setf (find-bar-mode fb) mode
          (find-bar-direction fb) direction
          (find-bar-origin fb) (and view (point-offset view)))
    (gtk:revealer-set-reveal-child (find-bar-replace-row fb) (and replace t))
    (gtk:search-bar-set-search-mode (find-bar-widget fb) t)
    ;; Find starts from the selection, if there is a short one on one line;
    ;; isearch starts empty, as in Emacs.
    (when view
      (if (eq mode :isearch)
          (gtk:editable-set-text (find-bar-entry fb) "")
          (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
            (when has
              (let ((text (gtk:text-buffer-get-text (view-gtk-buffer view) start end nil)))
                (when (and (< (length text) 200) (not (find #\Newline text)))
                  (gtk:editable-set-text (find-bar-entry fb) text))))))))
  (gtk:widget-grab-focus (find-bar-entry fb))
  (gtk:editable-select-region (find-bar-entry fb) 0 -1)
  (find-update fb))

(defun find-close (fb &key (select t))
  "Close the bar. With SELECT, leave the current match selected in the editor."
  (let ((buffer (find-bar-buffer fb))
        (index (find-bar-current fb))
        (view (and *window* (selected-view *window*))))
    (let ((pattern (gtk:editable-get-text (find-bar-entry fb))))
      (when (plusp (length pattern)) (setf *last-search* pattern)))
    (when (and select buffer index view (eq (view-buffer view) buffer)
               (< index (length (find-bar-matches fb))))
      (let ((match (aref (find-bar-matches fb) index))
            (gtk-buffer (buffer-text buffer)))
        (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer (cdr match)) (iter-at gtk-buffer (car match)))))
    ;; Closing the bar clears its entry, which searches again: not as isearch,
    ;; which would move the cursor back.
    (setf (find-bar-mode fb) :find)
    (gtk:search-bar-set-search-mode (find-bar-widget fb) nil)
    (when view (focus-view view))))

;;; Replacing from the bar

(defun find-replace-one (fb)
  "Replace the current match, then go to the next."
  (let ((index (find-bar-current fb))
        (buffer (find-bar-buffer fb))
        (pattern (gtk:editable-get-text (find-bar-entry fb))))
    (if (or (null index) (null buffer) (>= index (length (find-bar-matches fb))))
        (find-update fb)
        (let* ((gtk-buffer (buffer-text buffer))
               (match (aref (find-bar-matches fb) index))
               (replacement (replacement-for (text-string gtk-buffer (car match) (cdr match))
                                             (gtk:editable-get-text (find-bar-replace-entry fb))
                                             :case-fold (case-fold-p pattern))))
          (replace-text-between gtk-buffer (car match) (cdr match) replacement)
          (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (+ (car match) (length replacement))))
          (find-update fb)))))

(defun find-replace-all (fb)
  (let* ((view (and *window* (selected-view *window*)))
         (pattern (gtk:editable-get-text (find-bar-entry fb))))
    (when (and view (plusp (length pattern)))
      (let* ((gtk-buffer (view-gtk-buffer view))
             (matches (find-matches gtk-buffer pattern))
             (to (gtk:editable-get-text (find-bar-replace-entry fb))))
        (with-user-action (gtk-buffer)
          (loop for i from (1- (length matches)) downto 0
                for (start . end) = (aref matches i)
                do (replace-text-between gtk-buffer start end
                                         (replacement-for (text-string gtk-buffer start end) to
                                                          :case-fold (case-fold-p pattern)))))
        (find-update fb)
        (message "Replaced ~d occurrence~:p" (length matches))))))

;;; Incremental search (Emacs)

(defparameter *isearch-commands* '(isearch-forward isearch-backward keyboard-quit find-next find-previous
                                   query-replace)
  "Commands that work inside an incremental search; any other ends it first.")

(defun isearch-active-p ()
  (and *window*
       (let ((fb (window-find-bar *window*)))
         (and (find-bar-open-p fb) (eq (find-bar-mode fb) :isearch)))))

(defun isearch (direction)
  (let ((fb (window-find-bar *window*)))
    (if (isearch-active-p)
        (if (string= (gtk:editable-get-text (find-bar-entry fb)) "")
            ;; C-s C-s: search again for the last search.
            (progn (setf (find-bar-direction fb) direction)
                   (gtk:editable-set-text (find-bar-entry fb) *last-search*)
                   (gtk:editable-set-position (find-bar-entry fb) -1))
            (find-step fb direction))
        (progn (current-tab-view)
               (find-open fb :mode :isearch :direction direction)))))

(define-command isearch-forward ()
  "Search forward as you type. C-s again goes to the next match; RET ends the search there, C-g goes back."
  (isearch 1))

(define-command isearch-backward ()
  "Search backward as you type. C-r again goes to the previous match."
  (isearch -1))

(defun isearch-exit ()
  "End the incremental search at the current match, with the mark where it began."
  (let* ((fb (window-find-bar *window*))
         (origin (find-bar-origin fb))
         (view (selected-view *window*)))
    (find-close fb :select nil)
    (when (and view origin (/= origin (point-offset view)))
      (push-mark (view-buffer view) origin)
      (message "Mark saved where search started"))))

(defun isearch-abort ()
  "End the incremental search, back where it began."
  (let* ((fb (window-find-bar *window*))
         (origin (find-bar-origin fb))
         (view (selected-view *window*)))
    (find-close fb :select nil)
    (when (and view origin) (set-point view origin))))

;;; Query-replace (Emacs M-%)

(defstruct (query-replace (:conc-name qr-))
  view from to position end-mark (count 0) case-fold)

(defvar *query-replace* nil "The query-replace in progress, or nil.")

(defun qr-text (qr) (view-gtk-buffer (qr-view qr)))

(defun qr-next-match (qr)
  "The next match at or after the query-replace's position, as (start . end), or nil."
  (let* ((gtk-buffer (qr-text qr))
         (end (if (qr-end-mark qr)
                  (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer (qr-end-mark qr)))
                  (text-length gtk-buffer)))
         (string (text-string gtk-buffer (min (qr-position qr) end) end))
         (match (first (find-all (qr-from qr) string :case-fold (qr-case-fold qr)))))
    (and match (cons (+ (qr-position qr) (car match)) (+ (qr-position qr) (cdr match))))))

(defun qr-show (qr)
  "Highlight the next match and ask about it, or finish if there is none."
  (let ((match (qr-next-match qr))
        (gtk-buffer (qr-text qr)))
    (clear-search-tags gtk-buffer)
    (if (null match)
        (query-replace-finish)
        (progn
          (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer :search-current)
                                     (iter-at gtk-buffer (car match)) (iter-at gtk-buffer (cdr match)))
          (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (cdr match)))
          (scroll-to-cursor (qr-view qr))
          (message "Replace ~a with ~a?  y: yes  n: no  !: all  .: this one and stop  q: stop"
                   (qr-from qr) (qr-to qr))))
    match))

(defun qr-replace (qr match)
  (let* ((gtk-buffer (qr-text qr))
         (replacement (replacement-for (text-string gtk-buffer (car match) (cdr match)) (qr-to qr)
                                       :case-fold (qr-case-fold qr))))
    (replace-text-between gtk-buffer (car match) (cdr match) replacement)
    (incf (qr-count qr))
    (setf (qr-position qr) (+ (car match) (length replacement)))))

(defun query-replace-key (key)
  "The key reader while query-replace asks about a match."
  (let* ((qr *query-replace*)
         (match (and qr (qr-next-match qr))))
    (cond ((null match) (query-replace-finish) nil)
          ((member key '("y" "SPC") :test #'string=) (qr-replace qr match) (qr-show qr) t)
          ((member key '("n" "DEL") :test #'string=) (setf (qr-position qr) (cdr match)) (qr-show qr) t)
          ((string= key "!")
           (with-user-action ((qr-text qr))
             (loop for m = (qr-next-match qr) while m do (qr-replace qr m)))
           (query-replace-finish) t)
          ((string= key ".") (qr-replace qr match) (query-replace-finish) t)
          ((member key '("q" "RET" "ESC") :test #'string=) (query-replace-finish) t)
          ;; Anything else ends query-replace and does what it usually does.
          (t (query-replace-finish) nil))))

(defun query-replace-finish ()
  (let ((qr *query-replace*))
    (setf *query-replace* nil
          *key-reader* nil)
    (when qr
      (clear-search-tags (qr-text qr))
      (when (qr-end-mark qr) (gtk:text-buffer-delete-mark (qr-text qr) (qr-end-mark qr)))
      (message "Replaced ~d occurrence~:p" (qr-count qr)))))

(defun start-query-replace (view from to)
  (when (string= from "") (editor-error "Nothing to replace"))
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (let ((qr (make-query-replace :view view :from from :to to :case-fold (case-fold-p from)
                                    :position (if has (gtk:text-iter-get-offset start) (point-offset view))
                                    :end-mark (and has (gtk:text-buffer-create-mark gtk-buffer nil end nil)))))
        (setf (buffer-local (view-buffer view) :mark-active) nil
              *query-replace* qr
              *key-reader* 'query-replace-key)
        (focus-view view)
        (qr-show qr)))))

(define-command query-replace ()
  "Replace a string with another, asking about each match after the cursor
(or in the selection): y or SPC replaces, n or DEL skips, ! replaces the
rest, . replaces this one and stops, q or RET stops."
  (let* ((view (current-tab-view))
         (fb (window-find-bar *window*))
         (searching (and (find-bar-open-p fb) (gtk:editable-get-text (find-bar-entry fb)))))
    (when (find-bar-open-p fb) (find-close fb :select nil))
    (flet ((ask-to (from)
             (open-picker (window-picker *window*)
                          :placeholder (format nil "Replace ~a with:" from)
                          :on-choose (lambda (to) (start-query-replace view from to)))))
      (if (and searching (plusp (length searching)))
          (ask-to searching)
          (open-picker (window-picker *window*)
                       :placeholder "Query replace:" :text *last-search*
                       :on-choose (lambda (from)
                                    (setf *last-search* from)
                                    ;; The picker closes after this returns; open it again next.
                                    (glib:idle-add glib:+priority-default-idle+
                                                   (lambda () (ask-to from) nil))))))))

(define-command replace-string ()
  "Replace every match of a string after the cursor (or in the selection) with another."
  (let ((view (current-tab-view)))
    (open-picker (window-picker *window*)
                 :placeholder "Replace string:" :text *last-search*
                 :on-choose
                 (lambda (from)
                   (glib:idle-add glib:+priority-default-idle+
                                  (lambda ()
                                    (open-picker (window-picker *window*)
                                                 :placeholder (format nil "Replace ~a with:" from)
                                                 :on-choose (lambda (to)
                                                              (start-query-replace view from to)
                                                              (query-replace-key "!")))
                                    nil))))))

(define-command find-replace ()
  "Find and replace in the current buffer, from the find bar."
  (current-tab-view)
  (find-open (window-find-bar *window*) :replace t))
