;;;; inspector.lisp — the Inspector page
;;;;
;;;; Shows an object from the Lisp as swank's inspector describes it: a
;;;; title and text in which values and actions are links. Clicking a value
;;;; inspects it; clicking an action runs it and shows the object again.
;;;; Back and Forward walk the objects inspected.

(in-package #:cadre-ui)

(defstruct (inspector (:conc-name ins-))
  text-view title back forward
  (links '())                           ; (start end kind index), offsets into the text
  (next 0) (more nil)
  (thread t))                           ; the thread to ask: a debugger's, for frame values

(defvar *inspector* nil)

(defun inspector-buffer ()
  (gtk:text-view-get-buffer (ins-text-view *inspector*)))

(defun make-inspector-widget ()
  (let* ((text-view (make-instance 'gtk:text-view :editable nil :cursor-visible nil :monospace t
                                                  :wrap-mode :word-char :left-margin 10 :right-margin 10
                                                  :top-margin 6 :bottom-margin 6))
         (title (make-instance 'gtk:label :xalign 0.0 :hexpand t :ellipsize :middle :selectable t
                                          :css-classes '("heading")))
         (back (make-instance 'gtk:button :icon-name "cadre-go-back-symbolic" :tooltip-text "Back"
                                          :sensitive nil :css-classes '("flat")))
         (forward (make-instance 'gtk:button :icon-name "cadre-go-forward-symbolic" :tooltip-text "Forward"
                                             :sensitive nil :css-classes '("flat")))
         (refresh (make-instance 'gtk:button :icon-name "view-refresh-symbolic"
                                             :tooltip-text "Inspect the object again" :css-classes '("flat")))
         (gtk-buffer (gtk:text-view-get-buffer text-view)))
    (setf *inspector* (make-inspector :text-view text-view :title title :back back :forward forward))
    (gtk:text-tag-table-add (gtk:text-buffer-get-tag-table gtk-buffer)
                            (make-instance 'gtk:text-tag :name "cadre-inspector-value" :foreground "#2b6cb0"))
    (gtk:text-tag-table-add (gtk:text-buffer-get-tag-table gtk-buffer)
                            (make-instance 'gtk:text-tag :name "cadre-inspector-action" :foreground "#a3299e"
                                                         :underline :single))
    (gtk:text-tag-table-add (gtk:text-buffer-get-tag-table gtk-buffer)
                            (make-instance 'gtk:text-tag :name "cadre-inspector-label" :weight 700))
    (style-inspector-tags)
    (gobject:connect back :clicked (lambda (b) (declare (ignore b)) (call-command 'inspector-back)))
    (gobject:connect forward :clicked (lambda (b) (declare (ignore b)) (call-command 'inspector-forward)))
    (gobject:connect refresh :clicked (lambda (b) (declare (ignore b)) (call-command 'inspector-refresh)))
    (let ((click (gtk:gesture-click-new))
          (motion (gtk:event-controller-motion-new)))
      (gobject:connect click :released
                       (lambda (gesture n x y)
                         (declare (ignore gesture n))
                         (let ((link (inspector-link-at x y)))
                           (when link
                             (handler-case (follow-inspector-link link)
                               (editor-error (e) (message "~a" (editor-error-message e))))))))
      (gobject:connect motion :motion
                       (lambda (controller x y)
                         (declare (ignore controller))
                         (gtk:widget-set-cursor-from-name text-view (if (inspector-link-at x y) "pointer" "text"))))
      (gtk:widget-add-controller text-view click)
      (gtk:widget-add-controller text-view motion))
    (inspector-show-empty)
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :spacing 4 :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2
          back forward refresh title)
        (gtk:separator)
        (gtk:scrolled-window :vexpand t :child text-view)))))

(defun style-inspector-tags ()
  (when *inspector*
    (let ((table (gtk:text-buffer-get-tag-table (inspector-buffer)))
          (dark (adw:dark-p)))
      (setf (gobject:property (gtk:text-tag-table-lookup table "cadre-inspector-value") :foreground)
            (if dark "#61afef" "#1d5fb8")
            (gobject:property (gtk:text-tag-table-lookup table "cadre-inspector-action") :foreground)
            (if dark "#e386d8" "#a3299e")))))

(defun inspector-show-empty ()
  (gtk:label-set-text (ins-title *inspector*) "Nothing inspected")
  (text-replace-contents (inspector-buffer)
                         (format nil "Inspect a value with the Inspect command, by clicking a local in the debugger, ~
                                      or from Lisp code with (swank:inspect-in-emacs object).")))

(defun inspector-link-at (x y)
  (let ((text-view (ins-text-view *inspector*)))
    (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
      (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
        (when ok
          (let ((offset (gtk:text-iter-get-offset iter)))
            (find-if (lambda (link) (and (<= (first link) offset) (< offset (second link))))
                     (ins-links *inspector*))))))))

(defun insert-segments (segments)
  "Add SEGMENTS (from inspector-segments) at the end of the inspector's text."
  (let ((gtk-buffer (inspector-buffer)))
    (dolist (segment segments)
      (let ((start (text-length gtk-buffer))
            (text (second segment)))
        (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) text -1)
        (let ((end (text-length gtk-buffer)))
          (flet ((tag (name) (gtk:text-buffer-apply-tag-by-name gtk-buffer name (iter-at gtk-buffer start)
                                                                (iter-at gtk-buffer end))))
            (case (first segment)
              (:value (tag "cadre-inspector-value")
               (push (list start end :value (third segment)) (ins-links *inspector*)))
              (:action (tag "cadre-inspector-action")
               (push (list start end :action (third segment)) (ins-links *inspector*)))
              (:label (tag "cadre-inspector-label")))))))))

(defun insert-more-link ()
  (let* ((gtk-buffer (inspector-buffer))
         (start (text-length gtk-buffer)))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) "[show more]" -1)
    (gtk:text-buffer-apply-tag-by-name gtk-buffer "cadre-inspector-action" (iter-at gtk-buffer start)
                                       (gtk:text-buffer-get-end-iter gtk-buffer))
    (push (list start (text-length gtk-buffer) :more nil) (ins-links *inspector*))))

(defun show-inspection (reply &key (thread t))
  "Show swank's inspector REPLY on the Inspector page."
  (let ((inspection (parse-inspection reply)))
    (unless inspection (editor-error "Nothing more to show"))
    (setf (ins-links *inspector*) '()
          (ins-next *inspector*) (inspection-next inspection)
          (ins-more *inspector*) (inspection-more inspection)
          (ins-thread *inspector*) thread)
    (gtk:label-set-text (ins-title *inspector*) (inspection-title inspection))
    (gtk:widget-set-tooltip-text (ins-title *inspector*) (inspection-title inspection))
    (text-replace-contents (inspector-buffer) "")
    (insert-segments (inspection-parts inspection))
    (when (inspection-more inspection) (insert-more-link))
    (gtk:widget-set-sensitive (ins-back *inspector*) t)
    (gtk:widget-set-sensitive (ins-forward *inspector*) t)
    (set-panel-visible *window* t)
    (panel-show (window-panel *window*) "inspector")))

(defun inspector-rex (form &key (on-ok #'show-inspection))
  (with-connection (connection)
    (let ((thread (ins-thread *inspector*)))
      (rex connection form :thread thread
           :on-ok (lambda (reply) (funcall on-ok reply :thread thread))))))

(defun follow-inspector-link (link)
  (destructuring-bind (start end kind index) link
    (declare (ignore start end))
    (ecase kind
      (:value (inspector-rex (swank-call "swank:inspect-nth-part" index)))
      (:action (inspector-rex (swank-call "swank:inspector-call-nth-action" index)
                              :on-ok (lambda (reply &key thread)
                                       (when reply (show-inspection reply :thread thread)))))
      (:more (let ((from (ins-next *inspector*)))
               (inspector-rex (swank-call "swank:inspector-range" from (+ from 500))
                              :on-ok (lambda (reply &key thread)
                                       (declare (ignore thread))
                                       (show-more-parts reply))))))))

(defun show-more-parts (content)
  (let ((more-link (find :more (ins-links *inspector*) :key #'third))
        (gtk-buffer (inspector-buffer)))
    (when more-link
      (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer (first more-link)) (iter-at gtk-buffer (second more-link)))
      (setf (ins-links *inspector*) (remove more-link (ins-links *inspector*))))
    (multiple-value-bind (segments next more) (parse-inspector-range content)
      (insert-segments segments)
      (setf (ins-next *inspector*) next
            (ins-more *inspector*) more)
      (when more (insert-more-link)))))

;;; Commands

(defun expression-at-cursor (view)
  "The selection, or else the expression before the cursor or the symbol at it, as text."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (if has
          (gtk:text-buffer-get-text gtk-buffer start end t)
          (let ((syntax (buffer-syntax (view-buffer view))))
            (when syntax
              (multiple-value-bind (line column) (cursor-line-column view)
                (or (symbol-at syntax line column)
                    (multiple-value-bind (sl sc) (backward-sexp-position syntax line column)
                      (and sl (view-region-text view sl sc line column)))))))))))

(defun inspect-string (string package)
  (with-connection (connection)
    (rex connection (swank-call "swank:init-inspector" string) :package package
         :on-ok (lambda (reply) (show-inspection reply)))))

(define-command inspect-value ()
  "Inspect the value of an expression (at first, the one at the cursor)."
  (let* ((view (current-view))
         (initial (or (and view (expression-at-cursor view)) ""))
         (package (if view (view-package view) (and (connected-p) (connection-package *connection*)))))
    (open-picker (window-picker *window*)
                 :placeholder "Inspect the value of" :text initial
                 :on-choose (lambda (text)
                              (unless (string= (string-trim " " text) "")
                                (inspect-string text package))))))

(define-command inspector-back ()
  "Inspect the object inspected before this one."
  (inspector-rex (swank-call "swank:inspector-pop")
                 :on-ok (lambda (reply &key thread)
                          (if reply (show-inspection reply :thread thread) (message "This is the first object")))))

(define-command inspector-forward ()
  "Inspect the object inspected after this one."
  (inspector-rex (swank-call "swank:inspector-next")
                 :on-ok (lambda (reply &key thread)
                          (if reply (show-inspection reply :thread thread) (message "This is the last object")))))

(define-command inspector-refresh ()
  "Inspect the current object again, to see changes."
  (inspector-rex (swank-call "swank:inspector-reinspect")))
