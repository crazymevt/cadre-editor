;;;; notes.lisp — compiler notes: the Problems page and underlines
;;;;
;;;; Each note is shown in the Problems page and, if its buffer is open, as
;;;; a wavy underline over the expression it is about, with the message as a
;;;; tooltip.

(in-package #:cadre-ui)

(defstruct (shown-note (:conc-name sn-))
  note buffer file line column end-line end-column)

(defvar *notes* '() "Notes on display, as shown-note structures.")
(defvar *notes-list* nil "The Problems page's list store.")

(defparameter *note-faces* '(:note-error :note-warning :note-style))

(defun note-face (severity)
  (case (severity-rank severity) (0 :note-error) (1 :note-warning) (t :note-style)))

(defun ensure-note-tags (gtk-buffer)
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (unless (gtk:text-tag-table-lookup table "cadre-note-error")
      (dolist (face *note-faces*)
        (gtk:text-tag-table-add table (make-instance 'gtk:text-tag :name (tag-name face) :underline :error)))
      (style-note-tags gtk-buffer))))

(defun hex-rgba (string)
  (let ((rgba (gdk:make-rgba)))
    (gdk:rgba-parse rgba string)
    rgba))

(defun style-note-tags (gtk-buffer)
  (loop for face in *note-faces*
        for color in (if (adw:dark-p) '("#ff6b66" "#e5a50a" "#7f848e") '("#d0312d" "#c27c0e" "#8a8f98"))
        do (let ((tag (face-tag gtk-buffer face)))
             (when tag (setf (gobject:property tag :underline-rgba) (hex-rgba color))))))

(defun note-buffer (shown)
  (or (and (sn-buffer shown) (member (sn-buffer shown) (buffer-list)) (sn-buffer shown))
      (and (sn-file shown) (find-file-buffer (sn-file shown)))))

(defun underline-note (shown)
  (let ((buffer (note-buffer shown)))
    (when (and buffer (typep (buffer-text buffer) 'gtk:text-buffer))
      (let ((gtk-buffer (buffer-text buffer)))
        (ensure-note-tags gtk-buffer)
        (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer (note-face (compiler-note-severity (sn-note shown))))
                                   (line-iter gtk-buffer (sn-line shown) (sn-column shown))
                                   (line-iter gtk-buffer (sn-end-line shown) (sn-end-column shown)))))))

(defun remove-underlines (buffer &optional (first-line 0) last-line)
  (let ((gtk-buffer (buffer-text buffer)))
    (when (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) "cadre-note-error")
      (let ((start (line-iter gtk-buffer first-line))
            (end (if last-line (line-end-iter gtk-buffer last-line) (gtk:text-buffer-get-end-iter gtk-buffer))))
        (dolist (face *note-faces*)
          (gtk:text-buffer-remove-tag gtk-buffer (face-tag gtk-buffer face) start end))))))

(defun note-range (buffer position)
  "The line/column range a note at POSITION (an offset) covers: the
expression there, or the rest of its line."
  (let ((text (buffer-text buffer)))
    (multiple-value-bind (line column) (text-position-line text (min position (text-length text)))
      (let ((syntax (buffer-syntax buffer)))
        (multiple-value-bind (el ec) (and syntax (forward-sexp-position syntax line column))
          (if el
              (values line column el ec)
              (values line column line (max (1+ column) (length (text-line-string text line))))))))))

(defun place-note (note &key buffer)
  "A shown-note for NOTE (a compiler-note). BUFFER is the buffer compiled, for notes in it."
  (let* ((location (compiler-note-location note))
         (target (or (and (getf location :buffer) buffer)
                     (and (getf location :buffer) (find-buffer (getf location :buffer)))
                     (and (getf location :file) (find-file-buffer (getf location :file)))))
         (file (or (getf location :file) (and target (buffer-file target)))))
    (if (and target (getf location :position))
        (multiple-value-bind (l c el ec) (note-range target (getf location :position))
          (make-shown-note :note note :buffer target :file file :line l :column c :end-line el :end-column ec))
        (make-shown-note :note note :buffer target :file file :line (getf location :line) :column 0
                         :end-line (getf location :line) :end-column 0))))

(defun show-notes (notes &key buffer replace-lines)
  "Show NOTES (compiler-notes) from compiling BUFFER. REPLACE-LINES (first .
last) clears earlier notes in those lines of BUFFER first; :all clears all of
BUFFER's notes."
  (when (and buffer replace-lines)
    (setf *notes* (remove-if (lambda (s)
                               (and (eq (note-buffer s) buffer)
                                    (or (eq replace-lines :all)
                                        (and (sn-line s) (<= (car replace-lines) (sn-line s) (cdr replace-lines))))))
                             *notes*))
    (if (eq replace-lines :all)
        (remove-underlines buffer)
        (remove-underlines buffer (car replace-lines) (cdr replace-lines))))
  (let ((new (mapcar (lambda (n) (place-note n :buffer buffer)) notes)))
    (dolist (s new) (when (sn-line s) (underline-note s)))
    (setf *notes* (stable-sort (append *notes* new) #'<
                               :key (lambda (s) (severity-rank (compiler-note-severity (sn-note s))))))
    (refresh-problems)))

(define-command clear-notes ()
  "Remove every compiler note."
  (dolist (buffer (buffer-list))
    (when (typep (buffer-text buffer) 'gtk:text-buffer) (remove-underlines buffer)))
  (setf *notes* '())
  (refresh-problems))

;;; The Problems page

(defun severity-icon (severity)
  (case (severity-rank severity)
    (0 "dialog-error-symbolic") (1 "dialog-warning-symbolic") (t "dialog-information-symbolic")))

(defun note-place-string (shown)
  (format nil "~a~@[:~d~]"
          (cond ((sn-file shown) (file-namestring (sn-file shown)))
                ((sn-buffer shown) (buffer-name (sn-buffer shown)))
                (t ""))
          (and (sn-line shown) (1+ (sn-line shown)))))

(defun make-problems-widget ()
  (let* ((store (gio:make-list-store))
         (list-view (gtk:make-list-view
                     store
                     :setup (lambda ()
                              (gtk:build
                                (gtk:box :spacing 8 :margin-start 6 :margin-end 6
                                  (gtk:image)
                                  (gtk:label :xalign 0.0 :hexpand t :ellipsize :end)
                                  (gtk:label :css-classes '("dim-label")))))
                     :bind (lambda (row shown)
                             (let* ((icon (gtk:widget-get-first-child row))
                                    (text (gtk:widget-get-next-sibling icon))
                                    (place (gtk:widget-get-next-sibling text))
                                    (message (compiler-note-message (sn-note shown))))
                               (gtk:image-set-from-icon-name icon (severity-icon (compiler-note-severity (sn-note shown))))
                               (gtk:label-set-text text (substitute #\Space #\Newline message))
                               (gtk:widget-set-tooltip-text row message)
                               (gtk:label-set-text place (note-place-string shown)))))))
    (setf *notes-list* store)
    (gtk:list-view-set-single-click-activate list-view t)
    (gobject:connect list-view :activate
                     (lambda (lv position) (declare (ignore lv))
                       (goto-note (gobject:lisp-object-value (gio:list-model-get-item store position)))))
    (gtk:build
      (gtk:box :orientation :vertical
        (gtk:box :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2 :spacing 6
          (gtk:label :hexpand t)
          (gtk:button :label "Ask Claude to Fix" :css-classes '("flat")
                      :tooltip-text "Send the problems to Claude and ask for a fix"
                      :on-clicked (lambda (b) (declare (ignore b)) (call-command 'ask-claude-about-problems))))
        (gtk:scrolled-window :child list-view :vexpand t)))))

(defun refresh-problems ()
  (when *notes-list*
    (gio:list-store-remove-all *notes-list*)
    (dolist (s *notes*) (gio:list-store-append *notes-list* (gobject:make-lisp-object s)))
    (let ((errors (count 0 *notes* :key (lambda (s) (severity-rank (compiler-note-severity (sn-note s))))))
          (others (count-if-not #'zerop *notes* :key (lambda (s) (severity-rank (compiler-note-severity (sn-note s)))))))
      (panel-set-title (window-panel *window*) "problems"
                       (if *notes* (format nil "Problems (~d)" (+ errors others)) "Problems")))))

(defun goto-note (shown)
  (let ((buffer (note-buffer shown)))
    (flet ((visit (view)
             (when (sn-line shown)
               (goto-line-column view (sn-line shown) (sn-column shown) :extend nil))
             (focus-view view)))
      (cond (buffer (visit (show-buffer *window* buffer)))
            ((sn-file shown) (open-file-path (pathname (sn-file shown)) :then #'visit))))))

(defun note-at (buffer line column)
  (find-if (lambda (s) (and (eq (note-buffer s) buffer) (sn-line s)
                            (or (> line (sn-line s)) (and (= line (sn-line s)) (>= column (sn-column s))))
                            (or (< line (sn-end-line s)) (and (= line (sn-end-line s)) (<= column (sn-end-column s))))))
           *notes*))

(defun setup-note-tooltips (view)
  (let ((text-view (view-text-view view)))
    (gtk:widget-set-has-tooltip text-view t)
    (gobject:connect text-view :query-tooltip
                     (lambda (widget x y keyboard tooltip)
                       (declare (ignore widget keyboard))
                       (multiple-value-bind (bx by)
                           (gtk:text-view-window-to-buffer-coords text-view :widget x y)
                         (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
                           (let ((shown (and ok (note-at (view-buffer view) (gtk:text-iter-get-line iter)
                                                         (gtk:text-iter-get-line-offset iter)))))
                             (when shown
                               (gtk:tooltip-set-text tooltip (compiler-note-message (sn-note shown)))
                               t))))))))

(define-command next-note ()
  "Go to the next compiler note."
  (let ((view (current-view)))
    (multiple-value-bind (line) (cursor-line-column view)
      (let ((next (or (find-if (lambda (s) (and (eq (note-buffer s) (view-buffer view)) (sn-line s) (> (sn-line s) line)))
                               (sort (copy-list *notes*) #'< :key (lambda (s) (or (sn-line s) 0))))
                      (first *notes*))))
        (if next
            (progn (goto-note next) (message "~a" (compiler-note-message (sn-note next))))
            (message "No notes"))))))

(define-command previous-note ()
  "Go to the previous compiler note."
  (let ((view (current-view)))
    (multiple-value-bind (line) (cursor-line-column view)
      (let ((previous (or (find-if (lambda (s) (and (eq (note-buffer s) (view-buffer view)) (sn-line s) (< (sn-line s) line)))
                                   (sort (copy-list *notes*) #'> :key (lambda (s) (or (sn-line s) 0))))
                          (car (last *notes*)))))
        (if previous
            (progn (goto-note previous) (message "~a" (compiler-note-message (sn-note previous))))
            (message "No notes"))))))
