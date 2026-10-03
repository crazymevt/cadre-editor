;;;; highlight.lisp — syntax colors, the current line, and matching parens
;;;;
;;;; Each Lisp buffer has a LISP-SYNTAX (core) kept in step with its
;;;; gtk:text-buffer through the buffer's insert-text and delete-range
;;;; signals. Highlighting runs when idle and only tags the lines on screen
;;;; (plus a margin) that changed since they were last tagged.
;;;;
;;;; Tags are created in priority order (later wins): current line, syntax
;;;; faces, search matches, matching parens.

(in-package #:cadre-ui)

(defparameter *image-faces* '(:macro :undefined-function)
  "Faces from what the connected Lisp knows (image-faces.lisp).")

(defparameter *tag-faces*
  (append '(:current-line)
          (remove :quote *faces*) *image-faces* '(:quote)
          (loop for i below *paren-face-count* collect (list :paren i))
          '(:search :search-current :paren-match :paren-mismatch))
  "Every face with a tag, in priority order.")

(defun syntax-face-p (face)
  (or (consp face) (member face *faces*) (member face *image-faces*)))

(defun tag-name (face)
  (if (consp face)
      (format nil "cadre-paren-~d" (second face))
      (format nil "cadre-~(~a~)" face)))

(defun face-tag (gtk-buffer face)
  (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) (tag-name face)))

(defun style-buffer-tags (gtk-buffer)
  (dolist (face *tag-faces*)
    (let ((tag (face-tag gtk-buffer face)))
      (when tag (restyle-tag tag face)))))

(defun ensure-tags (gtk-buffer)
  "Give GTK-BUFFER Cadre's tags, once."
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (unless (gtk:text-tag-table-lookup table (tag-name :current-line))
      (dolist (face *tag-faces*)
        (gtk:text-tag-table-add table (make-instance 'gtk:text-tag :name (tag-name face))))
      (style-buffer-tags gtk-buffer))))

(defun restyle-all-buffers ()
  (dolist (gtk-buffer (append (loop for buffer in (buffer-list)
                                    when (typep (buffer-text buffer) 'gtk:text-buffer)
                                      collect (buffer-text buffer))
                              (remove nil (list (and *inspector* (inspector-buffer))))
                              (loop for review in *reviews*
                                    when (rv-gtk-buffer review) collect (rv-gtk-buffer review))))
    (style-buffer-tags gtk-buffer)
    (restyle-named-tags gtk-buffer)))

;;; Keeping the syntax in step with the text

(defun buffer-syntax (buffer)
  "BUFFER's Lisp syntax, or nil if it is not a Lisp buffer."
  (buffer-local buffer :syntax))

(defun attach-syntax (buffer)
  "Set up BUFFER's tags and, for Lisp buffers, its syntax. Safe to call again,
for instance after the buffer's major mode changes."
  (let ((gtk-buffer (buffer-text buffer)))
    (ensure-tags gtk-buffer)
    (ensure-note-tags gtk-buffer)
    (unless (buffer-local buffer :change-handlers)
      (setf (buffer-local buffer :change-handlers)
            (list
             (gobject:connect gtk-buffer :insert-text
                              (lambda (b location text length)
                                (declare (ignore b length))
                                (let ((syntax (buffer-syntax buffer)))
                                  (when syntax
                                    (syntax-lines-changed syntax (gtk:text-iter-get-line location) 1
                                                          (1+ (count #\Newline text)))))))
             (gobject:connect gtk-buffer :delete-range
                              (lambda (b start end)
                                (declare (ignore b))
                                (let ((syntax (buffer-syntax buffer)))
                                  (when syntax
                                    (let ((first (gtk:text-iter-get-line start)))
                                      (syntax-lines-changed syntax first
                                                            (1+ (- (gtk:text-iter-get-line end) first))
                                                            1))))))
             (gobject:connect gtk-buffer :changed
                              (lambda (b) (declare (ignore b))
                                (schedule-highlight buffer)
                                (clear-inline-result buffer)
                                (completion-buffer-changed buffer))))))
    (let ((lisp (eq (buffer-major-mode buffer) 'lisp-mode)))
      (cond ((and lisp (not (buffer-syntax buffer)))
             (setf (buffer-local buffer :syntax) (make-lisp-syntax gtk-buffer)))
            ((and (not lisp) (buffer-syntax buffer))
             (setf (buffer-local buffer :syntax) nil)
             (remove-syntax-tags gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                                 (gtk:text-buffer-get-end-iter gtk-buffer)))))
    (schedule-highlight buffer)))

;;; Tagging lines

(defun remove-syntax-tags (gtk-buffer start end)
  (dolist (face *tag-faces*)
    (when (syntax-face-p face)
      (gtk:text-buffer-remove-tag gtk-buffer (face-tag gtk-buffer face) start end))))

(defun highlight-line (gtk-buffer syntax line &optional image)
  "Tag LINE's tokens with their faces. IMAGE (from image-context) adds the
faces that come from what the connected Lisp knows."
  (let ((string (text-line-string gtk-buffer line)))
    (remove-syntax-tags gtk-buffer (line-iter gtk-buffer line) (line-end-iter gtk-buffer line))
    (loop for token across (line-tokens syntax line)
          for face = (or (token-face token string)
                         (and image (eq (token-type token) :symbol) (image-face image syntax line token string)))
          when face
            do (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face)
                                          (line-iter gtk-buffer line (token-start token))
                                          (line-iter gtk-buffer line (token-end token))))))

(defun visible-lines (view &optional (margin 0))
  "The first and last lines on screen in VIEW, widened by MARGIN lines."
  (let* ((text-view (view-text-view view))
         (rect (gtk:text-view-get-visible-rect text-view))
         (top (gtk:text-iter-get-line (gtk:text-view-get-line-at-y text-view (gdk:rectangle-y rect))))
         (bottom (gtk:text-iter-get-line
                  (gtk:text-view-get-line-at-y text-view (+ (gdk:rectangle-y rect)
                                                            (gdk:rectangle-height rect)))))
         (count (gtk:text-buffer-get-line-count (view-gtk-buffer view))))
    (values (max 0 (- top margin))
            (min (1- count) (+ bottom margin (if (zerop (gdk:rectangle-height rect)) 60 0))))))

(defun highlight-view (view)
  (let ((syntax (buffer-syntax (view-buffer view)))
        (gtk-buffer (view-gtk-buffer view)))
    (when syntax
      (multiple-value-bind (first last) (visible-lines view 40)
        (setf last (min last (1- (syntax-line-count syntax))))
        (ensure-lexed syntax last)
        (loop with image = (image-context (view-buffer view) syntax first)
              for line from first to last
              for info = (line-info syntax line)
              unless (line-info-highlighted info)
                do (highlight-line gtk-buffer syntax line image)
                   (setf (line-info-highlighted info) t))))))

(defun schedule-highlight (buffer)
  "Highlight BUFFER's views when GTK is next idle."
  (unless (buffer-local buffer :highlight-pending)
    (setf (buffer-local buffer :highlight-pending) t)
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (setf (buffer-local buffer :highlight-pending) nil)
                     (when *window*
                       (dolist (view (append (buffer-views *window* buffer)
                                             (and (repl-view) (eq (view-buffer (repl-view)) buffer)
                                                  (list (repl-view)))))
                         (highlight-view view)
                         (update-cursor-decorations view)))
                     nil))))

;;; The current line and matching parens

(defun update-cursor-decorations (view)
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (cursor (cursor-iter gtk-buffer)))
    (multiple-value-bind (line column) (iter-line-column cursor)
      ;; Current line: from the line's start to the next line's start, so
      ;; the paragraph background spans the whole width.
      (let ((tag (face-tag gtk-buffer :current-line)))
        (when tag
          (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                      (gtk:text-buffer-get-end-iter gtk-buffer))
          (let ((end (line-iter gtk-buffer line)))
            (unless (gtk:text-iter-forward-line end)
              (setf end (gtk:text-buffer-get-end-iter gtk-buffer)))
            (gtk:text-buffer-apply-tag gtk-buffer tag (line-iter gtk-buffer line) end))))
      ;; Matching parens.
      (let ((match (face-tag gtk-buffer :paren-match))
            (mismatch (face-tag gtk-buffer :paren-mismatch)))
        (when match
          (dolist (range (buffer-local buffer :paren-ranges))
            (destructuring-bind (l c) range
              (when (< l (gtk:text-buffer-get-line-count gtk-buffer))
                (let ((start (line-iter gtk-buffer l c))
                      (end (line-iter gtk-buffer l (1+ c))))
                  (gtk:text-buffer-remove-tag gtk-buffer match start end)
                  (gtk:text-buffer-remove-tag gtk-buffer mismatch start end)))))
          (setf (buffer-local buffer :paren-ranges) nil)
          (let ((syntax (buffer-syntax buffer)))
            (when (and syntax (eq (context-at syntax line column) :code))
              (multiple-value-bind (l1 c1 l2 c2 matched) (paren-match-at syntax line column)
                (when l1
                  (let ((tag (if matched match mismatch)))
                    (dolist (range (list (list l1 c1) (list l2 c2)))
                      (destructuring-bind (l c) range
                        (gtk:text-buffer-apply-tag gtk-buffer tag (line-iter gtk-buffer l c)
                                                   (line-iter gtk-buffer l (1+ c)))))
                    (setf (buffer-local buffer :paren-ranges) (list (list l1 c1) (list l2 c2)))))))))))))

(defun setup-theme-following ()
  "Use the current theme now, and again when the light/dark style changes."
  (load-user-themes)
  (apply-theme)
  (gobject:connect (adw:style-manager-get-default) "notify::dark"
                   (lambda (manager pspec) (declare (ignore manager pspec))
                     (apply-theme))))
