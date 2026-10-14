;;;; highlight.lisp — syntax colors, the current line, and matching parens
;;;;
;;;; Each Lisp buffer (Common Lisp, Racket or Clojure) has a LISP-SYNTAX
;;;; (core), read by its mode's dialect, kept in step with its
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
          (remove :quote *faces*) *markdown-faces* *image-faces* *code-faces* '(:quote)
          (loop for i below *paren-face-count* collect (list :paren i))
          '(:search :search-current :paren-match :paren-mismatch))
  "Every face with a tag, in priority order.")

(defun syntax-face-p (face)
  (or (consp face) (member face *faces*) (member face *image-faces*) (member face *markdown-faces*)
      (member face *code-faces*)))

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
    (restyle-named-tags gtk-buffer)
    (restyle-preview-tags gtk-buffer)
    (restyle-hyperspec-tags gtk-buffer)))

;;; Keeping the syntax in step with the text

(defun buffer-sexp-syntax (buffer)
  "BUFFER's s-expression syntax in any Lisp (Common Lisp, Racket, Clojure), or
nil: for editing, which works the same in all of them."
  (buffer-local buffer :syntax))

(defun buffer-syntax (buffer)
  "BUFFER's syntax if it holds Common Lisp, or nil: for what the connected
Lisp (through Swank) and Common Lisp's own rules know about."
  (let ((syntax (buffer-local buffer :syntax)))
    (and syntax (eq (syntax-dialect syntax) *common-lisp-dialect*) syntax)))

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
                                (markdown-lines-changed buffer (gtk:text-iter-get-line location))
                                (let ((syntax (buffer-sexp-syntax buffer)))
                                  (when syntax
                                    (syntax-lines-changed syntax (gtk:text-iter-get-line location) 1
                                                          (1+ (count #\Newline text)))))))
             (gobject:connect gtk-buffer :delete-range
                              (lambda (b start end)
                                (declare (ignore b))
                                (markdown-lines-changed buffer (gtk:text-iter-get-line start))
                                (let ((syntax (buffer-sexp-syntax buffer)))
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
    (let ((dialect (mode-dialect (buffer-major-mode buffer)))
          (syntax (buffer-sexp-syntax buffer)))
      (unless (and syntax (eq (syntax-dialect syntax) dialect))
        (when syntax
          (setf (buffer-local buffer :syntax) nil)
          (remove-syntax-tags gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                              (gtk:text-buffer-get-end-iter gtk-buffer)))
        (when dialect
          (setf (buffer-local buffer :syntax) (make-lisp-syntax gtk-buffer :dialect dialect)))))
    (attach-markdown buffer)
    (attach-tree-sitter buffer)
    (schedule-highlight buffer)))

;;; Tagging lines

(defun remove-syntax-tags (gtk-buffer start end)
  (dolist (face *tag-faces*)
    (when (syntax-face-p face)
      (gtk:text-buffer-remove-tag gtk-buffer (face-tag gtk-buffer face) start end))))

(defun highlight-line (gtk-buffer syntax line &optional image)
  "Tag LINE's tokens with their faces. IMAGE (from image-context) adds the
faces that come from what the connected Lisp knows."
  (let ((string (text-line-string gtk-buffer line))
        (dialect (syntax-dialect syntax)))
    (remove-syntax-tags gtk-buffer (line-iter gtk-buffer line) (line-end-iter gtk-buffer line))
    (loop for token across (line-tokens syntax line)
          for face = (or (token-face token string dialect)
                         (and image (eq (token-type token) :symbol) (image-face image syntax line token string)))
          when face
            do (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face)
                                          (line-iter gtk-buffer line (token-start token))
                                          (line-iter gtk-buffer line (token-end token))))
    ;; What #; or #_ comments out looks like a comment, even across lines.
    (loop for (start end) in (datum-comment-columns syntax line)
          do (let ((from (line-iter gtk-buffer line start))
                   (to (line-iter gtk-buffer line end)))
               (remove-syntax-tags gtk-buffer from to)
               (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer :comment) from to)))))

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
  (let ((syntax (buffer-sexp-syntax (view-buffer view)))
        (gtk-buffer (view-gtk-buffer view)))
    (when syntax
      (multiple-value-bind (first last) (visible-lines view 40)
        (setf last (min last (1- (syntax-line-count syntax))))
        (ensure-lexed syntax last)
        (loop with image = (and (buffer-syntax (view-buffer view))   ; the image knows Common Lisp only
                                (image-context (view-buffer view) syntax first))
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
                         (highlight-markdown-view view)
                         (highlight-tree-sitter-view view)
                         (update-cursor-decorations view)))
                     nil))))

;;; The current line and matching parens

(defun update-cursor-decorations (view)
  (unless (or (buffer-local (view-buffer view) :preview-of)  ; a page, not text being edited
              (hyperspec-buffer-p (view-buffer view))
              (image-buffer-p (view-buffer view)))         ; a picture
    (update-cursor-decorations-1 view)))

(defun update-cursor-decorations-1 (view)
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
          ;; From the whole buffer, not just the two parens: once the text
          ;; between them is replaced, they are one tagged run and the new
          ;; text inside takes the tag too.
          (let ((start (gtk:text-buffer-get-start-iter gtk-buffer))
                (end (gtk:text-buffer-get-end-iter gtk-buffer)))
            (gtk:text-buffer-remove-tag gtk-buffer match start end)
            (gtk:text-buffer-remove-tag gtk-buffer mismatch start end))
          (let ((syntax (buffer-sexp-syntax buffer)))
            (when (and syntax (eq (context-at syntax line column) :code))
              (multiple-value-bind (l1 c1 l2 c2 matched) (paren-match-at syntax line column)
                (when l1
                  (let ((tag (if matched match mismatch)))
                    (dolist (range (list (list l1 c1) (list l2 c2)))
                      (destructuring-bind (l c) range
                        (gtk:text-buffer-apply-tag gtk-buffer tag (line-iter gtk-buffer l c)
                                                   (line-iter gtk-buffer l (1+ c)))))))))))))))

(defun setup-theme-following ()
  "Use the current theme now, and again when the light/dark style changes."
  (load-user-themes)
  (apply-theme)
  (gobject:connect (adw:style-manager-get-default) "notify::dark"
                   (lambda (manager pspec) (declare (ignore manager pspec))
                     (apply-theme))))
