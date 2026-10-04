;;;; markdown.lisp — Markdown files: highlighting, a live preview, and
;;;; editing help
;;;;
;;;; Highlighting works a line at a time from core's MARKDOWN-LINE-SPANS,
;;;; keeping each line's starting state (in a fenced code block or not) and
;;;; tagging only the lines on screen, as Lisp highlighting does.
;;;;
;;;; The preview is a buffer of its own, shown beside the source in another
;;;; editor group and drawn again as you type: headings, emphasis, code
;;;; (Lisp highlighted), lists and task lists, quotes, tables, rules,
;;;; images from local files, and links you can click. It follows the
;;;; source as you scroll it.
;;;;
;;;; Return continues a list item or quote (and ends the list on an empty
;;;; item); Tab and Shift+Tab indent and outdent list items; typing * _ or ~
;;;; with text selected wraps it.

(in-package #:cadre-ui)

(define-option *markdown-preview-scroll-sync* t boolean
  "Scroll a Markdown preview along with its source."
  :category "Editing")

;;; Highlighting

(defstruct (markdown-lines (:conc-name ml-))
  (states (make-array 1 :adjustable t :fill-pointer 1 :initial-element '(:text)))
  (tagged (make-hash-table)))

(defun markdown-buffer-p (buffer) (eq (buffer-major-mode buffer) 'markdown-mode))

(defun markdown-lines-changed (buffer line)
  "Text changed from LINE on: states and tags after it must be worked out again."
  (let ((ml (buffer-local buffer :markdown)))
    (when ml
      (setf (fill-pointer (ml-states ml)) (max 1 (min (fill-pointer (ml-states ml)) (1+ line))))
      (let ((tagged (ml-tagged ml)))
        (loop for l being the hash-keys of tagged
              when (>= l line) do (remhash l tagged))))))

(defun markdown-line-state (ml gtk-buffer line)
  "The state at the start of LINE, working forward from the last one known."
  (let ((states (ml-states ml)))
    (loop for l from (1- (fill-pointer states)) below line
          do (vector-push-extend (nth-value 1 (markdown-line-spans (text-line-string gtk-buffer l) (aref states l)))
                                 states))
    (aref states line)))

(defun highlight-markdown-line (gtk-buffer ml line)
  (let* ((string (text-line-string gtk-buffer line))
         (start (line-iter gtk-buffer line))
         (next (let ((it (line-iter gtk-buffer line)))
                 (if (gtk:text-iter-forward-line it) it (gtk:text-buffer-get-end-iter gtk-buffer)))))
    (remove-syntax-tags gtk-buffer start next)
    (dolist (span (markdown-line-spans string (markdown-line-state ml gtk-buffer line)))
      (destructuring-bind (s e face) span
        (if (eq face :md-code-block)
            ;; The background spans the whole line, as the current line's does.
            (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face) (line-iter gtk-buffer line)
                                       (let ((it (line-iter gtk-buffer line)))
                                         (if (gtk:text-iter-forward-line it) it (gtk:text-buffer-get-end-iter gtk-buffer))))
            (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face)
                                       (line-iter gtk-buffer line s) (line-iter gtk-buffer line e)))))))

(defun highlight-markdown-view (view)
  (let ((ml (buffer-local (view-buffer view) :markdown))
        (gtk-buffer (view-gtk-buffer view)))
    (when ml
      (multiple-value-bind (first last) (visible-lines view 40)
        (setf last (min last (1- (gtk:text-buffer-get-line-count gtk-buffer))))
        (loop for line from first to last
              unless (gethash line (ml-tagged ml))
                do (highlight-markdown-line gtk-buffer ml line)
                   (setf (gethash line (ml-tagged ml)) t))))))

(defun attach-markdown (buffer)
  "Start or stop Markdown highlighting in BUFFER, to match its mode."
  (cond ((and (markdown-buffer-p buffer) (not (buffer-local buffer :markdown)))
         (setf (buffer-local buffer :markdown) (make-markdown-lines)))
        ((and (not (markdown-buffer-p buffer)) (buffer-local buffer :markdown))
         (setf (buffer-local buffer :markdown) nil)
         (let ((gtk-buffer (buffer-text buffer)))
           (remove-syntax-tags gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                               (gtk:text-buffer-get-end-iter gtk-buffer))))))

;;; The preview: tags

(defparameter *preview-heading-scales* #(2.0d0 1.6d0 1.35d0 1.15d0 1.0d0 0.9d0))

(defun preview-tag (gtk-buffer name &rest properties)
  "The tag NAME in GTK-BUFFER, made with PROPERTIES if it is new."
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (or (gtk:text-tag-table-lookup table name)
        (let ((tag (make-instance 'gtk:text-tag :name name)))
          (loop for (key value) on properties by #'cddr
                do (setf (gobject:property tag key) value))
          (gtk:text-tag-table-add table tag)
          tag))))

(defparameter *preview-tag-faces*
  '(("md-p-strong" . :md-strong) ("md-p-em" . :md-emphasis) ("md-p-strike" . :md-strike)
    ("md-p-code" . :md-code) ("md-p-link" . :md-link) ("md-p-quote" . :md-quote)
    ("md-p-rule" . :md-markup) ("md-p-marker" . :md-list)
    ("md-p-h1" . :md-heading) ("md-p-h2" . :md-heading) ("md-p-h3" . :md-heading)
    ("md-p-h4" . :md-heading) ("md-p-h5" . :md-heading) ("md-p-h6" . :md-heading))
  "Preview tags colored by a theme face.")

(defun restyle-preview-tags (gtk-buffer)
  "Color GTK-BUFFER's preview tags from the current theme."
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer))
        (code-background (theme-color :md-code-block :paragraph-background)))
    (loop for (name . face) in *preview-tag-faces*
          for tag = (gtk:text-tag-table-lookup table name)
          when tag do (restyle-tag tag face))
    (let ((code (gtk:text-tag-table-lookup table "md-p-code"))
          (block (gtk:text-tag-table-lookup table "md-p-codeblock")))
      (when (and code code-background) (setf (gobject:property code :background) code-background))
      (when (and block code-background) (setf (gobject:property block :paragraph-background) code-background)))))

(defun ensure-preview-tags (gtk-buffer)
  (unless (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) "md-p-strong")
    (ensure-tags gtk-buffer)
    ;; Made first, so every other tag (code's Monospace) wins over it.
    (preview-tag gtk-buffer "md-p-body" :family "Sans" :scale 1.1d0 :pixels-below-lines 2)
    (loop for level from 1 to 6
          do (preview-tag gtk-buffer (format nil "md-p-h~d" level)
                          :scale (aref *preview-heading-scales* (1- level)) :weight 700
                          :pixels-above-lines (if (<= level 2) 14 8) :pixels-below-lines 4))
    (preview-tag gtk-buffer "md-p-strong")
    (preview-tag gtk-buffer "md-p-em")
    (preview-tag gtk-buffer "md-p-strike")
    (preview-tag gtk-buffer "md-p-code" :family "Monospace")
    (preview-tag gtk-buffer "md-p-codeblock" :family "Monospace" :left-margin 28 :right-margin 16
                                             :pixels-above-lines 0 :pixels-below-lines 0 :wrap-mode :char)
    (preview-tag gtk-buffer "md-p-link")
    (preview-tag gtk-buffer "md-p-quote")
    (preview-tag gtk-buffer "md-p-rule")
    (preview-tag gtk-buffer "md-p-marker")
    (preview-tag gtk-buffer "md-p-table" :family "Monospace" :wrap-mode :none)
    (preview-tag gtk-buffer "md-p-table-header" :weight 700)
    (preview-tag gtk-buffer "md-p-gap" :scale 0.45d0)
    (restyle-preview-tags gtk-buffer)))

(defun margin-tag (gtk-buffer level &key hang)
  "A tag indenting paragraphs LEVEL steps; with HANG, the first line starts
further left, for a list item's marker."
  (preview-tag gtk-buffer (format nil "md-p-~:[margin~;hang~]-~d" hang level)
               :left-margin (+ 24 (* 28 level)) :indent (if hang -20 0)))

;;; The preview: drawing

(defvar *preview* nil "While drawing: (gtk-buffer links lines base-directory).")

(defun preview-insert (text tags)
  "Insert TEXT at the end of the preview being drawn, with TAGS (tags or names)."
  (let* ((gtk-buffer (first *preview*))
         (start (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter gtk-buffer))))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) text -1)
    (let ((s (iter-at gtk-buffer start))
          (e (gtk:text-buffer-get-end-iter gtk-buffer)))
      (dolist (tag tags)
        (if (stringp tag)
            (gtk:text-buffer-apply-tag-by-name gtk-buffer tag s e)
            (gtk:text-buffer-apply-tag gtk-buffer tag s e))))
    start))

(defun preview-end () (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter (first *preview*))))

(defun preview-image (url alt tags)
  "Show the local image URL, or its ALT text when it can't be shown."
  (let* ((base (fourth *preview*))
         (path (and (not (search "://" url)) base
                    (merge-pathnames (string-left-trim "/" (substitute #\/ #\\ url)) base)))
         (texture (and path (probe-file path)
                       (ignore-errors
                        (let ((pixbuf (gdk-pixbuf:pixbuf-new-from-file-at-scale (uiop:native-namestring path) 640 -1 t)))
                          (gdk:texture-new-for-pixbuf pixbuf))))))
    (if texture
        (let ((gtk-buffer (first *preview*)))
          (gtk:text-buffer-insert-paintable gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) texture))
        (let ((start (preview-insert (format nil "[~a]" (if (string= alt "") "image" alt)) (cons "md-p-link" tags))))
          (push (list start (preview-end) url) (second *preview*))))))

(defun preview-inlines (inlines tags)
  "Insert INLINES ((string . nodes)) with TAGS."
  (let ((string (car inlines)))
    (labels ((walk (node tags)
               (destructuring-bind (kind s e cs ce children &rest more) node
                 (declare (ignore e))
                 (case kind
                   (:text (preview-insert (subseq string cs ce) tags))
                   (:escape (preview-insert (subseq string cs ce) tags))
                   (:code (preview-insert (subseq string cs ce) (cons "md-p-code" tags)))
                   (:strong (dolist (c children) (walk c (cons "md-p-strong" tags))))
                   (:emphasis (dolist (c children) (walk c (cons "md-p-em" tags))))
                   (:strike (dolist (c children) (walk c (cons "md-p-strike" tags))))
                   (:link (let ((start (preview-end)))
                            (dolist (c children) (walk c (cons "md-p-link" tags)))
                            (push (list start (preview-end) (third more)) (second *preview*))))
                   (:autolink (let ((start (preview-insert (subseq string cs ce) (cons "md-p-link" tags))))
                                (push (list start (preview-end) (first more)) (second *preview*))))
                   (:image (preview-image (third more) (subseq string cs ce) tags))
                   (t (preview-insert (subseq string s (fifth node)) tags))))))
      (dolist (node (cdr inlines)) (walk node tags)))))

(defun preview-code (info text tags)
  (let* ((gtk-buffer (first *preview*))
         (lisp (lisp-language-p info))
         (state '(:code 0)))
    (preview-insert (format nil "~%") (list "md-p-gap"))
    (dolist (line (split-text-lines text))
      (let ((start (preview-insert (format nil "~a~%" line) (list* "md-p-codeblock" tags))))
        (when lisp
          (multiple-value-bind (tokens end) (lex-line line state)
            (setf state end)
            (dolist (tk tokens)
              (let ((face (token-face tk line)))
                (when face
                  (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face)
                                             (iter-at gtk-buffer (+ start (token-start tk)))
                                             (iter-at gtk-buffer (+ start (token-end tk)))))))))))))

(defun preview-table (aligns header rows tags)
  (let* ((cells (cons (mapcar #'inline-plain-text header)
                      (mapcar (lambda (row) (mapcar #'inline-plain-text row)) rows)))
         (columns (reduce #'max cells :key #'length :initial-value 0))
         (widths (loop for c below columns
                       collect (reduce #'max cells :key (lambda (row) (length (or (nth c row) ""))) :initial-value 1))))
    (flet ((row-text (row)
             (format nil "~{~a~^  │  ~}~%"
                     (loop for c below columns
                           for text = (or (nth c row) "")
                           for width = (nth c widths)
                           for pad = (- width (length text))
                           collect (case (or (nth c aligns) :left)
                                     (:right (format nil "~v@{ ~}~a" pad nil text))
                                     (:center (format nil "~v@{ ~}~a~v@{ ~}" (floor pad 2) nil text (ceiling pad 2) nil))
                                     (t (format nil "~a~v@{ ~}" text pad nil)))))))
      (preview-insert (row-text (first cells)) (list* "md-p-table" "md-p-table-header" tags))
      (preview-insert (format nil "~{~a~^──┼──~}~%" (mapcar (lambda (w) (make-string w :initial-element #\─)) widths))
                      (list* "md-p-table" "md-p-rule" tags))
      (dolist (row (rest cells))
        (preview-insert (row-text row) (list* "md-p-table" tags))))))

(defun preview-blocks (blocks level &key tight quote)
  "Insert BLOCKS indented LEVEL steps. TIGHT leaves out the space after
paragraphs (in a list item); QUOTE styles them as quoted."
  (let ((gtk-buffer (first *preview*)))
    (loop for (block . more) on blocks
          do (let ((tags (append (list (margin-tag gtk-buffer level)) (and quote (list "md-p-quote")))))
               (push (cons (car (last block)) (preview-end)) (third *preview*))
               (ecase (first block)
                 (:heading (preview-inlines (third block) (cons (format nil "md-p-h~d" (second block)) tags))
                  (preview-insert (format nil "~%") tags))
                 (:paragraph (preview-inlines (second block) tags)
                  (preview-insert (format nil "~%") tags))
                 (:code (preview-code (second block) (third block) (list (margin-tag gtk-buffer level))))
                 (:quote (preview-blocks (second block) (1+ level) :quote t))
                 (:rule (preview-insert (format nil "~a~%" (make-string 48 :initial-element #\─)) (cons "md-p-rule" tags)))
                 (:table (preview-table (second block) (third block) (fourth block) tags))
                 (:list
                  (destructuring-bind (ordered start items line) (rest block)
                    (declare (ignore line))
                    (loop for (task item-blocks) in items
                          for n from start
                          do (let ((marker (format nil "~:[~a~*~;~*~d.~]~@[ ~a~]  "
                                                   ordered (if (evenp level) "•" "◦") n
                                                   (case task (:open "☐") (:done "☑"))))
                                   (first (first item-blocks)))
                               (preview-insert marker (list (margin-tag gtk-buffer (1+ level) :hang t) "md-p-marker"))
                               (if (and first (eq (first first) :paragraph))
                                   (progn
                                     (preview-inlines (second first)
                                                      (append (list (margin-tag gtk-buffer (1+ level) :hang t))
                                                              (and quote (list "md-p-quote"))
                                                              (and (eq task :done) (list "md-p-strike"))))
                                     (preview-insert (format nil "~%") (list (margin-tag gtk-buffer (1+ level) :hang t)))
                                     (preview-blocks (rest item-blocks) (1+ level) :tight t :quote quote))
                                   (progn (preview-insert (format nil "~%") nil)
                                          (preview-blocks item-blocks (1+ level) :tight t :quote quote))))))))
               ;; Space between blocks, but not between a tight list's paragraphs.
               (unless (or (and tight (member (first block) '(:paragraph :list))) (eq (first block) :code)
                           (and (eq (first block) :list) more (eq (first (first more)) :list)))
                 (preview-insert (format nil "~%") (list "md-p-gap")))
               (when (eq (first block) :code)
                 (preview-insert (format nil "~%") (list "md-p-gap")))))))

(defun render-preview (preview)
  "Draw PREVIEW (a buffer) again from its source, keeping its scroll position."
  (let* ((source (buffer-local preview :preview-of))
         (gtk-buffer (buffer-text preview))
         (view (first (and *window* (buffer-views *window* preview))))
         (adjustment (and view (gtk:scrolled-window-get-vadjustment (view-widget view))))
         (scroll (and adjustment (gtk:adjustment-get-value adjustment))))
    (when (and source (member source (buffer-list)))
      (ensure-preview-tags gtk-buffer)
      (let ((*preview* (list gtk-buffer '() '()
                             (and (buffer-file source) (uiop:pathname-directory-pathname (buffer-file source))))))
        (gtk:text-buffer-set-text gtk-buffer "" -1)
        (handler-case (preview-blocks (markdown-blocks (buffer-string source)) 0)
          (error (e) (preview-insert (format nil "Couldn't show the preview: ~a~%" e) nil)))
        (gtk:text-buffer-apply-tag-by-name gtk-buffer "md-p-body" (gtk:text-buffer-get-start-iter gtk-buffer)
                                           (gtk:text-buffer-get-end-iter gtk-buffer))
        (setf (buffer-local preview :links) (second *preview*)
              (buffer-local preview :block-lines) (sort (third *preview*) #'< :key #'car)))
      (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer))
      (setf (buffer-modified-p preview) nil)
      (when scroll
        (glib:idle-add glib:+priority-default-idle+
                       (lambda () (gtk:adjustment-set-value adjustment scroll) nil))))))

(defun schedule-preview (preview)
  (let ((timer (buffer-local preview :render-timer)))
    (when timer (glib:source-remove timer))
    (setf (buffer-local preview :render-timer)
          (glib:timeout-add glib:+priority-default+ 250
                            (lambda ()
                              (setf (buffer-local preview :render-timer) nil)
                              (when (member preview (buffer-list)) (render-preview preview))
                              nil)))))

;;; The preview: following links and scrolling

(defun open-link (preview url)
  "Follow URL from PREVIEW: web links in the browser, #anchors to the
heading, other paths as files beside the source."
  (let ((source (buffer-local preview :preview-of)))
    (cond ((or (search "://" url) (string-equal "mailto:" url :end2 (min 7 (length url))))
           (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new url) (window-gtk-window *window*))
                      (lambda (ok) (declare (ignore ok)))
                      :error (lambda (e) (message "Couldn't open ~a: ~a" url e))))
          ((and (plusp (length url)) (char= (char url 0) #\#))
           (let* ((anchor (subseq url 1))
                  (heading (find anchor (markdown-headings (buffer-string source))
                                 :key (lambda (h) (heading-anchor (second h))) :test #'string=)))
             (if heading
                 (scroll-preview-to-line preview (third heading))
                 (message "No heading ~a" url))))
          ((and source (buffer-file source))
           (let* ((file (subseq url 0 (or (position #\# url) (length url))))
                  (path (merge-pathnames file (uiop:pathname-directory-pathname (buffer-file source)))))
             (if (probe-file path)
                 (open-file-path path)
                 (message "No file ~a" file))))
          (t (message "Can't open ~a" url)))))

(defun heading-anchor (title)
  "The #anchor GitHub gives a heading titled TITLE."
  (with-output-to-string (out)
    (loop for c across (string-downcase title)
          do (cond ((or (alphanumericp c) (char= c #\-) (char= c #\_)) (write-char c out))
                   ((char= c #\Space) (write-char #\- out))))))

(defun link-at (preview offset)
  (find-if (lambda (link) (<= (first link) offset (1- (second link)))) (buffer-local preview :links)))

(defun preview-offset-at (view x y)
  (let ((text-view (view-text-view view)))
    (multiple-value-bind (bx by) (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
      (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
        (and ok (gtk:text-iter-get-offset iter))))))

(defun preview-anchors (preview)
  "PREVIEW's blocks as (source line . offset), by line, one per line."
  (let ((anchors '()))
    (dolist (e (buffer-local preview :block-lines))
      (let ((same (assoc (car e) anchors)))
        (if same
            (setf (cdr same) (min (cdr same) (cdr e)))
            (push (cons (car e) (cdr e)) anchors))))
    (sort anchors #'< :key #'car)))

(defun preview-y-for-line (view preview line)
  "The y in VIEW (of PREVIEW) that matches source LINE (fractional): between
the blocks before and after it, in proportion."
  (let* ((text-view (view-text-view view))
         (gtk-buffer (view-gtk-buffer view))
         (anchors (preview-anchors preview))
         (before (let ((best nil)) (dolist (a anchors best) (when (<= (car a) line) (setf best a)))))
         (after (find-if (lambda (a) (> (car a) line)) anchors)))
    (flet ((y-of (anchor) (if anchor
                              (values (gtk:text-view-get-line-yrange text-view (iter-at gtk-buffer (cdr anchor))))
                              0)))
      (cond ((and before after)
             (let ((y1 (y-of before)) (y2 (y-of after)))
               (+ y1 (* (- y2 y1) (/ (- line (car before)) (- (car after) (car before)))))))
            (before (y-of before))
            (t 0)))))

(defun source-top-line (view)
  "The source line at the top of VIEW, with the fraction of it scrolled past."
  (let* ((text-view (view-text-view view))
         (top (gtk:adjustment-get-value (gtk:scrolled-window-get-vadjustment (view-widget view))))
         (iter (gtk:text-view-get-line-at-y text-view (round top))))
    (if (<= top 0)
        0
        (multiple-value-bind (y height) (gtk:text-view-get-line-yrange text-view iter)
          (+ (gtk:text-iter-get-line iter)
             (if (and height (plusp height)) (max 0 (min 1 (/ (- top y) height))) 0))))))

(defun scroll-preview-to-line (preview line &key at-end)
  "Scroll PREVIEW's views, only up and down, to match source LINE; AT-END
means the source is scrolled to its end, so the preview goes to its end."
  (dolist (view (buffer-views *window* preview))
    (let ((vertical (gtk:scrolled-window-get-vadjustment (view-widget view)))
          (horizontal (gtk:scrolled-window-get-hadjustment (view-widget view))))
      (gtk:adjustment-set-value horizontal 0d0)
      (gtk:adjustment-set-value vertical
                                (float (if at-end
                                           (gtk:adjustment-get-upper vertical)
                                           (preview-y-for-line view preview line))
                                       1d0)))))

(defun preview-source-scrolled (view)
  "VIEW, of a Markdown buffer with a preview, scrolled: scroll the preview to match."
  (let ((preview (buffer-local (view-buffer view) :preview)))
    (when (and *markdown-preview-scroll-sync* preview (member preview (buffer-list))
               (not (buffer-local preview :sync-pending)))
      (setf (buffer-local preview :sync-pending) t)
      (glib:idle-add glib:+priority-default-idle+
                     (lambda ()
                       (setf (buffer-local preview :sync-pending) nil)
                       (when (member preview (buffer-list))
                         (let ((adjustment (gtk:scrolled-window-get-vadjustment (view-widget view))))
                           (scroll-preview-to-line
                            preview (source-top-line view)
                            :at-end (and (> (gtk:adjustment-get-value adjustment) 0)
                                         (>= (+ (gtk:adjustment-get-value adjustment) (gtk:adjustment-get-page-size adjustment))
                                             (- (gtk:adjustment-get-upper adjustment) 1))))))
                       nil)))))

(defun setup-preview-view (view)
  "Make VIEW, of a preview buffer, look like a page: read-only, wrapped,
proportional text, with links that open when clicked."
  (let ((text-view (view-text-view view))
        (preview (view-buffer view)))
    (gtk:text-view-set-editable text-view nil)
    (gtk:text-view-set-cursor-visible text-view nil)
    (gtk:text-view-set-monospace text-view nil)
    (gtk:text-view-set-wrap-mode text-view :word-char)
    (gtk:text-view-set-left-margin text-view 16)
    (gtk:text-view-set-right-margin text-view 24)
    (gtk:text-view-set-top-margin text-view 12)
    (gtk:widget-add-css-class text-view "cadre-markdown-preview")
    (let ((click (gtk:gesture-click-new)))
      (gtk:gesture-single-set-button click 1)
      (gobject:connect click :released
                       (lambda (gesture n x y)
                         (declare (ignore gesture n))
                         (let* ((offset (preview-offset-at view x y))
                                (link (and offset (link-at preview offset))))
                           (when link (open-link preview (third link))))))
      (gtk:widget-add-controller text-view click))
    (let ((motion (gtk:event-controller-motion-new)))
      (gobject:connect motion :motion
                       (lambda (controller x y)
                         (declare (ignore controller))
                         (let ((offset (preview-offset-at view x y)))
                           (gtk:widget-set-cursor-from-name text-view
                                                            (if (and offset (link-at preview offset)) "pointer" "text")))))
      (gtk:widget-add-controller text-view motion))))

;;; Opening the preview

(defun make-preview-buffer (source)
  (let ((preview (make-buffer :name (format nil "Preview ~a" (buffer-name source)) :text (make-gtk-text))))
    (setf (buffer-local preview :preview-of) source
          (buffer-local source :preview) preview
          (buffer-local preview :source-handler)
          (gobject:connect (buffer-text source) :changed
                           (lambda (b) (declare (ignore b))
                             (when (member preview (buffer-list)) (schedule-preview preview)))))
    (render-preview preview)
    preview))

(defun forget-preview (buffer)
  "When a preview or its source is killed, unhook them from each other."
  (let ((source (buffer-local buffer :preview-of))
        (preview (buffer-local buffer :preview)))
    (when (and source (eq (buffer-local source :preview) buffer))
      (let ((handler (buffer-local buffer :source-handler)))
        (when handler (gobject:disconnect (buffer-text source) handler)))
      (setf (buffer-local source :preview) nil))
    (when (and preview (member preview (buffer-list)))
      (setf (buffer-local preview :preview-of) nil)
      (dolist (view (buffer-views *window* preview)) (close-view *window* view)))
    ;; A source opened only for its preview goes when the preview does.
    (when (and source (member source (buffer-list)) (null (buffer-views *window* source))
               (not (buffer-modified-p source)))
      (kill-buffer source))))

(add-hook '*buffer-killed-hook* 'forget-preview)

(defun preview-group (win group)
  "Another editor group than GROUP, made by splitting to the right if there's none."
  (or (find-if-not (lambda (g) (eq g group)) (groups-in-order win))
      (let ((new (split-group win group :right)))
        ;; The split shows a copy of GROUP's tab; the preview takes its place.
        (dolist (view (loop for page in (group-pages new)
                            for v = (page-view win page) when v collect v))
          (close-view win view))
        new)))

(defun open-markdown-preview (path &key (group (window-active-group *window*)))
  "Show a preview of the Markdown file PATH in GROUP, without opening the file in an editor."
  (let ((source (load-file-buffer path)))
    (unless source (editor-error "Can't read ~a" (file-namestring path)))
    (let* ((existing (buffer-local source :preview))
           (preview (if (and existing (member existing (buffer-list))) existing (make-preview-buffer source))))
      (show-buffer *window* preview :group group))))

(define-command markdown-preview ()
  "Show this Markdown file rendered, beside it, updated as you type."
  (:modes markdown-mode)
  (let* ((win *window*)
         (view (current-view))
         (source (view-buffer view))
         (existing (buffer-local source :preview))
         (preview (if (and existing (member existing (buffer-list))) existing (make-preview-buffer source)))
         (shown (first (buffer-views win preview))))
    (if shown
        (show-buffer win preview :group (view-group shown) :focus nil)
        (show-buffer win preview :group (preview-group win (view-group view)) :focus nil))
    (activate-group win (view-group view))
    (focus-view view)
    ;; Now, and again once both views have laid out their text.
    (preview-source-scrolled view)
    (glib:timeout-add glib:+priority-default+ 400
                      (lambda () (when (member source (buffer-list)) (preview-source-scrolled view)) nil))))

;;; Editing

(defun current-line-text (view)
  (multiple-value-bind (line) (cursor-line-column view)
    (text-line-string (view-gtk-buffer view) line)))

(define-command markdown-newline ()
  "Start a new line, continuing the list item or quote the cursor is in. On
an empty item, end the list instead."
  (:modes markdown-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (let ((before (subseq (text-line-string gtk-buffer line) 0 column)))
        (multiple-value-bind (prefix empty) (markdown-continuation before)
          (with-user-action (gtk-buffer)
            (cond ((and prefix empty)
                   ;; An empty item: take its marker away and stop the list.
                   (gtk:text-buffer-delete gtk-buffer (line-iter gtk-buffer line) (line-iter gtk-buffer line column)))
                  (prefix (gtk:text-buffer-insert-at-cursor gtk-buffer (format nil "~%~a" prefix) -1))
                  (t (gtk:text-buffer-insert-at-cursor
                      gtk-buffer (format nil "~%~a" (subseq before 0 (leading-spaces-of before))) -1)))))
        (scroll-to-cursor view)))))

(defun leading-spaces-of (string)
  (or (position #\Space string :test-not #'char=) (length string)))

(defun list-line-p (string)
  (let ((s (string-left-trim " " string)))
    (or (and (plusp (length s)) (member (char s 0) '(#\- #\* #\+))
             (or (= 1 (length s)) (char= (char s 1) #\Space)))
        (let ((e (position-if-not #'digit-char-p s)))
          (and e (plusp e) (< e (length s)) (member (char s e) '(#\. #\)))
               (or (= (1+ e) (length s)) (char= (char s (1+ e)) #\Space)))))))

(define-command markdown-indent ()
  "Indent the list item on this line (or every selected line's), or insert spaces."
  (:modes markdown-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (first last) (selected-lines view)
      (if (list-line-p (text-line-string gtk-buffer first))
          (with-user-action (gtk-buffer)
            (loop for line from first to last
                  unless (string= "" (text-line-string gtk-buffer line))
                    do (gtk:text-buffer-insert gtk-buffer (line-iter gtk-buffer line) "  " -1)))
          (multiple-value-bind (line column) (cursor-line-column view)
            (declare (ignore line))
            (gtk:text-buffer-insert-at-cursor gtk-buffer (make-string (- 4 (mod column 4)) :initial-element #\Space) -1))))))

(define-command markdown-outdent ()
  "Outdent the list item on this line (or every selected line's)."
  (:modes markdown-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (first last) (selected-lines view)
      (with-user-action (gtk-buffer)
        (loop for line from first to last
              for n = (min 2 (leading-spaces-of (text-line-string gtk-buffer line)))
              when (plusp n)
                do (gtk:text-buffer-delete gtk-buffer (line-iter gtk-buffer line) (line-iter gtk-buffer line n)))))))

(defun selected-lines (view)
  "The first and last lines of the selection, or the cursor's line twice."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (if has
          (values (gtk:text-iter-get-line start)
                  (let ((l (gtk:text-iter-get-line end)))
                    (if (and (> l (gtk:text-iter-get-line start)) (zerop (gtk:text-iter-get-line-offset end))) (1- l) l)))
          (let ((l (cursor-line-column view))) (values l l))))))

(defun word-bounds-at-cursor (view)
  (let* ((gtk-buffer (view-gtk-buffer view))
         (text (text-string gtk-buffer))
         (point (point-offset view))
         (start (loop for i downfrom point while (and (> i 0) (alphanumericp (char text (1- i)))) finally (return i)))
         (end (loop for i from point while (and (< i (length text)) (alphanumericp (char text i))) finally (return i))))
    (values start end)))

(defun toggle-markup (open &optional (close open))
  "Put OPEN and CLOSE around the selection or the word at the cursor, or
take them away if they're there already. With nothing to wrap, insert
both and put the cursor between."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (text (text-string gtk-buffer)))
    (multiple-value-bind (start end)
        (multiple-value-bind (has s e) (gtk:text-buffer-get-selection-bounds gtk-buffer)
          (if has (values (gtk:text-iter-get-offset s) (gtk:text-iter-get-offset e)) (word-bounds-at-cursor view)))
      (with-user-action (gtk-buffer)
        (cond ((and (>= start (length open)) (<= (+ end (length close)) (length text))
                    (string= open text :start2 (- start (length open)) :end2 start)
                    (string= close text :start2 end :end2 (+ end (length close))))
               (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer end) (iter-at gtk-buffer (+ end (length close))))
               (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer (- start (length open))) (iter-at gtk-buffer start))
               (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer (- start (length open)))
                                             (iter-at gtk-buffer (- end (length open)))))
              (t
               (insert-text-at gtk-buffer end close)
               (insert-text-at gtk-buffer start open)
               (if (= start end)
                   (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (+ start (length open))))
                   (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer (+ start (length open)))
                                                 (iter-at gtk-buffer (+ end (length open)))))))))))

(define-command markdown-bold ()
  "Make the selection or the word at the cursor bold (or not)."
  (:modes markdown-mode)
  (toggle-markup "**"))

(define-command markdown-italic ()
  "Make the selection or the word at the cursor italic (or not)."
  (:modes markdown-mode)
  (toggle-markup "*"))

(define-command markdown-code ()
  "Make the selection or the word at the cursor code (or not)."
  (:modes markdown-mode)
  (toggle-markup "`"))

(define-command markdown-strikethrough ()
  "Strike through the selection or the word at the cursor (or not)."
  (:modes markdown-mode)
  (toggle-markup "~~"))

(define-command markdown-link ()
  "Make the selection a link and put the cursor where the URL goes (or, if
the selection is a URL, where the text goes)."
  (:modes markdown-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has s e) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (let* ((start (if has (gtk:text-iter-get-offset s) (point-offset view)))
             (end (if has (gtk:text-iter-get-offset e) start))
             (selected (text-string gtk-buffer start end))
             (url (search "://" selected)))
        (with-user-action (gtk-buffer)
          (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer start) (iter-at gtk-buffer end))
          (insert-text-at gtk-buffer start (if url (format nil "[](~a)" selected) (format nil "[~a]()" selected)))
          (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (if url (1+ start) (+ start (length selected) 3)))))))))

(define-command markdown-goto-heading ()
  "Go to a heading in this Markdown file."
  (:modes markdown-mode)
  (let* ((view (current-view))
         (headings (markdown-headings (text-string (view-gtk-buffer view)))))
    (unless headings (editor-error "No headings"))
    (open-picker (window-picker *window*)
                 :items headings
                 :label (lambda (h) (format nil "~v@{  ~}~a" (1- (first h)) nil (second h)))
                 :detail (lambda (h) (format nil "line ~d" (1+ (third h))))
                 :placeholder "Go to a heading"
                 :on-choose (lambda (h)
                              (goto-line-column view (third h) 0)
                              (focus-view view)))))

(defparameter *markdown-wrap-pairs* '(("*" . "*") ("_" . "_") ("~" . "~"))
  "Pairs that wrap the selection in Markdown, besides brackets and quotes.")

(defun markdown-context-menu (menu)
  "Add Markdown's commands to MENU, a right-click menu."
  (let ((section (gio:menu-new)))
    (command-item section "Open Preview to the Side" 'markdown-preview)
    (command-item section "Go to Heading…" 'markdown-goto-heading)
    (gio:menu-append-section menu nil section))
  (let ((format (gio:menu-new)))
    (command-item format "Bold" 'markdown-bold)
    (command-item format "Italic" 'markdown-italic)
    (command-item format "Code" 'markdown-code)
    (command-item format "Strikethrough" 'markdown-strikethrough)
    (command-item format "Link" 'markdown-link)
    (gio:menu-append-submenu menu "Format" format)))
