;;;; hyperspec.lisp — the Common Lisp standard (and library docs) in a tab
;;;;
;;;; Look Up in HyperSpec (in the right-click menu of Lisp code) shows the
;;;; standard's entry for the symbol at the cursor in a HyperSpec tab beside
;;;; the code; Search HyperSpec picks any entry by name. The text is the
;;;; Common Lisp docset (core's docset.lisp, data/docsets/common-lisp/,
;;;; made from the dpANS3 draft of the standard by spec-browser); gtk4's and
;;;; raylib's docsets show too when they're built. Docsets are opened on
;;;; the first lookup.
;;;;
;;;; The tab is one read-only buffer drawn like the Markdown preview:
;;;; headings, syntax lines, Lisp-highlighted examples, aligned tables, and
;;;; links to other entries that open in the same tab. It keeps a history:
;;;; Back and Forward at the top of the page, Alt+Left and Alt+Right (and
;;;; Backspace), or the mouse's back and forward buttons.

(in-package #:cadre-ui)

(define-major-mode hyperspec-mode (:title "HyperSpec")
  "A page of the Common Lisp standard or a library's documentation.")

;;; The docsets

(defvar *docsets* :unread
  "The docsets the HyperSpec tab can show, opened on first use.")

(defun docsets ()
  (when (eq *docsets* :unread)
    (setf *docsets* (cadre-docset:find-docsets :directories (list (resource-pathname "data/docsets/")))))
  *docsets*)

(defun find-docset (id)
  (find id (docsets) :key #'cadre-docset:docset-id :test #'equal))

;;; The page: what the tab shows, and where it has been

(defstruct (spec-page (:conc-name sp-))
  docset-id entry-id
  (links '())     ; (start end target) in the buffer
  (back '())      ; (docset-id . entry-id), most recent first
  (forward '()))

(defun hyperspec-buffer-p (buffer) (and (buffer-local buffer :hyperspec) t))

(defun hyperspec-buffer ()
  "The HyperSpec tab's buffer, made if there's none."
  (or (find-if #'hyperspec-buffer-p (buffer-list))
      (let ((buffer (make-buffer :name "HyperSpec" :text (make-gtk-text) :major-mode 'hyperspec-mode)))
        (setf (buffer-local buffer :hyperspec) (make-spec-page))
        buffer)))

;;; Tags. Layout tags are made here; colors come from theme faces
;;; (FACE-TAG), so a theme change recolors the page without drawing it again.

(defun ensure-hyperspec-tags (gtk-buffer)
  (unless (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) "hs-body")
    (ensure-tags gtk-buffer)
    ;; Made first, so every other tag (code's Monospace) wins over it.
    (preview-tag gtk-buffer "hs-body" :family "Sans" :scale 1.1d0 :pixels-below-lines 2)
    ;; Margins before code and syntax lines, whose own margins win.
    (loop for level from 0 to 4
          do (margin-tag gtk-buffer level) (margin-tag gtk-buffer level :hang t))
    (preview-tag gtk-buffer "hs-title" :scale 1.8d0 :weight 700 :pixels-below-lines 6)
    (preview-tag gtk-buffer "hs-kind" :style :italic)
    (preview-tag gtk-buffer "hs-heading" :scale 1.3d0 :weight 700 :pixels-above-lines 14 :pixels-below-lines 4)
    (loop for level from 1 to 4
          do (preview-tag gtk-buffer (format nil "hs-h~d" level)
                          :scale (aref *preview-heading-scales* level) :weight 700
                          :pixels-above-lines 10 :pixels-below-lines 4))
    (preview-tag gtk-buffer "hs-code" :family "Monospace")
    (preview-tag gtk-buffer "hs-codeblock" :family "Monospace" :left-margin 28 :right-margin 16
                                           :pixels-above-lines 0 :pixels-below-lines 0 :wrap-mode :char)
    (preview-tag gtk-buffer "hs-syntax" :family "Monospace" :left-margin 48 :indent -20)
    (preview-tag gtk-buffer "hs-table" :family "Monospace" :wrap-mode :none)
    (preview-tag gtk-buffer "hs-italic" :style :italic)
    (preview-tag gtk-buffer "hs-bold" :weight 700)
    (preview-tag gtk-buffer "hs-sup" :rise 4000 :scale 0.8d0)
    (preview-tag gtk-buffer "hs-sub" :rise -3000 :scale 0.8d0)
    (preview-tag gtk-buffer "hs-term")
    (preview-tag gtk-buffer "hs-nav" :scale 0.9d0 :pixels-below-lines 8)
    (preview-tag gtk-buffer "hs-gap" :scale 0.45d0)
    (restyle-hyperspec-tags gtk-buffer)))

(defun restyle-hyperspec-tags (gtk-buffer)
  "Glossary terms are links too, but there are so many that they're only colored."
  (let ((term (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) "hs-term"))
        (color (theme-color :md-link :foreground)))
    (when (and term color)
      (setf (gobject:property term :foreground) color))))

(defparameter *hyperspec-styles*
  '((:title "hs-title" :md-heading) (:kind "hs-kind" :comment)
    (:heading "hs-heading" :md-heading)
    (:heading-1 "hs-h1" :md-heading) (:heading-2 "hs-h2" :md-heading)
    (:heading-3 "hs-h3" :md-heading) (:heading-4 "hs-h4" :md-heading)
    (:code "hs-code" :md-code) (:code-block "hs-codeblock" :md-code-block)
    (:syntax "hs-syntax") (:syntax-name "hs-bold" :definition-name)
    (:param "hs-italic" :definition-name) (:italic "hs-italic") (:bold "hs-bold")
    (:sup "hs-sup") (:sub "hs-sub")
    (:link nil :md-link) (:term "hs-term") (:note "hs-italic" :comment)
    (:quote nil :md-quote) (:marker nil :md-list) (:table "hs-table")
    (:table-header "hs-bold") (:nav "hs-nav" :md-link) (:attribution "hs-italic" :comment) (:gap "hs-gap"))
  "Each style the page uses: its layout tag and the theme face coloring it.")

;;; Drawing

(defvar *page* nil "While drawing: (gtk-buffer docset-id links).")

(defun page-tags (styles)
  "The tags for STYLES: style keywords, or tags (such as margins) as they are."
  (let ((gtk-buffer (first *page*)))
    (loop for style in styles
          append (if (keywordp style)
                     (destructuring-bind (&optional name face) (rest (assoc style *hyperspec-styles*))
                       (remove nil (list (and name (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer) name))
                                         (and face (face-tag gtk-buffer face)))))
                     (list style)))))

(defun page-end () (gtk:text-iter-get-offset (gtk:text-buffer-get-end-iter (first *page*))))

(defun page-insert (text styles)
  "Insert TEXT at the end of the page with STYLES. Returns where it starts."
  (let* ((gtk-buffer (first *page*))
         (start (page-end)))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) text -1)
    (let ((s (iter-at gtk-buffer start))
          (e (gtk:text-buffer-get-end-iter gtk-buffer)))
      (dolist (tag (page-tags styles))
        (gtk:text-buffer-apply-tag gtk-buffer tag s e)))
    start))

(defun page-link (start target)
  (push (list start (page-end) target) (third *page*)))

(defun inline-text (inline)
  "INLINE's text, without its markup."
  (cond ((stringp inline) inline)
        ((atom inline) "")
        ((member (first inline) '(:ref :term :link :secref))
         (format nil "~{~a~}" (mapcar #'inline-text (cddr inline))))
        (t (format nil "~{~a~}" (mapcar #'inline-text (rest inline))))))

(defun page-inlines (inlines styles)
  (dolist (inline inlines) (page-inline inline styles)))

(defun page-inline (inline styles)
  (if (stringp inline)
      (page-insert inline styles)
      (let ((start (page-end)))
        (case (first inline)
          (:ref (page-inlines (cddr inline) (cons :link styles))
           (page-link start (list :entry (second *page*) (second inline))))
          (:term (if (second inline)
                     (progn (page-inlines (cddr inline) (cons :term styles))
                            (page-link start (list :entry (second *page*) (second inline))))
                     (page-inlines (cddr inline) styles)))
          (:link (page-inlines (cddr inline) (cons :link styles))
           (page-link start (list :url (second inline))))
          (:secref (page-inlines (cddr inline) styles))
          (:code (page-inlines (rest inline) (cons :code styles)))
          (:param (page-inlines (rest inline) (cons :param styles)))
          ((:i :math) (page-inlines (rest inline) (cons :italic styles)))
          ((:b :input) (page-inlines (rest inline) (cons :bold styles)))
          (:sup (page-inlines (rest inline) (cons :sup styles)))
          ((:sub :sense) (page-inlines (rest inline) (cons :sub styles)))
          (:note (page-inlines (rest inline) (cons :note styles)))
          (t (page-inlines (rest inline) styles))))))

(defun highlight-page-code (start)
  "Highlight the Lisp from START to the end of the page."
  (let* ((gtk-buffer (first *page*))
         (state '(:code 0))
         (offset start))
    (dolist (line (split-text-lines (text-string gtk-buffer start (page-end))))
      (multiple-value-bind (tokens end) (lex-line line state)
        (setf state end)
        (dolist (tk tokens)
          (let ((face (token-face tk line)))
            ;; Parens keep the code block's color; the rest is colored as in an editor.
            (when (and face (not (consp face)))
              (gtk:text-buffer-apply-tag gtk-buffer (face-tag gtk-buffer face)
                                         (iter-at gtk-buffer (+ offset (token-start tk)))
                                         (iter-at gtk-buffer (+ offset (token-end tk))))))))
      (incf offset (1+ (length line))))))

(defun page-code-block (language inlines styles)
  (let ((start (page-end)))
    (page-inlines inlines (cons :code-block styles))
    (page-insert (format nil "~%") (cons :code-block styles))
    (when (or (null language) (lisp-language-p language))
      (highlight-page-code start))))

(defun page-syntax (plist styles)
  (let ((styles (cons :syntax styles)))
    (when (getf plist :setf) (page-insert "(setf (" styles))
    (page-inlines (getf plist :name) (cons :syntax-name styles))
    (when (getf plist :args)
      (page-insert " " styles)
      (page-inlines (getf plist :args) styles))
    (when (getf plist :setf) (page-insert ") " styles))
    (when (getf plist :values)
      (page-insert (if (getf plist :setf) "" " ⇒ ") styles)
      (page-inlines (getf plist :values) styles))
    (when (getf plist :setf) (page-insert ")" styles))
    (page-insert (format nil "~%") styles)))

(defun page-table (plist styles)
  "A table as aligned columns of monospaced text, keeping its links."
  (let* ((header (getf plist :header))
         (rows (getf plist :rows))
         (all (if header (cons header rows) rows))
         (columns (reduce #'max all :key #'length :initial-value 0))
         (widths (loop for c below columns
                       collect (reduce #'max all :initial-value 1
                                                 :key (lambda (row) (length (inline-text (cons :x (nth c row)))))))))
    (when (getf plist :caption)
      (page-inlines (getf plist :caption) (cons :italic styles))
      (page-insert (format nil "~%") styles))
    (flet ((row (cells styles)
             (loop for c below columns
                   for cell = (nth c cells)
                   for pad = (- (nth c widths) (length (inline-text (cons :x cell))))
                   do (page-inlines cell (cons :table styles))
                      (when (< c (1- columns))
                        (page-insert (make-string (+ pad 3) :initial-element #\Space) (cons :table styles))))
             (page-insert (format nil "~%") (cons :table styles))))
      (when header
        (row header (cons :table-header styles))
        (page-insert (format nil "~{~a~^───~}~%" (mapcar (lambda (w) (make-string w :initial-element #\─)) widths))
                     (list* :table :marker styles)))
      (dolist (r rows) (row r styles)))))

(defun page-blocks (blocks level styles)
  "Insert BLOCKS indented LEVEL steps, with a little space after each."
  (dolist (block blocks)
    (page-block block level styles)
    (page-insert (format nil "~%") '(:gap))))

(defun page-block (block level styles)
  (let* ((gtk-buffer (first *page*))
         (styles (cons (margin-tag gtk-buffer level) styles)))
    (case (first block)
      (:p (page-inlines (rest block) styles)
       (page-insert (format nil "~%") styles))
      (:heading (page-inlines (cddr block) (cons (case (second block)
                                                   (1 :heading-1) (2 :heading-2) (3 :heading-3) (t :heading-4))
                                                 styles))
       (page-insert (format nil "~%") styles))
      (:code-block (page-code-block (second block) (cddr block) styles))
      (:syntax (page-syntax (rest block) styles))
      (:table (page-table (rest block) styles))
      (:quote (page-blocks (rest block) (1+ level) (cons :quote styles)))
      (:list
       (dolist (item (rest block))
         (let* ((parts (rest item))
                (label (and (consp (first parts)) (eq (first (first parts)) :label) (pop parts)))
                (hang (margin-tag gtk-buffer (1+ level) :hang t)))
           (if label
               (page-inlines (rest label) (list hang :bold))
               (page-insert (if (evenp level) "•" "◦") (list hang :marker)))
           (page-insert "  " (list hang))
           ;; The first paragraph goes beside the marker, the rest below it.
           (if (and parts (eq (first (first parts)) :p))
               (progn (page-inlines (rest (first parts)) (cons hang (rest styles)))
                      (page-insert (format nil "~%") (list hang))
                      (page-blocks-tight (rest parts) (1+ level) (rest styles)))
               (progn (page-insert (format nil "~%") (list hang))
                      (page-blocks-tight parts (1+ level) (rest styles)))))))
      (t ;; A kind of block this reader doesn't know: its text.
       (page-insert (format nil "~a~%" (inline-text (cons :x (remove-if-not #'stringp (rest block))))) styles)))))

(defun page-blocks-tight (blocks level styles)
  (dolist (block blocks) (page-block block level styles)))

(defun draw-hyperspec-page (buffer docset entry)
  "Draw ENTRY of DOCSET in BUFFER, the HyperSpec tab's."
  (let* ((gtk-buffer (buffer-text buffer))
         (page (buffer-local buffer :hyperspec))
         (*page* (list gtk-buffer (cadre-docset:docset-id docset) '())))
    (ensure-hyperspec-tags gtk-buffer)
    (gtk:text-buffer-set-text gtk-buffer "" -1)
    ;; Back and Forward, when there's somewhere to go.
    (when (or (sp-back page) (sp-forward page))
      (when (sp-back page)
        (page-link (page-insert "← Back" '(:nav)) '(:back)))
      (when (and (sp-back page) (sp-forward page)) (page-insert "     " '(:nav)))
      (when (sp-forward page)
        (page-link (page-insert "Forward →" '(:nav)) '(:forward)))
      (page-insert (format nil "~%") '(:nav)))
    (page-insert (format nil "~{~a~^, ~}" (cadre-docset:entry-names entry)) '(:title))
    (let ((kind (cadre-docset:entry-kind-name entry)))
      (when kind (page-insert (format nil "   ~a" kind) '(:kind))))
    (page-insert (format nil "~%") nil)
    (loop for (label . blocks) in (cadre-docset:entry-sections entry)
          do (page-insert (format nil "~a~%" label) '(:heading))
             (page-blocks blocks 0 nil))
    (page-insert (format nil "~%~a — ~a~%" (cadre-docset:docset-title docset)
                         (or (getf (cadre-docset:docset-manifest docset) :subtitle) ""))
                 '(:attribution))
    (gtk:text-buffer-apply-tag-by-name gtk-buffer "hs-body" (gtk:text-buffer-get-start-iter gtk-buffer)
                                       (gtk:text-buffer-get-end-iter gtk-buffer))
    (setf (sp-links page) (third *page*))
    (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer))
    (setf (buffer-modified-p buffer) nil)))

;;; Showing an entry

(defun hyperspec-group (win)
  "Where the HyperSpec tab goes: where it is already, else beside the code."
  (let ((shown (first (buffer-views win (hyperspec-buffer))))
        (group (window-active-group win)))
    (cond (shown (view-group shown))
          ((null (group-pages group)) group)
          (t (preview-group win group)))))

(defun show-hyperspec-entry (docset-id entry-id &key (record t) focus)
  "Show the entry ENTRY-ID of the docset DOCSET-ID in the HyperSpec tab.
RECORD puts the entry shown now in the history, so Back returns to it."
  (let* ((docset (or (find-docset docset-id) (editor-error "No docset ~a" docset-id)))
         (entry (or (cadre-docset:docset-entry docset entry-id) (editor-error "No entry ~a" entry-id)))
         (win *window*)
         (from (and win (current-view-or-nil win)))
         (buffer (hyperspec-buffer))
         (page (buffer-local buffer :hyperspec)))
    (when (and record (sp-entry-id page)
               (not (and (equal docset-id (sp-docset-id page)) (equal entry-id (sp-entry-id page)))))
      (push (cons (sp-docset-id page) (sp-entry-id page)) (sp-back page))
      (setf (sp-forward page) '()))
    (setf (sp-docset-id page) docset-id
          (sp-entry-id page) entry-id)
    (draw-hyperspec-page buffer docset entry)
    (setf (buffer-name buffer) (format nil "HyperSpec: ~a" (first (cadre-docset:entry-names entry))))
    (when win
      (let ((view (show-buffer win buffer :group (hyperspec-group win) :focus focus)))
        (update-tab-titles win buffer)
        (dolist (v (buffer-views win buffer))
          (gtk:adjustment-set-value (gtk:scrolled-window-get-vadjustment (view-widget v)) 0d0))
        ;; Looked up from code: the code keeps the keyboard.
        (unless focus
          (when (and from (not (eq from view)))
            (activate-group win (view-group from))
            (focus-view from)))))))

(defun current-view-or-nil (win)
  (or (focused-view win) (selected-view win)))

(defun hyperspec-history-step (direction)
  (let* ((buffer (hyperspec-buffer))
         (page (buffer-local buffer :hyperspec))
         (here (and (sp-entry-id page) (cons (sp-docset-id page) (sp-entry-id page)))))
    (multiple-value-bind (from to) (if (eq direction :back)
                                       (values (sp-back page) (sp-forward page))
                                       (values (sp-forward page) (sp-back page)))
      (unless from (editor-error (if (eq direction :back) "Nothing to go back to" "Nothing to go forward to")))
      (let ((target (first from)))
        (if (eq direction :back)
            (setf (sp-back page) (rest from) (sp-forward page) (if here (cons here to) to))
            (setf (sp-forward page) (rest from) (sp-back page) (if here (cons here to) to)))
        (show-hyperspec-entry (car target) (cdr target) :record nil :focus t)))))

(define-command hyperspec-back ()
  "Go back to the HyperSpec entry shown before this one."
  (hyperspec-history-step :back))

(define-command hyperspec-forward ()
  "Go forward again in the HyperSpec tab's history."
  (hyperspec-history-step :forward))

(defun follow-hyperspec-link (target)
  (ecase (first target)
    (:entry (show-hyperspec-entry (second target) (third target) :focus t))
    (:back (hyperspec-back))
    (:forward (hyperspec-forward))
    (:url (gio:async (gtk:uri-launcher-launch (gtk:uri-launcher-new (second target)) (window-gtk-window *window*))
                     (lambda (ok) (declare (ignore ok)))
                     :error (lambda (e) (message "Couldn't open ~a: ~a" (second target) e))))))

(defun hyperspec-link-at (buffer offset)
  (find-if (lambda (link) (<= (first link) offset (1- (second link))))
           (sp-links (buffer-local buffer :hyperspec))))

(defun setup-hyperspec-view (view)
  "Make VIEW, of the HyperSpec buffer, a page: read-only and wrapped, with
links that open when clicked and the mouse's back and forward buttons."
  (let ((text-view (view-text-view view))
        (buffer (view-buffer view)))
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
                         ;; Not when the press started a selection.
                         (unless (gtk:text-buffer-get-has-selection (view-gtk-buffer view))
                           (let* ((offset (preview-offset-at view x y))
                                  (link (and offset (hyperspec-link-at buffer offset))))
                             (when link
                               (handler-case (follow-hyperspec-link (third link))
                                 (editor-error (e) (message "~a" (editor-error-message e)))))))))
      (gtk:widget-add-controller text-view click))
    (let ((buttons (gtk:gesture-click-new)))
      (gtk:gesture-single-set-button buttons 0)
      (gobject:connect buttons :pressed
                       (lambda (gesture n x y)
                         (declare (ignore n x y))
                         (handler-case
                             (case (gtk:gesture-single-get-current-button gesture)
                               (8 (hyperspec-back))
                               (9 (hyperspec-forward)))
                           (editor-error (e) (message "~a" (editor-error-message e))))))
      (gtk:widget-add-controller text-view buttons))
    (let ((motion (gtk:event-controller-motion-new)))
      (gobject:connect motion :motion
                       (lambda (controller x y)
                         (declare (ignore controller))
                         (let ((offset (preview-offset-at view x y)))
                           (gtk:widget-set-cursor-from-name
                            text-view (if (and offset (hyperspec-link-at buffer offset)) "pointer" "text")))))
      (gtk:widget-add-controller text-view motion))))

;;; Looking up the symbol at the cursor

(defun split-symbol-name (text)
  "TEXT, as written in code (#'cl:mapcar, 'foo, :key), as its name and its
package prefix (nil when it has none)."
  (let* ((text (string-left-trim "#'`,@" text))
         (colon (position #\: text :from-end t)))
    (cond ((null colon) (values text nil))
          ((zerop colon) (values (subseq text 1) "KEYWORD"))
          (t (values (subseq text (1+ colon))
                     (string-upcase (string-right-trim ":" (subseq text 0 colon))))))))

(defun hyperspec-records (text)
  "The (docset . record)s for TEXT, a symbol as written: the standard's for
an unqualified name, a package's docs for a qualified one, and failing
those, anything by that name."
  (multiple-value-bind (name prefix) (split-symbol-name text)
    (let ((package (if (or (null prefix) (member prefix '("CL" "KEYWORD") :test #'string=))
                       "COMMON-LISP"
                       prefix)))
      (or (cadre-docset:lookup-symbol (docsets) name package)
          (loop for docset in (docsets)
                append (loop for record in (cadre-docset:find-records docset name)
                             collect (cons docset record)))))))

(defun record-label (pair)
  (first (cadre-docset:record-names (cdr pair))))

(defun record-detail (pair)
  (let ((record (cdr pair)))
    (format nil "~@[~a~]~:[~; — ~a~]" (cadre-docset:record-kind-name record)
            (rest (docsets)) (cadre-docset:docset-title (car pair)))))

(defun show-record (pair)
  (show-hyperspec-entry (cadre-docset:docset-id (car pair)) (cadre-docset:record-id (cdr pair))))

(define-command hyperspec-look-up ()
  "Show the Common Lisp standard's entry for the symbol at the cursor (or
the selection), in a HyperSpec tab beside the code."
  (let ((text (selection-or-symbol-text)))
    (unless text (editor-error "No symbol at the cursor"))
    (let* ((pairs (hyperspec-records text))
           (symbols (remove nil pairs :key (lambda (p) (cadre-docset:record-package (cdr p))))))
      (cond ((null pairs) (hyperspec-search-for (nth-value 0 (split-symbol-name text))))
            ((or (null symbols) (null (rest symbols))) (show-record (first pairs)))
            ;; LIST is a function and a class; FUNCTION an operator and a class.
            (t (open-picker (window-picker *window*)
                            :items symbols :label #'record-label :detail #'record-detail
                            :placeholder (format nil "Entries for ~a" text)
                            :on-choose #'show-record))))))

(defun hyperspec-search-for (text)
  (let ((items (loop for docset in (docsets)
                     append (loop for record in (cadre-docset:docset-index docset)
                                  append (loop for name in (cadre-docset:record-names record)
                                               collect (list* name docset record))))))
    (unless items (editor-error "No HyperSpec found in ~a" (uiop:native-namestring (resource-pathname "data/docsets/"))))
    (open-picker (window-picker *window*)
                 :items items
                 :label #'first
                 :detail (lambda (item) (record-detail (rest item)))
                 :placeholder "Search the HyperSpec"
                 :text (or text "")
                 :on-choose (lambda (item) (show-record (rest item))))))

(define-command hyperspec-search ()
  "Pick any entry of the Common Lisp standard (or a library's docs) by name."
  (hyperspec-search-for nil))

;;; Remembering the tab between sessions (restore.lisp)

(defun hyperspec-state (buffer)
  (let ((page (buffer-local buffer :hyperspec)))
    (and (sp-entry-id page) (list :hyperspec (sp-docset-id page) (sp-entry-id page)))))

(defun restore-hyperspec-buffer (docset-id entry-id)
  "The HyperSpec buffer showing ENTRY-ID, or nil if it can't be shown."
  (let* ((docset (find-docset docset-id))
         (entry (and docset (cadre-docset:docset-entry docset entry-id))))
    (when entry
      (let* ((buffer (hyperspec-buffer))
             (page (buffer-local buffer :hyperspec)))
        (setf (sp-docset-id page) docset-id
              (sp-entry-id page) entry-id)
        (draw-hyperspec-page buffer docset entry)
        (setf (buffer-name buffer) (format nil "HyperSpec: ~a" (first (cadre-docset:entry-names entry))))
        buffer))))
