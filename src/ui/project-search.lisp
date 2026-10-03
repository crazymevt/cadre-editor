;;;; project-search.lisp — find (and replace) in every file of the project,
;;;; and renaming a symbol everywhere
;;;;
;;;; The Search page of the sidebar. Searching runs on a thread over the
;;;; project's files (what the explorer shows), using open buffers' text for
;;;; open files, so unsaved changes count. Results are grouped by file;
;;;; clicking one opens it there. Replacing shows a check box on each match:
;;;; Replace All changes the checked ones, in open buffers as one undo step
;;;; each, and in other files on disk.
;;;;
;;;; Renaming a symbol uses the same page in symbol mode: occurrences come
;;;; from the lexer, so strings and comments are left alone and pkg:name
;;;; counts as name.

(in-package #:cadre-ui)

(defstruct (project-search (:conc-name ps-))
  widget entry replace-entry replace-revealer replace-toggle case-button word-button regex-button
  status results apply-button
  (mode :text)                          ; :text, or :symbol for renaming
  (symbol nil)
  (results-data '())                    ; (path match-rows), match-rows: (line start end check)
  (generation 0))

(defvar *project-search* nil)

(defparameter *max-project-matches* 5000)

(defun make-project-search-widget ()
  (let* ((entry (make-instance 'gtk:search-entry :placeholder-text "Search the project" :hexpand t :width-chars 8))
         (replace-entry (make-instance 'gtk:entry :placeholder-text "Replace" :hexpand t :width-chars 6))
         (case-button (make-instance 'gtk:toggle-button :label "Aa" :tooltip-text "Match case" :css-classes '("flat")))
         (word-button (make-instance 'gtk:toggle-button :label "W" :tooltip-text "Whole words only" :css-classes '("flat")))
         (regex-button (make-instance 'gtk:toggle-button :label ".*" :tooltip-text "Regular expression (\\1 in the replacement is the first group)"
                                                         :css-classes '("flat")))
         (replace-toggle (make-instance 'gtk:toggle-button :icon-name "cadre-replace-symbolic"
                                                           :tooltip-text "Replace" :css-classes '("flat")))
         (apply-button (make-instance 'gtk:button :label "Replace All" :css-classes '("suggested-action") :sensitive nil))
         (replace-revealer (gtk:build
                             (gtk:revealer :reveal-child nil :transition-type :slide-down
                               (gtk:box :spacing 4 :margin-top 4 replace-entry apply-button))))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label" "caption")))
         (results (make-instance 'gtk:box :orientation :vertical :spacing 2 :margin-end 4))
         (ps (make-project-search :entry entry :replace-entry replace-entry :replace-revealer replace-revealer
                                  :replace-toggle replace-toggle :case-button case-button :word-button word-button
                                  :regex-button regex-button
                                  :status status :results results :apply-button apply-button)))
    (setf *project-search* ps)
    (gobject:connect entry :activate (lambda (e) (declare (ignore e)) (setf (ps-mode ps) :text) (run-project-search)))
    (gobject:connect entry :search-changed
                     (lambda (e) (declare (ignore e))
                       (when (eq (ps-mode ps) :text) (schedule-project-search))))
    (dolist (b (list case-button word-button regex-button))
      (gobject:connect b :toggled (lambda (b) (declare (ignore b)) (run-project-search))))
    (gobject:connect replace-toggle :toggled
                     (lambda (b) (show-replace-row (gtk:toggle-button-get-active b))))
    (gobject:connect replace-entry :activate (lambda (e) (declare (ignore e)) (confirm-project-replace)))
    (gobject:connect apply-button :clicked (lambda (b) (declare (ignore b)) (confirm-project-replace)))
    (setf (ps-widget ps)
          (gtk:build
            (gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 6
              (gtk:label :label "SEARCH" :xalign 0.0 :margin-start 4 :margin-top 8 :margin-bottom 4
                         :css-classes '("caption-heading" "dim-label"))
              entry
              (gtk:box :spacing 0 case-button word-button regex-button
                (gtk:box :hexpand t)
                replace-toggle)
              replace-revealer
              status
              (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child results))))))

(defun show-replace-row (on)
  (let ((ps *project-search*))
    (gtk:revealer-set-reveal-child (ps-replace-revealer ps) on)
    (unless (eq (gtk:toggle-button-get-active (ps-replace-toggle ps)) on)
      (gtk:toggle-button-set-active (ps-replace-toggle ps) on))
    (when (eq (ps-mode ps) :text)
      (gtk:button-set-label (ps-apply-button ps) "Replace All"))
    ;; Check boxes show only while replacing.
    (loop for (nil rows) in (ps-results-data ps)
          do (loop for row in rows do (gtk:widget-set-visible (fourth row) on)))))

(defvar *project-search-timer* nil)

(defun schedule-project-search ()
  (when *project-search-timer* (glib:source-remove *project-search-timer*))
  (setf *project-search-timer*
        (glib:timeout-add glib:+priority-default+ 350
                          (lambda () (setf *project-search-timer* nil) (run-project-search) nil))))

(defun relative-name (path)
  (let ((project (window-project *window*)))
    (if project
        (or (ignore-errors (enough-namestring path project)) (namestring path))
        (namestring path))))

(defun lisp-file-p (path) (eq (major-mode-for-file path) 'lisp-mode))

(defun run-project-search ()
  "Search the project for the entry's text (or, in symbol mode, the symbol), on a thread."
  (let* ((ps *project-search*)
         (pattern (gtk:editable-get-text (ps-entry ps)))
         (project (window-project *window*))
         (mode (ps-mode ps))
         (case-sensitive (gtk:toggle-button-get-active (ps-case-button ps)))
         (whole-word (gtk:toggle-button-get-active (ps-word-button ps)))
         (regex (gtk:toggle-button-get-active (ps-regex-button ps)))
         (bad-regex (and regex (eq mode :text)
                         (handler-case (progn (regex-scanner pattern) nil)
                           (editor-error (e) (editor-error-message e)))))
         (generation (incf (ps-generation ps)))
         ;; Open buffers' text, read here on the GUI thread.
         (open-texts (loop for buffer in (buffer-list)
                           when (buffer-file buffer)
                             collect (cons (namestring (or (probe-file (buffer-file buffer)) (buffer-file buffer)))
                                           (buffer-string buffer)))))
    (cond
      ((null project) (gtk:label-set-text (ps-status ps) "Open a folder to search it."))
      ((string= pattern "") (show-project-results ps '() 0) (gtk:label-set-text (ps-status ps) ""))
      (bad-regex (show-project-results ps '() 0) (gtk:label-set-text (ps-status ps) bad-regex))
      (t
       (gtk:label-set-text (ps-status ps) "Searching…")
       (sb-thread:make-thread
        (lambda ()
          (let ((results '()) (count 0))
            (dolist (path (project-files project :hidden-names *explorer-hidden-names*
                                                 :hidden-types *explorer-hidden-types*))
              (when (< count *max-project-matches*)
                (let* ((text (or (cdr (assoc (namestring path) open-texts :test #'string=))
                                 (read-text-file path)))
                       (matches (and text
                                     (if (eq mode :symbol)
                                         (and (lisp-file-p path) (symbol-occurrences text pattern))
                                         (text-matches text pattern :case-sensitive case-sensitive
                                                                    :whole-word whole-word :regex regex)))))
                  (when matches
                    (incf count (length matches))
                    (push (list path (coerce (split-text-lines text) 'vector) matches) results)))))
            (deliver-to-gui (lambda ()
                              (when (= generation (ps-generation ps))
                                (show-project-results ps (nreverse results) count))))))
        :name "project search")))))

(defun match-markup (line start end)
  (let* ((from (max 0 (- start 40)))
         (to (min (length line) (+ end 80))))
    (format nil "~a<b>~a</b>~a"
            (glib:markup-escape-text (string-left-trim " 	" (subseq line from start)) -1)
            (glib:markup-escape-text (subseq line start end) -1)
            (glib:markup-escape-text (subseq line end to) -1))))

(defun show-project-results (ps results count)
  (clear-box (ps-results ps))
  (let ((replacing (gtk:revealer-get-reveal-child (ps-replace-revealer ps))))
    (setf (ps-results-data ps)
          (loop for (path lines matches) in results
                collect (let ((rows '())
                              (box (make-instance 'gtk:box :orientation :vertical)))
                          (dolist (m matches)
                            (destructuring-bind (line start end) m
                              (let* ((check (make-instance 'gtk:check-button :active t :visible replacing))
                                     (label (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t
                                                                      :use-markup t
                                                                      :label (match-markup (aref lines line) start end)))
                                     (number (make-instance 'gtk:label :label (princ-to-string (1+ line))
                                                                       :width-chars 4 :xalign 1.0
                                                                       :css-classes '("dim-label" "caption")))
                                     (button (make-instance 'gtk:button :css-classes '("flat" "cadre-search-match")
                                                                        :child (gtk:build (gtk:box :spacing 6 number label)))))
                                (let ((path path) (line line) (start start) (end end))
                                  (gobject:connect button :clicked
                                                   (lambda (b) (declare (ignore b)) (show-match path line start end))))
                                (gtk:box-append box (gtk:build (gtk:box check button)))
                                (push (list line start end check) rows))))
                          (gtk:box-append (ps-results ps)
                                          (make-instance 'gtk:expander :expanded t :child box
                                                                       :label (format nil "~a  (~d)" (relative-name path)
                                                                                      (length matches))))
                          (list path (nreverse rows)))))
    (gtk:label-set-text (ps-status ps)
                        (cond ((zerop count) "No results.")
                              (t (format nil "~d result~:p in ~d file~:p~:[~;, stopped at ~d~]"
                                         count (length results) (>= count *max-project-matches*)
                                         *max-project-matches*))))
    (gtk:widget-set-sensitive (ps-apply-button ps) (plusp count))))

(defun show-match (path line start end)
  (open-file-path path :then (lambda (view)
                               (let ((gtk-buffer (view-gtk-buffer view)))
                                 (gtk:text-buffer-select-range gtk-buffer (line-iter gtk-buffer line start)
                                                               (line-iter gtk-buffer line end))
                                 (scroll-to-cursor view)))))

;;; Replacing

(defun checked-matches (rows)
  (loop for (line start end check) in rows
        when (gtk:check-button-get-active check) collect (list line start end)))

(defun replace-buffer-text (buffer text)
  "Make BUFFER's text TEXT, changing only the part that differs, as one undo step."
  (let* ((gtk-buffer (buffer-text buffer))
         (old (text-string gtk-buffer))
         (prefix (or (mismatch old text) (length old)))
         (suffix (let ((m (mismatch old text :from-end t))) (if m (- (length old) m) 0)))
         (suffix (min suffix (- (length old) prefix) (- (length text) prefix))))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer prefix) (iter-at gtk-buffer (- (length old) suffix)))
      (gtk:text-buffer-insert gtk-buffer (iter-at gtk-buffer prefix) (subseq text prefix (- (length text) suffix)) -1))))

(defun write-text-file (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun apply-project-replace ()
  "Replace the checked matches. Returns the number of matches and files changed."
  (let* ((ps *project-search*)
         (replacement (gtk:editable-get-text (ps-replace-entry ps)))
         (pattern (gtk:editable-get-text (ps-entry ps)))
         (case-sensitive (or (eq (ps-mode ps) :symbol) (gtk:toggle-button-get-active (ps-case-button ps))))
         (regex (and (eq (ps-mode ps) :text) (gtk:toggle-button-get-active (ps-regex-button ps))))
         (matches 0) (files 0))
    (loop for (path rows) in (ps-results-data ps)
          for checked = (checked-matches rows)
          when checked
            do (let* ((buffer (find-file-buffer path))
                      (text (if buffer (buffer-string buffer) (read-text-file path))))
                 (when text
                   ;; Keep each match's case when matching ignored it.
                   (let* ((lines (coerce (split-text-lines text) 'vector))
                          (new (with-output-to-string (out)
                                 (let ((pieces (sort (copy-list checked)
                                                     (lambda (a b) (or (< (first a) (first b))
                                                                       (and (= (first a) (first b)) (< (second a) (second b))))))))
                                   (loop for line-text across lines
                                         for n from 0
                                         do (let ((pos 0))
                                              (loop for (l s e) in pieces
                                                    when (= l n)
                                                      do (write-string line-text out :start pos :end s)
                                                         (write-string (cond (regex (regex-replacement pattern (subseq line-text s e) replacement
                                                                                                       :case-sensitive case-sensitive))
                                                                             (case-sensitive replacement)
                                                                             (t (replacement-for (subseq line-text s e) replacement)))
                                                                       out)
                                                         (setf pos e))
                                              (write-string line-text out :start pos))
                                            (when (< n (1- (length lines))) (terpri out)))))))
                     (if buffer (replace-buffer-text buffer new) (write-text-file path new))
                     (incf matches (length checked))
                     (incf files)))))
    (values matches files)))

(defun confirm-project-replace ()
  (let* ((ps *project-search*)
         (count (loop for (nil rows) in (ps-results-data ps) sum (length (checked-matches rows))))
         (renaming (eq (ps-mode ps) :symbol))
         (replacement (gtk:editable-get-text (ps-replace-entry ps))))
    (cond
      ((zerop count) (message "Nothing to replace"))
      ((and renaming (string= replacement "")) (message "Type the new name first"))
      (t
       (let ((dialog (adw:alert-dialog-new
                      (if renaming
                          (format nil "Rename ~a to ~a?" (ps-symbol ps) replacement)
                          (format nil "Replace ~d match~:*~[es~;~:;es~] with “~a”?" count replacement))
                      (format nil "~d place~:p in ~d file~:p. Open files change in their buffers (undo works there); other files are written to disk."
                              count (count-if (lambda (r) (checked-matches (second r))) (ps-results-data ps))))))
         (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
         (adw:alert-dialog-add-response dialog "replace" (if renaming "_Rename" "_Replace"))
         (adw:alert-dialog-set-response-appearance dialog "replace" :suggested)
         (adw:alert-dialog-set-close-response dialog "cancel")
         (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                    (lambda (response)
                      (when (string= response "replace")
                        (multiple-value-bind (matches files) (apply-project-replace)
                          (message "~:[Replaced~;Renamed~] ~d occurrence~:p in ~d file~:p" renaming matches files)
                          (when renaming
                            (setf (ps-symbol ps) replacement)
                            (gtk:editable-set-text (ps-entry ps) replacement))
                          (run-project-search))))))))))

;;; Commands

(defun selection-or-symbol-text (&key (symbol t))
  "The selection (if short and on one line), else the symbol at the cursor."
  (let ((view (and *window* (selected-view *window*))))
    (when view
      (let ((gtk-buffer (view-gtk-buffer view)))
        (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
          (or (and has (let ((text (gtk:text-buffer-get-text gtk-buffer start end nil)))
                         (and (< (length text) 200) (not (find #\Newline text)) text)))
              (and symbol (symbol-at-cursor view))))))))

(defun symbol-at-cursor (view)
  (let ((syntax (buffer-syntax (view-buffer view))))
    (if syntax
        (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c))
        ;; Outside Lisp, the word at the cursor.
        (let* ((gtk-buffer (view-gtk-buffer view))
               (point (point-offset view))
               (text (text-string gtk-buffer))
               (start (loop for i downfrom point while (and (> i 0) (symbol-constituent-p (char text (1- i)))) finally (return i)))
               (end (loop for i from point while (and (< i (length text)) (symbol-constituent-p (char text i))) finally (return i))))
          (and (< start end) (subseq text start end))))))

(defun open-project-search (&key text (mode :text) replace focus-replace)
  (show-sidebar-page *window* "search" :toggle nil)
  (let ((ps *project-search*))
    (setf (ps-mode ps) mode)
    (gtk:button-set-label (ps-apply-button ps) (if (eq mode :symbol) "Rename" "Replace All"))
    (when text
      (gtk:editable-set-text (ps-entry ps) text))
    (show-replace-row replace)
    (run-project-search)
    (if focus-replace
        (progn (gtk:widget-grab-focus (ps-replace-entry ps))
               (gtk:editable-select-region (ps-replace-entry ps) 0 -1))
        (progn (gtk:widget-grab-focus (ps-entry ps))
               (gtk:editable-select-region (ps-entry ps) 0 -1)))))

(define-command find-in-project ()
  "Search every file in the project (the selection, or the symbol at the cursor)."
  (open-project-search :text (selection-or-symbol-text)))

(define-command replace-in-project ()
  "Find and replace in every file in the project."
  (open-project-search :text (selection-or-symbol-text) :replace t))

(define-command find-symbol-in-project ()
  "Find every use of the symbol at the cursor in the project's Lisp files (not in strings or comments)."
  (let ((name (or (selection-or-symbol-text) (editor-error "No symbol at the cursor"))))
    (setf (ps-symbol *project-search*) name)
    (open-project-search :text name :mode :symbol)))

(define-command rename-symbol ()
  "Rename the symbol at the cursor in every Lisp file of the project: review
the places in the Search page, then Rename."
  (let ((name (or (let ((view (selected-view *window*))) (and view (symbol-at-cursor view)))
                  (editor-error "No symbol at the cursor"))))
    (setf (ps-symbol *project-search*) (subseq name (symbol-name-part name)))
    (gtk:editable-set-text (ps-replace-entry *project-search*) (ps-symbol *project-search*))
    (open-project-search :text (ps-symbol *project-search*) :mode :symbol :replace t :focus-replace t)))
