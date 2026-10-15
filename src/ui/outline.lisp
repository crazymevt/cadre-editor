;;;; outline.lisp — the definitions in the current file, or the whole project
;;;;
;;;; The sidebar's Outline page lists what the file in the selected tab
;;;; defines (for Markdown, its headings), with the one around the cursor
;;;; selected; clicking one goes there. Go to Symbol picks one by name. The
;;;; list is read again a moment after the file changes.
;;;;
;;;; Project shows every Lisp, Racket and Clojure definition in the project
;;;; instead, grouped by package or namespace (Racket: by file), or by file:
;;;; the groups start closed but for the current file's, and a filter finds
;;;; definitions by name. Sort by Name lists definitions alphabetically
;;;; (in both views) instead of in the order the files have them. It's read on a thread (files that haven't changed
;;;; aren't read again) when shown and after a file is saved. Go to Symbol in
;;;; Project picks from all of it.

(in-package #:cadre-ui)

(defstruct (outline (:conc-name ol-))
  widget list title buffer items timer (updating nil)
  (mode :file)                          ; :file or :project
  (group-by :namespace)                 ; :namespace or :file, for the project
  stack project-list filter group-dropdown status
  (rows '())                            ; the project list's rows: (:group key) or (:definition entry)
  (expanded (make-hash-table :test 'equal)))

(defvar *outline* nil "The Outline page.")

(defun outline-items (buffer)
  "What BUFFER defines, as (name kind line depth): definitions for Lisp
(Common Lisp, Racket or Clojure),
headings for Markdown, nil for other files."
  (case (buffer-major-mode buffer)
    (lisp-mode
     (mapcar (lambda (d)
               (list (if (eq (definition-kind d) :method)
                         (format nil "~a ~a" (definition-name d) (or (definition-arglist d) ""))
                         (definition-name d))
                     (definition-kind d) (definition-line d) 0))
             (buffer-definitions buffer)))
    ((racket-mode clojure-mode)
     (let ((syntax (buffer-sexp-syntax buffer)))
       (and syntax
            (mapcar (lambda (d) (list (definition-name d) (definition-kind d) (definition-line d) 0))
                    (dialect-definitions syntax)))))
    (markdown-mode
     (mapcar (lambda (h) (list (second h) :heading (third h) (1- (first h))))
             (markdown-headings (buffer-string buffer))))
    (t (let ((document (and (buffer-ts-document buffer) (fresh-ts-document buffer))))
         (and document (ts-outline document))))))

(defun outline-kind-label (kind)
  (case kind (:heading "") (t (kind-name kind))))

(defun make-outline-row (item)
  (destructuring-bind (name kind line depth) item
    (declare (ignore line))
    (let ((row (make-instance 'gtk:list-box-row)))
      (gtk:list-box-row-set-child
       row (gtk:build
             (gtk:box :spacing 8 :margin-start (+ 10 (* 14 depth)) :margin-end 8 :margin-top 2 :margin-bottom 2
               (gtk:label :label name :xalign 0.0 :hexpand t :ellipsize :end
                          :css-classes (if (eq kind :heading) '() '("monospace")))
               (gtk:label :label (outline-kind-label kind) :css-classes '("dim-label" "caption")))))
      row)))

(defun make-outline-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :single :css-classes '("navigation-sidebar")))
         (title (make-instance 'gtk:label :xalign 0.0 :margin-start 12 :margin-bottom 4 :ellipsize :middle
                                          :css-classes '("dim-label" "caption")))
         (this-file (make-instance 'gtk:toggle-button :label "This File" :active t))
         (project (make-instance 'gtk:toggle-button :label "Project" :group this-file))
         (sort-by-name (make-instance 'gtk:check-button :label "Sort by name" :active (outline-sort-by-name-p)
                                                        :tooltip-text "List definitions alphabetically, not in the files' order"))
         (group-by (gtk:drop-down-new-from-strings '("Namespace" "File")))
         (filter (make-instance 'gtk:search-entry :placeholder-text "Filter" :hexpand t :width-chars 6))
         (status (make-instance 'gtk:label :xalign 0.0 :margin-start 12 :margin-bottom 4 :wrap t
                                           :css-classes '("dim-label" "caption")))
         (project-list (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (stack (make-instance 'gtk:stack :vexpand t))
         (widget nil))
    (gtk:stack-add-named stack (gtk:build (gtk:box :orientation :vertical
                                            title
                                            (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child list)))
                         "file")
    (gtk:stack-add-named stack (gtk:build (gtk:box :orientation :vertical
                                            (gtk:box :margin-start 8 :margin-end 8 :margin-bottom 4 :spacing 4
                                              filter group-by)
                                            status
                                            (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child project-list)))
                         "project")
    (setf widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :margin-start 12 :margin-end 6 :margin-top 4 :spacing 6
                       (gtk:label :label "OUTLINE" :xalign 0.0 :hexpand t :css-classes '("caption-heading" "dim-label")))
                     (gtk:box :margin-start 8 :margin-end 8 :margin-bottom 2
                       (gtk:box :css-classes '("linked") :homogeneous t :hexpand t this-file project))
                     (gtk:box :margin-start 8 :margin-end 8 :margin-bottom 4
                       sort-by-name)
                     stack)))
    (setf *outline* (make-outline :widget widget :list list :title title :stack stack :project-list project-list
                                  :filter filter :group-dropdown group-by :status status))
    (gobject:connect project :toggled
                     (lambda (b)
                       (let ((on (gtk:toggle-button-get-active b)))
                         (setf (ol-mode *outline*) (if on :project :file))
                         (gtk:stack-set-visible-child-name stack (if on "project" "file"))
                         (if on (show-project-outline :rescan t) (refresh-outline :force t)))))
    (gobject:connect group-by "notify::selected"
                     (lambda (d p) (declare (ignore p))
                       (setf (ol-group-by *outline*) (if (= 1 (gtk:drop-down-get-selected d)) :file :namespace))
                       (clrhash (ol-expanded *outline*))
                       (fill-project-outline)))
    (gobject:connect filter :search-changed (lambda (e) (declare (ignore e)) (fill-project-outline)))
    (gobject:connect sort-by-name :toggled
                     (lambda (b)
                       (setf (setting :outline-sort-by-name) (gtk:check-button-get-active b))
                       (if (eq (ol-mode *outline*) :project) (fill-project-outline) (refresh-outline :force t))))
    (gobject:connect project-list :row-activated
                     (lambda (lb row) (declare (ignore lb)) (project-outline-row-activated row)))
    (gtk:list-box-set-activate-on-single-click project-list t)
    (gobject:connect list :row-activated
                     (lambda (lb row)
                       (declare (ignore lb))
                       (let ((item (nth (gtk:list-box-row-get-index row) (ol-items *outline*)))
                             (view (selected-view *window*)))
                         (when (and item view (eq (view-buffer view) (ol-buffer *outline*)))
                           (goto-line-column view (third item) 0)
                           (focus-view view)))))
    (gtk:list-box-set-activate-on-single-click list t)
    widget))

(defun outline-visible-p ()
  (and *outline* *window* (gtk:widget-get-visible (window-sidebar *window*))
       (equal "outline" (sidebar-page *window*))))

(defun outline-sort-by-name-p () (and (setting :outline-sort-by-name) t))

(defun sort-outline-items (items)
  "ITEMS by name when Sort by Name is on, unless they nest (Markdown's
headings, which only make sense in order)."
  (if (and (outline-sort-by-name-p) (every (lambda (item) (zerop (fourth item))) items)
           (notany (lambda (item) (eq (second item) :heading)) items))
      (stable-sort (copy-list items) #'string-lessp :key #'first)
      items))

(defun fill-outline (buffer)
  (let ((ol *outline*)
        (items (and buffer (sort-outline-items (outline-items buffer)))))
    (setf (ol-buffer ol) buffer (ol-items ol) items)
    (gtk:label-set-text (ol-title ol)
                        (cond ((null buffer) "No file")
                              ((or (member (buffer-major-mode buffer) '(lisp-mode racket-mode clojure-mode markdown-mode))
                                   (buffer-ts-document buffer))
                               (if items (buffer-display-name buffer) (format nil "~a defines nothing" (buffer-display-name buffer))))
                              (t (format nil "No outline for ~a" (buffer-display-name buffer)))))
    (gtk:list-box-remove-all (ol-list ol))
    (dolist (item items) (gtk:list-box-append (ol-list ol) (make-outline-row item)))))

(defun select-outline-row (view)
  "Select the item the cursor of VIEW is in (the last one starting above it)."
  (let* ((ol *outline*)
         (line (cursor-line-column view))
         ;; The item starting last above the cursor, wherever sorting put it.
         (index (loop with best = nil and best-line = -1
                      for item in (ol-items ol)
                      for i from 0
                      when (and (<= (third item) line) (> (third item) best-line))
                        do (setf best i best-line (third item))
                      finally (return best)))
         (row (and index (gtk:list-box-get-row-at-index (ol-list ol) index))))
    (if row
        (gtk:list-box-select-row (ol-list ol) row)
        (gtk:list-box-unselect-all (ol-list ol)))))

(defun refresh-outline (&key force)
  "Show the selected tab's outline, if the Outline page is showing."
  (when (and (outline-visible-p) (eq (ol-mode *outline*) :project))
    (return-from refresh-outline (when force (show-project-outline))))
  (when (outline-visible-p)
    (let* ((view (selected-view *window*))
           (buffer (and view (view-buffer view))))
      (when (or force (not (eq buffer (ol-buffer *outline*))))
        (fill-outline buffer)
        (when buffer (outline-watch buffer)))
      (when view (select-outline-row view)))))

(defun outline-watch (buffer)
  "Read BUFFER's outline again a moment after it changes."
  (unless (buffer-local buffer :outline-handler)
    (setf (buffer-local buffer :outline-handler)
          (gobject:connect (buffer-text buffer) :changed
                           (lambda (b) (declare (ignore b))
                             (when (and *outline* (eq buffer (ol-buffer *outline*)))
                               (let ((timer (ol-timer *outline*)))
                                 (when timer (glib:source-remove timer))
                                 (setf (ol-timer *outline*)
                                       (glib:timeout-add glib:+priority-default+ 400
                                                         (lambda ()
                                                           (setf (ol-timer *outline*) nil)
                                                           (refresh-outline :force t)
                                                           nil))))))))))

;;; The project's outline

(defvar *project-outline* '() "Every definition in the project: (group pathname definition) each.")
(defvar *project-outline-root* nil)
(defvar *project-outline-scanning* nil)
(defvar *outline-file-cache* (make-hash-table :test 'equal)
  "Namestring → (write date . its FILE-OUTLINE), so unchanged files aren't read again.")
(defparameter *project-outline-limit* 1500 "The most definitions the project list shows at once.")

(defun scan-project-outline (root)
  "Every Lisp, Racket and Clojure definition under ROOT (on a thread, or not)."
  (let ((entries '()))
    (dolist (path (project-files root :hidden-names *explorer-hidden-names* :hidden-types *explorer-hidden-types*))
      (let ((mode (major-mode-for-file path)))
        (when (member mode '(lisp-mode clojure-mode racket-mode))
          (let* ((name (namestring path))
                 (date (ignore-errors (file-write-date path)))
                 (cached (gethash name *outline-file-cache*))
                 (outline (if (and cached (eql (car cached) date))
                              (cdr cached)
                              (let ((o (ignore-errors (file-outline (or (read-text-file path) "") mode))))
                                (setf (gethash name *outline-file-cache*) (cons date o))
                                o))))
            (loop for (group . definition) in outline
                  do (push (list group path definition) entries))))))
    (nreverse entries)))

(defun show-project-outline (&key rescan)
  "Show the project's definitions, reading them again (on a thread) with RESCAN or a new project."
  (let ((root (and *window* (window-project *window*))))
    (cond ((null root)
           (setf *project-outline* '() *project-outline-root* nil)
           (fill-project-outline))
          ((or rescan (not (equal root *project-outline-root*)))
           (fill-project-outline)
           (unless *project-outline-scanning*
             (setf *project-outline-scanning* t)
             (when (null *project-outline*) (gtk:label-set-text (ol-status *outline*) "Reading the project…"))
             (sb-thread:make-thread
              (lambda ()
                (let ((entries (ignore-errors (scan-project-outline root))))
                  (glib:call-in-main-thread
                   (lambda ()
                     (setf *project-outline-scanning* nil
                           *project-outline* entries
                           *project-outline-root* root)
                     (fill-project-outline)))))
              :name "cadre outline")))
          (t (fill-project-outline)))))

(defun outline-group-key (entry root group-by)
  (destructuring-bind (group path definition) entry
    (declare (ignore definition))
    (if (and (eq group-by :namespace) group)
        group
        (file-in-project-name path root))))

(defun project-outline-groups (group-by)
  "The project's definitions as (key . entries), the groups sorted, each group's in file and line order."
  (let ((table (make-hash-table :test 'equal))
        (root *project-outline-root*))
    (dolist (entry *project-outline*)
      (push entry (gethash (outline-group-key entry root group-by) table)))
    (sort (loop for key being the hash-keys of table using (hash-value entries)
                collect (cons key (if (outline-sort-by-name-p)
                                      (sort (copy-list entries) #'string-lessp
                                            :key (lambda (e) (definition-name (third e))))
                                      (sort (copy-list entries)
                                            (lambda (a b) (or (string< (namestring (second a)) (namestring (second b)))
                                                              (and (equal (second a) (second b))
                                                                   (< (definition-line (third a)) (definition-line (third b))))))))))
          #'string-lessp :key #'car)))

(defun current-file-groups (group-by)
  "The groups the selected tab's file has definitions in: these start open."
  (let* ((view (and *window* (selected-view *window*)))
         (file (and view (buffer-file (view-buffer view))))
         (name (and file (namestring (or (probe-file file) file)))))
    (when name
      (remove-duplicates
       (loop for entry in *project-outline*
             when (string= (namestring (second entry)) name)
               collect (outline-group-key entry *project-outline-root* group-by))
       :test #'string=))))

(defun project-group-row (key count expanded)
  (let ((row (make-instance 'gtk:list-box-row)))
    (gtk:list-box-row-set-child
     row (gtk:build
           (gtk:box :spacing 6 :margin-start 6 :margin-end 8 :margin-top 3 :margin-bottom 3
             (gtk:image :icon-name (if expanded "pan-down-symbolic" "pan-end-symbolic"))
             (gtk:label :label key :xalign 0.0 :hexpand t :ellipsize :middle :css-classes '("heading"))
             (gtk:label :label (princ-to-string count) :css-classes '("dim-label" "caption")))))
    row))

(defun project-definition-row (entry show-file)
  (destructuring-bind (group path definition) entry
    (declare (ignore group))
    (let ((row (make-instance 'gtk:list-box-row)))
      (gtk:list-box-row-set-child
       row (gtk:build
             (gtk:box :spacing 8 :margin-start 28 :margin-end 8 :margin-top 1 :margin-bottom 1
               (gtk:label :label (definition-name definition) :xalign 0.0 :hexpand t :ellipsize :end
                          :css-classes '("monospace"))
               (gtk:label :label (if show-file
                                     (format nil "~a · ~a" (kind-name (definition-kind definition)) (file-namestring path))
                                     (kind-name (definition-kind definition)))
                          :css-classes '("dim-label" "caption")))))
      (gtk:widget-set-tooltip-text row (format nil "~a:~d" (file-in-project-name path *project-outline-root*)
                                               (1+ (definition-line definition))))
      row)))

(defun fill-project-outline ()
  "Show the project's definitions: the groups, and the open ones' definitions (with a filter, its matches)."
  (let* ((ol *outline*)
         (list (ol-project-list ol))
         (filter (string-trim " " (gtk:editable-get-text (ol-filter ol))))
         (group-by (ol-group-by ol))
         (groups (project-outline-groups group-by))
         (rows '())
         (shown 0))
    (when (zerop (hash-table-count (ol-expanded ol)))
      (dolist (key (current-file-groups group-by)) (setf (gethash key (ol-expanded ol)) t)))
    (gtk:list-box-remove-all list)
    (flet ((add (row data) (gtk:list-box-append list row) (push data rows)))
      (dolist (group groups)
        (let* ((key (car group))
               (entries (if (string= filter "")
                            (cdr group)
                            (remove-if-not (lambda (e) (search filter (definition-name (third e)) :test #'char-equal))
                                           (cdr group))))
               (open (or (string/= filter "") (gethash key (ol-expanded ol)))))
          (when entries
            (add (project-group-row key (length entries) open) (list :group key))
            (when open
              (dolist (entry entries)
                (when (< shown *project-outline-limit*)
                  (incf shown)
                  (add (project-definition-row entry (eq group-by :namespace)) (list :definition entry)))))))))
    (setf (ol-rows ol) (nreverse rows))
    (gtk:label-set-text (ol-status ol)
                        (cond ((null *project-outline-root*) (if (and *window* (window-project *window*)) "Reading the project…" "Open a folder to see its definitions"))
                              ((null *project-outline*) "No Lisp, Racket or Clojure definitions in the project")
                              ((>= shown *project-outline-limit*) (format nil "The first ~:d shown: filter to find others" shown))
                              (t (format nil "~:d definitions in ~:d ~:[files~;packages and namespaces~]"
                                         (length *project-outline*) (length groups) (eq group-by :namespace)))))))

(defun project-outline-row-activated (row)
  (let ((data (nth (gtk:list-box-row-get-index row) (ol-rows *outline*))))
    (case (first data)
      (:group (let ((key (second data)))
                (setf (gethash key (ol-expanded *outline*)) (not (gethash key (ol-expanded *outline*))))
                (fill-project-outline)))
      (:definition (go-to-project-definition (second data))))))

(defun go-to-project-definition (entry)
  (destructuring-bind (group path definition) entry
    (declare (ignore group))
    (open-file-path path :then (lambda (view)
                                 (goto-line-column view (definition-line definition) 0)
                                 (focus-view view)))))

(add-hook '*after-save-hook*
          (lambda (buffer)
            (declare (ignore buffer))
            (when (and *outline* (eq (ol-mode *outline*) :project) (outline-visible-p))
              (show-project-outline :rescan t))))

(define-command go-to-symbol-in-project ()
  "Go to a definition anywhere in the project (its Lisp, Racket and Clojure files), chosen by name."
  (let ((root (or (and *window* (window-project *window*)) (editor-error "Open a folder first"))))
    (unless (and *project-outline* (equal root *project-outline-root*))
      (setf *project-outline* (scan-project-outline root) *project-outline-root* root))
    (unless *project-outline* (editor-error "No definitions in the project"))
    (open-picker (window-picker *window*)
                 :items *project-outline*
                 :label (lambda (entry) (definition-name (third entry)))
                 :detail (lambda (entry)
                           (format nil "~a~@[ · ~a~] · ~a:~d" (kind-name (definition-kind (third entry))) (first entry)
                                   (file-in-project-name (second entry) root) (1+ (definition-line (third entry)))))
                 :placeholder "Go to a definition in the project"
                 :on-choose #'go-to-project-definition)))

(define-command show-outline ()
  "Show the Outline page: the definitions in this file."
  (show-sidebar-page *window* "outline" :toggle nil)
  (refresh-outline :force t))

(define-command go-to-symbol ()
  "Go to a definition in this file (for Markdown, a heading), chosen by name."
  (let* ((view (current-view))
         (items (outline-items (view-buffer view))))
    (unless items (editor-error "Nothing to go to in ~a" (buffer-display-name (view-buffer view))))
    (open-picker (window-picker *window*)
                 :items items
                 :label (lambda (item) (format nil "~v@{  ~}~a" (fourth item) nil (first item)))
                 :detail (lambda (item) (format nil "~@[~a · ~]line ~d"
                                                (let ((k (outline-kind-label (second item)))) (and (string/= k "") k))
                                                (1+ (third item))))
                 :placeholder "Go to a definition"
                 :on-choose (lambda (item)
                              (goto-line-column view (third item) 0)
                              (focus-view view)))))
