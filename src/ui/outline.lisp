;;;; outline.lisp — the definitions in the current file
;;;;
;;;; The sidebar's Outline page lists what the file in the selected tab
;;;; defines (for Markdown, its headings), with the one around the cursor
;;;; selected; clicking one goes there. Go to Symbol picks one by name. The
;;;; list is read again a moment after the file changes.

(in-package #:cadre-ui)

(defstruct (outline (:conc-name ol-))
  widget list title buffer items timer (updating nil))

(defvar *outline* nil "The Outline page.")

(defun outline-items (buffer)
  "What BUFFER defines, as (name kind line depth): definitions for Lisp,
headings for Markdown, nil for other files."
  (case (buffer-major-mode buffer)
    (lisp-mode
     (mapcar (lambda (d)
               (list (if (eq (definition-kind d) :method)
                         (format nil "~a ~a" (definition-name d) (or (definition-arglist d) ""))
                         (definition-name d))
                     (definition-kind d) (definition-line d) 0))
             (buffer-definitions buffer)))
    (markdown-mode
     (mapcar (lambda (h) (list (second h) :heading (third h) (1- (first h))))
             (markdown-headings (buffer-string buffer))))))

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
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:label :label "OUTLINE" :xalign 0.0 :margin-start 12 :margin-top 8
                                :css-classes '("caption-heading" "dim-label"))
                     title
                     (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child list)))))
    (setf *outline* (make-outline :widget widget :list list :title title))
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

(defun fill-outline (buffer)
  (let ((ol *outline*)
        (items (and buffer (outline-items buffer))))
    (setf (ol-buffer ol) buffer (ol-items ol) items)
    (gtk:label-set-text (ol-title ol)
                        (cond ((null buffer) "No file")
                              ((member (buffer-major-mode buffer) '(lisp-mode markdown-mode))
                               (if items (buffer-display-name buffer) (format nil "~a defines nothing" (buffer-display-name buffer))))
                              (t (format nil "No outline for ~a" (buffer-display-name buffer)))))
    (gtk:list-box-remove-all (ol-list ol))
    (dolist (item items) (gtk:list-box-append (ol-list ol) (make-outline-row item)))))

(defun select-outline-row (view)
  "Select the item the cursor of VIEW is in (the last one starting above it)."
  (let* ((ol *outline*)
         (line (cursor-line-column view))
         (index (loop with best = nil
                      for item in (ol-items ol)
                      for i from 0
                      when (<= (third item) line) do (setf best i)
                      finally (return best)))
         (row (and index (gtk:list-box-get-row-at-index (ol-list ol) index))))
    (if row
        (gtk:list-box-select-row (ol-list ol) row)
        (gtk:list-box-unselect-all (ol-list ol)))))

(defun refresh-outline (&key force)
  "Show the selected tab's outline, if the Outline page is showing."
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
