;;;; profiler.lisp — the Profiler page: where a program spends its time
;;;;
;;;; Profile Form runs the form at the cursor (or the selection) with
;;;; sb-sprof sampling it; Profile All Threads samples the whole Lisp, a
;;;; running app included, until Stop. The page shows the samples as a table
;;;; of functions (self: running that function itself; total: anywhere on
;;;; the stack), with a function's callers and callees beside it, or as a
;;;; flame graph: each stack of calls from the top down, as wide as the
;;;; samples it was in. Clicking a box zooms into it; clicking one above
;;;; zooms back out.

(in-package #:cadre-ui)

(defstruct (profiler-page (:conc-name pp-))
  widget title list details flame stack mode-dropdown run-button filter
  (profile nil)
  (sort :total)
  (path '())                            ; the flame graph's zoom
  (boxes '())                           ; the flame graph's boxes, as drawn
  (selected nil))                       ; the function chosen, by name

(defvar *profiler-page* nil)
(defvar *profiler-rows* (make-hash-table :test 'eq) "Profiler row → profile-function.")
(defvar *profiler-helper-connection* nil "The connection the helper was sent to.")

(defparameter *profiler-rows-shown* 500 "The most functions the table shows.")
(defparameter *flame-row-height* 18)
(defparameter *profiler-modes* '((:cpu "CPU") (:alloc "Allocation") (:time "Wall Time")))

(defun profiler-mode ()
  (first (nth (gtk:drop-down-get-selected (pp-mode-dropdown *profiler-page*)) *profiler-modes*)))

(defun make-profiler-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :single :css-classes '("navigation-sidebar")))
         (details (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8
                                          :margin-top 6 :margin-bottom 6))
         (flame (make-instance 'gtk:drawing-area :hexpand t :has-tooltip t))
         (title (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t :css-classes '("heading")
                                          :label "Nothing profiled"))
         (modes (gtk:drop-down-new-from-strings (mapcar #'second *profiler-modes*)))
         (run (make-instance 'gtk:button :label "Profile All Threads" :css-classes '("flat")
                                         :tooltip-text "Sample every thread of the Lisp (a running app too) until Stop"))
         (filter (make-instance 'gtk:search-entry :placeholder-text "Filter functions" :hexpand t))
         (table-button (make-instance 'gtk:toggle-button :label "Table" :active t :css-classes '("flat")))
         (flame-button (make-instance 'gtk:toggle-button :label "Flame Graph" :css-classes '("flat")))
         (stack (make-instance 'gtk:stack :vexpand t))
         (sort-buttons
           (loop for (key text width) in '((:self "Self" 7) (:total "Total" 7) (:name "Function" 0))
                 collect (let ((key key))
                           (gtk:build
                             (gtk:button :css-classes '("flat" "cadre-profiler-sort") :hexpand (eq key :name)
                                         :tooltip-text (format nil "Sort by ~(~a~)" text)
                                         :on-clicked (lambda (b) (declare (ignore b))
                                                       (setf (pp-sort *profiler-page*) key)
                                                       (fill-profiler-list *profiler-page*))
                               (gtk:label :label text :xalign (if (eq key :name) 0.0 1.0)
                                          :width-chars width :css-classes '("caption-heading"))))))))
    (gtk:widget-set-tooltip-text modes "What a sample counts: CPU time, allocation, or wall-clock time")
    (gtk:toggle-button-set-group flame-button table-button)
    (let ((paned (make-instance 'gtk:paned :orientation :horizontal :position 520
                                           :shrink-start-child nil :shrink-end-child nil)))
      (gtk:paned-set-start-child paned (gtk:build
                                         (gtk:box :orientation :vertical
                                           (gtk:box :margin-start 6 :margin-end 6 :margin-top 2 :margin-bottom 2 :spacing 2
                                             (first sort-buttons) (second sort-buttons) (third sort-buttons))
                                           (gtk:scrolled-window :vexpand t :child list))))
      (gtk:paned-set-end-child paned (make-instance 'gtk:scrolled-window :hscrollbar-policy :never :child details))
      (gtk:stack-add-named stack paned "table"))
    (gtk:stack-add-named stack (make-instance 'gtk:scrolled-window :child flame :vexpand t) "flame")
    (let ((widget (gtk:build
                    (gtk:box :orientation :vertical
                      (gtk:box :spacing 4 :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4
                        title
                        (gtk:box :css-classes '("linked") table-button flame-button)
                        filter
                        modes
                        run
                        (gtk:button :icon-name "edit-clear-all-symbolic" :css-classes '("flat")
                                    :tooltip-text "Forget the samples"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'clear-profile))))
                      (gtk:separator)
                      stack))))
      (setf *profiler-page* (make-profiler-page :widget widget :title title :list list :details details
                                                :flame flame :stack stack :mode-dropdown modes
                                                :run-button run :filter filter))
      (gobject:connect run :clicked (lambda (b) (declare (ignore b))
                                      (call-command (if (and (pp-profile *profiler-page*)
                                                             (profile-running (pp-profile *profiler-page*)))
                                                        'stop-profiling
                                                        'profile-all-threads))))
      (gobject:connect table-button :toggled
                       (lambda (b) (when (gtk:toggle-button-get-active b) (gtk:stack-set-visible-child-name stack "table"))))
      (gobject:connect flame-button :toggled
                       (lambda (b) (when (gtk:toggle-button-get-active b) (gtk:stack-set-visible-child-name stack "flame"))))
      (gobject:connect filter :search-changed (lambda (e) (declare (ignore e)) (fill-profiler-list *profiler-page*)))
      (gobject:connect list :row-selected
                       (lambda (lb row)
                         (declare (ignore lb))
                         (let ((function (and row (gethash row *profiler-rows*))))
                           (when function
                             (setf (pp-selected *profiler-page*) (profile-function-name function))
                             (fill-profiler-details *profiler-page*)
                             (gtk:widget-queue-draw flame)))))
      (setup-flame-graph *profiler-page*)
      (fill-profiler-page *profiler-page*)
      widget)))

;;; The table

(defun profiler-row (function samples)
  (let* ((box (make-instance 'gtk:box :spacing 2 :margin-start 6))
         (row (make-instance 'gtk:list-box-row :child box)))
    (gtk:box-append box (label (profile-percent (profile-function-self function) samples)
                               :width-chars 7 :xalign 1.0 :css-classes '("monospace" "numeric")))
    (gtk:box-append box (label (profile-percent (profile-function-total function) samples)
                               :width-chars 7 :xalign 1.0 :css-classes '("monospace" "numeric" "dim-label")))
    (gtk:box-append box (label (profile-function-name function) :ellipsize :end :hexpand t :margin-start 10
                                                                :css-classes '("monospace")))
    (gtk:widget-set-tooltip-text row (profile-function-name function))
    (setf (gethash row *profiler-rows*) function)
    row))

(defun fill-profiler-list (page)
  (let ((list (pp-list page))
        (profile (pp-profile page)))
    (clrhash *profiler-rows*)
    (gtk:list-box-remove-all list)
    (cond ((or (null profile) (zerop (profile-samples profile)))
           (gtk:list-box-append list (label (if (and profile (profile-running profile))
                                                "Profiling… Stop to see where the time went"
                                                "Profile Form (in a Lisp file's right-click menu) runs the form at the cursor and shows where its time went; Profile All Threads samples the whole Lisp until Stop")
                                            :margin-start 8 :wrap t :css-classes '("dim-label"))))
          (t
           (let* ((functions (sort-profile-functions (profile-functions profile) (pp-sort page)
                                                     :filter (gtk:editable-get-text (pp-filter page))))
                  (selected-row nil))
             (loop for function in functions
                   repeat *profiler-rows-shown*
                   do (let ((row (profiler-row function (profile-samples profile))))
                        (gtk:list-box-append list row)
                        (when (equal (profile-function-name function) (pp-selected page))
                          (setf selected-row row))))
             (when (> (length functions) *profiler-rows-shown*)
               (gtk:list-box-append list (label (format nil "Showing ~d functions of ~d: filter to find others"
                                                        *profiler-rows-shown* (length functions))
                                                :margin-start 8 :css-classes '("dim-label"))))
             (when selected-row (gtk:list-box-select-row list selected-row)))))))

(defun profiler-link (text count samples on-click)
  (let ((button (gtk:build
                  (gtk:button :css-classes '("flat" "cadre-link")
                    (gtk:box :spacing 8
                      (label (profile-percent count samples) :width-chars 6 :xalign 1.0 :css-classes '("numeric" "dim-label"))
                      (label text :ellipsize :end :css-classes '("monospace")))))))
    (gtk:widget-set-tooltip-text button text)
    (gobject:connect button :clicked (lambda (b) (declare (ignore b)) (funcall on-click)))
    button))

(defun select-profile-function (page name)
  "Show the function NAME in the table, with its callers and callees."
  (setf (pp-selected page) name)
  (unless (search (gtk:editable-get-text (pp-filter page)) name :test #'char-equal)
    (gtk:editable-set-text (pp-filter page) ""))
  (fill-profiler-list page)
  (fill-profiler-details page)
  (gtk:widget-queue-draw (pp-flame page)))

(defun fill-profiler-details (page)
  (let* ((box (pp-details page))
         (profile (pp-profile page))
         (function (and profile (pp-selected page) (find-profile-function profile (pp-selected page)))))
    (clear-box box)
    (if (null function)
        (gtk:box-append box (label "Choose a function to see what calls it and what it calls" :wrap t
                                                                                               :css-classes '("dim-label")))
        (let ((samples (profile-samples profile)))
          (gtk:box-append box (label (profile-function-name function) :wrap t :wrap-mode :char :selectable t
                                                                      :css-classes '("monospace" "heading")))
          (gtk:box-append box (label (format nil "Self ~a · total ~a · ~d sample~:p"
                                             (profile-percent (profile-function-self function) samples)
                                             (profile-percent (profile-function-total function) samples)
                                             (profile-function-total function))
                                     :css-classes '("dim-label")))
          (let ((definition (profile-function-definition function)))
            (gtk:box-append box (gtk:build
                                  (gtk:button :label (if definition "Go to Definition" "No source")
                                              :sensitive (and definition t) :halign :start
                                              :tooltip-text (if definition (format nil "Show ~a's source" definition) "")
                                              :on-clicked (lambda (b) (declare (ignore b))
                                                            (go-to-profiled-definition definition))))))
          (flet ((section (heading pairs)
                   (gtk:box-append box (label heading :margin-top 8 :css-classes '("heading")))
                   (if (null pairs)
                       (gtk:box-append box (label "Nothing" :css-classes '("dim-label")))
                       (loop for (name . count) in pairs
                             repeat 50
                             do (let ((name name))
                                  (gtk:box-append box (profiler-link name count samples
                                                                     (lambda () (select-profile-function page name)))))))))
            (section "Called by" (profile-function-callers function))
            (section "Calls" (profile-function-callees function)))))))

(defun go-to-profiled-definition (name)
  (with-connection (connection)
    (request #'backend-find-definitions connection name :package "COMMON-LISP-USER"
             :on-ok (lambda (definitions)
                      (let ((found (remove-if (lambda (d) (getf (parse-location (second d)) :error)) definitions)))
                        (if found
                            (go-to-definition-choices (current-view) name found)
                            (message "No source found for ~a" name)))))))

(defun fill-profiler-page (page)
  (let ((profile (pp-profile page)))
    (gtk:label-set-text (pp-title page)
                        (cond ((null profile) "Nothing profiled")
                              ((profile-running profile) "Profiling all threads…")
                              (t (format nil "~:d sample~:p of ~a" (profile-samples profile)
                                         (profile-mode-title (profile-mode profile))))))
    (gtk:button-set-label (pp-run-button page) (if (and profile (profile-running profile)) "Stop" "Profile All Threads"))
    (setf (pp-path page) '())
    (update-flame-boxes page)
    (fill-profiler-list page)
    (fill-profiler-details page)))

;;; The flame graph

(defun update-flame-boxes (page)
  (let* ((profile (pp-profile page))
         (tree (and profile (plusp (profile-samples profile)) (profile-tree profile))))
    (setf (pp-boxes page) (and tree (flame-boxes tree :path (pp-path page))))
    (gtk:drawing-area-set-content-height (pp-flame page)
                                         (* *flame-row-height* (if (pp-boxes page) (flame-depth (pp-boxes page)) 1)))
    (gtk:widget-queue-draw (pp-flame page))))

(defun flame-color (name)
  "A warm color for NAME, the same each time."
  (let* ((hash (sxhash name))
         (hue (/ (mod hash 50) 360.0))                ; red to orange-yellow
         (light (+ 0.55 (/ (mod (floor hash 50) 20) 100.0))))
    ;; HSL to RGB, saturation 0.75
    (flet ((channel (n)
             (let* ((k (mod (+ n (* hue 12)) 12))
                    (a (* 0.75 (min light (- 1 light)))))
               (- light (* a (max -1 (min (- k 3) (- 9 k) 1)))))))
      (values (channel 0) (channel 8) (channel 4)))))

(defun flame-box-at (page x y width)
  (let ((depth (floor y *flame-row-height*)))
    (find-if (lambda (box)
               (destructuring-bind (node d bx bw) box
                 (declare (ignore node))
                 (and (= d depth) (<= (* bx width) x (* (+ bx bw) width)))))
             (pp-boxes page))))

(defun draw-flame-graph (page area cr width height)
  (declare (ignore height))
  (let ((boxes (pp-boxes page))
        (row *flame-row-height*)
        (selected (pp-selected page))
        (zoomed (length (pp-path page))))
    (if (null boxes)
        (let ((layout (gtk:widget-create-pango-layout area "No samples yet"))
              (color (gtk:widget-get-color area)))
          (cairo:set-source-rgba cr (gdk:rgba-red color) (gdk:rgba-green color) (gdk:rgba-blue color) 0.6d0)
          (cairo:move-to cr 10d0 4d0)
          (pango-cairo:show-layout cr layout))
        (let ((layout (gtk:widget-create-pango-layout area nil)))
          (pango:layout-set-ellipsize layout :end)
          (dolist (box boxes)
            (destructuring-bind (node depth x w) box
              (let ((px (* x width)) (pw (* w width)) (py (* depth row)))
                (when (>= pw 0.5)
                  (multiple-value-bind (r g b) (flame-color (first node))
                    (if (< depth zoomed)
                        (cairo:set-source-rgba cr r g b 0.45d0)
                        (cairo:set-source-rgba cr r g b 1d0)))
                  (cairo:rectangle cr (float px 1d0) (float py 1d0) (float (max 0.5 (- pw 1)) 1d0) (float (1- row) 1d0))
                  (cairo:fill cr)
                  (when (equal (first node) selected)
                    (cairo:set-source-rgba cr 0.1d0 0.1d0 0.1d0 1d0)
                    (cairo:set-line-width cr 2d0)
                    (cairo:rectangle cr (float (+ px 1) 1d0) (float (+ py 1) 1d0)
                                     (float (max 0.5 (- pw 3)) 1d0) (float (- row 3) 1d0))
                    (cairo:stroke cr))
                  (when (> pw 24)
                    (pango:layout-set-text layout (first node) -1)
                    (pango:layout-set-width layout (* 1024 (floor (- pw 6))))
                    (cairo:set-source-rgba cr 0.08d0 0.08d0 0.08d0 1d0)
                    (cairo:move-to cr (float (+ px 3) 1d0) (float (+ py 1) 1d0))
                    (pango-cairo:show-layout cr layout))))))))))

(defun setup-flame-graph (page)
  (let ((area (pp-flame page))
        (click (gtk:gesture-click-new)))
    (gtk:drawing-area-set-draw-func area (lambda (area cr width height) (draw-flame-graph page area cr width height)))
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore n))
                       (let ((box (flame-box-at page x y (gtk:widget-get-width area)))
                             (profile (pp-profile page)))
                         (when (and box profile)
                           (gtk:gesture-set-state gesture :claimed)
                           (destructuring-bind (node depth &rest more) box
                             (declare (ignore more))
                             (setf (pp-path page)
                                   (if (< depth (length (pp-path page)))
                                       (subseq (pp-path page) 0 depth) ; above the zoomed node: zoom out
                                       (flame-path-to (profile-tree profile) node)))
                             (update-flame-boxes page)
                             (when (find-profile-function profile (first node))
                               (setf (pp-selected page) (first node))
                               (fill-profiler-details page)))))))
    (gtk:widget-add-controller area click)
    (gobject:connect area :query-tooltip
                     (lambda (widget x y keyboard tooltip)
                       (declare (ignore keyboard))
                       (let ((box (flame-box-at page x y (gtk:widget-get-width widget)))
                             (profile (pp-profile page)))
                         (when (and box profile)
                           (let ((node (first box)))
                             (gtk:tooltip-set-text tooltip
                                                   (format nil "~a~%~:d sample~:p, ~a~%Click to zoom in~:[~;, or out~]"
                                                           (first node) (second node)
                                                           (profile-percent (second node) (profile-samples profile))
                                                           (pp-path page)))
                             t)))))))

;;; Asking the Lisp

(defun ensure-profiler-helper (connection then)
  "Send the helper to CONNECTION's Lisp (once), then call THEN."
  (unless (search "SBCL" (connection-implementation connection))
    (editor-error "The profiler needs SBCL (this Lisp is ~a)" (connection-implementation connection)))
  (if (eq *profiler-helper-connection* connection)
      (funcall then)
      (rex connection (swank-call "swank:interactive-eval-region" +profiler-helper+ 3 120)
           :package "COMMON-LISP-USER"
           :on-ok (lambda (reply)
                    (declare (ignore reply))
                    (setf *profiler-helper-connection* connection)
                    (funcall then)))))

(defun fetch-profile (connection)
  (rex connection (list (remote-symbol "cadre-profiler:report"))
       :on-ok (lambda (reply)
                (let ((profile (parse-profile-report reply)))
                  (setf (pp-profile *profiler-page*) profile)
                  (message "~:d sample~:p of ~a: see the Profiler page"
                           (profile-samples profile) (profile-mode-title (profile-mode profile))))
                (fill-profiler-page *profiler-page*)
                (show-profiler-page))))

(defun add-profiler-tab ()
  "Put the Profiler's tab in the panel without showing the page: a running
app can be profiled with Profile All Threads."
  (when *window*
    (gtk:stack-page-set-visible (panel-page (window-panel *window*) "profiler") t)))

(defun show-profiler-page ()
  (panel-show (window-panel *window*) "profiler")
  (set-panel-visible *window* t))

(defun profile-form-text (text package)
  "Run TEXT, a form, with the profiler on; then show where its time went."
  (with-connection (connection)
    (ensure-profiler-helper
     connection
     (lambda ()
       (message "Profiling…")
       (rex connection (swank-call "swank:interactive-eval"
                                   (format nil "(cadre-profiler:run (lambda () ~a~%) :mode ~s)" text (profiler-mode))
                                   3 120)
            :package package
            :on-ok (lambda (result)
                     (show-result result)
                     (fetch-profile connection))
            :on-abort (lambda (reason)
                        (message "Profiling abandoned~@[: ~a~]" reason)
                        (fetch-profile connection)))))))

;;; Commands

(define-command profile-form ()
  "Run the selection, or the top-level form at the cursor, with the
profiler on, and show where the time went. On a definition, ask for a call
to profile."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (package (view-package view)))
    (if (gtk:text-buffer-get-has-selection gtk-buffer)
        (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
          (declare (ignore has))
          (profile-form-text (gtk:text-buffer-get-text gtk-buffer start end t) package))
        (multiple-value-bind (line column) (cursor-line-column view)
          (let ((syntax (current-lisp-syntax)))
            (multiple-value-bind (sl sc el ec) (toplevel-form-bounds syntax line column)
              (unless sl (editor-error "Not in a top-level form"))
              (if (definition-form-p syntax line column)
                  (let ((name (definition-name-at view syntax line column)))
                    (open-picker (window-picker *window*)
                                 :placeholder "Profile which call?"
                                 :text (if (and name (not (find #\( name))) (format nil "(~a " name) "")
                                 :on-choose (lambda (text)
                                              (unless (string= (string-trim " " text) "")
                                                (profile-form-text text package)))))
                  (profile-form-text (view-region-text view sl sc el ec) package))))))))

(define-command profile-expression ()
  "Profile a form typed in: run it with the profiler on and show where the
time went."
  (let ((view (current-view)))
    (open-picker (window-picker *window*)
                 :placeholder "Profile the form"
                 :on-choose (lambda (text)
                              (unless (string= (string-trim " " text) "")
                                (profile-form-text text (if (and view (eq (buffer-major-mode (view-buffer view)) 'lisp-mode))
                                                            (view-package view)
                                                            (and (connected-p) (connection-package *connection*)))))))))

(define-command profile-all-threads ()
  "Sample every thread of the Lisp, a running app included, until Stop
Profiling."
  (with-connection (connection)
    (ensure-profiler-helper
     connection
     (lambda ()
       (rex connection (list (remote-symbol "cadre-profiler:start") :mode (profiler-mode))
            :on-ok (lambda (reply)
                     (declare (ignore reply))
                     (setf (pp-profile *profiler-page*) (make-profile :running t))
                     (fill-profiler-page *profiler-page*)
                     (show-profiler-page)
                     (message "Profiling all threads: Stop on the Profiler page to see the result")))))))

(define-command stop-profiling ()
  "Stop Profile All Threads and show where the time went."
  (with-connection (connection)
    (ensure-profiler-helper
     connection
     (lambda ()
       (rex connection (list (remote-symbol "cadre-profiler:stop"))
            :on-ok (lambda (reply) (declare (ignore reply)) (fetch-profile connection)))))))

(define-command clear-profile ()
  "Forget the samples on the Profiler page."
  (setf (pp-profile *profiler-page*) nil
        (pp-selected *profiler-page*) nil)
  (fill-profiler-page *profiler-page*))

(define-command show-profiler ()
  "Show the Profiler page."
  (show-profiler-page))

(defun profiler-disconnected ()
  (setf *profiler-helper-connection* nil)
  (when (and *profiler-page* (pp-profile *profiler-page*) (profile-running (pp-profile *profiler-page*)))
    (setf (profile-running (pp-profile *profiler-page*)) nil)
    (fill-profiler-page *profiler-page*)))
