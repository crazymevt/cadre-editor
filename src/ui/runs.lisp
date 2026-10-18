;;;; runs.lisp — recent project processes, with their own output and controls.

(in-package #:cadre-ui)

(defstruct (project-run (:conc-name pr-))
  title root plan (started (get-universal-time))
  (start (get-internal-real-time)) end exit-code (status :running)
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (truncated nil) rerun stop available connection (stop-requested nil)
  ;; Whether what CONNECTION prints is this run's output too (not for a GTK
  ;; app, whose output comes from its Lisp's own stream).
  (capture-output t))

(defvar *project-runs* nil "Recent project processes, newest first; this session only.")
(defparameter *project-run-limit* 30)
(defparameter *project-run-output-limit* (* 256 1024))
(defvar *build* nil)
(defvar *runs-page* nil)

(defstruct (runs-page (:conc-name rnp-))
  list output output-scroller end-mark search title rerun stop locations selected
  (filling nil) (output-pending nil)
  ;; What the output view shows: which run, how much of its output, under
  ;; which filter, and whether with the truncation notice.
  shown-run (shown-length 0) shown-query shown-truncated)

(defun project-run-seconds (run)
  (/ (- (or (pr-end run) (get-internal-real-time)) (pr-start run))
     (float internal-time-units-per-second)))

(defun project-run-status-text (run)
  (case (pr-status run)
    (:running (if (pr-stop-requested run) "Stopping" "Running")) (:passed "Finished") (:stopped "Stopped")
    (otherwise (format nil "Failed~@[ (exit ~a)~]" (pr-exit-code run)))))

(defun begin-project-run (root plan title &key rerun stop available connection (capture-output t))
  (let ((run (make-project-run :root root :plan (copy-tree plan) :title title :rerun rerun :stop stop
                               :available available :connection connection
                               :capture-output capture-output)))
    (push run *project-runs*)
    ;; Never evict an active run: it still owns output and cancellation.
    (let ((remaining (max 0 (- *project-run-limit*
                               (count :running *project-runs* :key #'pr-status)))))
      (setf *project-runs*
            (remove-if-not (lambda (item)
                             (if (eq (pr-status item) :running) t
                                 (when (plusp remaining) (decf remaining) t)))
                           *project-runs*)))
    (when *runs-page*
      (setf (rnp-selected *runs-page*) run)
      (refresh-runs-page))
    run))

(defun append-project-run-output (run line &key (newline t))
  "Bound retained output; its original Output or REPL page keeps receiving it."
  (unless (pr-truncated run)
    (let* ((output (pr-output run))
           (remaining (max 0 (- *project-run-output-limit* (length output))))
           (count (min remaining (length line))))
      (loop for i below count do (vector-push-extend (char line i) output))
      (when newline
        (if (> remaining (length line))
            (vector-push-extend #\Newline output)
            (setf (pr-truncated run) t)))
      (when (> (length line) remaining) (setf (pr-truncated run) t))))
  (when (and *runs-page* (eq run (rnp-selected *runs-page*)))
    (let ((page *runs-page*))
      (unless (rnp-output-pending page)
        (setf (rnp-output-pending page) t)
        (glib:timeout-add glib:+priority-default+ 100
                          (lambda ()
                            (setf (rnp-output-pending page) nil)
                            (when (eq page *runs-page*) (update-run-output))
                            nil))))))

(defun finish-project-run (run code &key stopped)
  (when (eq (pr-status run) :running)
    (setf (pr-end run) (get-internal-real-time)
          (pr-exit-code run) code
          (pr-status run) (cond ((or stopped (pr-stop-requested run)) :stopped) ((eql code 0) :passed) (t :failed))))
  (when *runs-page* (refresh-runs-page))
  run)

(defun project-run-output-text (run &optional (query ""))
  (let ((text (pr-output run)))
    (concatenate 'string
                 (if (zerop (length query)) text
                     (with-output-to-string (out)
                       (dolist (line (split-lines text))
                         (when (search query line :test #'char-equal)
                           (write-line line out)))))
                 (if (pr-truncated run)
                     (format nil "~%[Retained output limit reached; full output remains in its original Output or REPL page.]~%")
                     ""))))

(defun project-run-location (line root)
  "Read a conventional file:line[:column] diagnostic. Positions are one-based."
  (cl-ppcre:register-groups-bind (file row column)
      ("^\\s*(.+?):([0-9]+)(?::([0-9]+))?(?::|\\s|$)" line)
    (let ((row (parse-integer row)) (column (if column (parse-integer column) 1)))
      (let ((path (ignore-errors (merge-pathnames file root))))
        (when (and path (not (wild-pathname-p path)) (plusp row) (plusp column))
          (list path row column))))))

(defun show-project-run-location (location)
  (destructuring-bind (file row column) location
    (open-file-path file :then (lambda (view) (goto-line-column view (1- row) (1- column) :extend nil)))))

(defun run-output-at-end-p (page)
  (let ((adjustment (gtk:scrolled-window-get-vadjustment (rnp-output-scroller page))))
    (>= (+ (gtk:adjustment-get-value adjustment) (gtk:adjustment-get-page-size adjustment))
        (- (gtk:adjustment-get-upper adjustment) 4))))

(defun scroll-run-output-to-end (page)
  (let ((view (rnp-output page)))
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (when (eq page *runs-page*)
                       (gtk:text-view-scroll-to-mark view (rnp-end-mark page) 0d0 t 0d0 1d0))
                     nil))))

(defun update-run-output ()
  "Show what the selected run printed since the view was filled. Only the new
text is added (so a selection and the scroll position stay), and the view
follows the end if it was there; under a filter, or after the truncation
notice, the view is filled again."
  (let* ((page *runs-page*) (run (rnp-selected page))
         (output (and run (pr-output run))))
    (if (and run (eq run (rnp-shown-run page))
             (zerop (length (gtk:editable-get-text (rnp-search page))))
             (equal "" (rnp-shown-query page))
             (eq (pr-truncated run) (rnp-shown-truncated page))
             (<= (rnp-shown-length page) (length output)))
        (when (< (rnp-shown-length page) (length output))
          (let ((at-end (run-output-at-end-p page))
                (buffer (gtk:text-view-get-buffer (rnp-output page))))
            (gtk:text-buffer-insert buffer (gtk:text-buffer-get-end-iter buffer)
                                    (subseq output (rnp-shown-length page)) -1)
            (setf (rnp-shown-length page) (length output))
            (when at-end (scroll-run-output-to-end page))))
        (refresh-run-output :locations nil))))

(defun refresh-run-output (&key (locations t))
  "Fill the output view with the selected run's output under the filter, and,
with LOCATIONS, list the file:line places in it."
  (let* ((page *runs-page*) (run (rnp-selected page))
         (query (gtk:editable-get-text (rnp-search page)))
         (at-end (or (not (eq run (rnp-shown-run page))) (run-output-at-end-p page))))
    (gtk:text-buffer-set-text (gtk:text-view-get-buffer (rnp-output page))
                             (if run (project-run-output-text run query) "") -1)
    (setf (rnp-shown-run page) run
          (rnp-shown-length page) (if run (length (pr-output run)) 0)
          (rnp-shown-query page) query
          (rnp-shown-truncated page) (and run (pr-truncated run)))
    (when at-end (scroll-run-output-to-end page))
    (when locations (refresh-run-locations page run query))))

(defun refresh-run-locations (page run query)
  "List up to 20 file:line places that RUN printed and that exist, each
checked on disk once."
  (gtk:list-box-remove-all (rnp-locations page))
  (when run
    (let ((seen '()) (count 0))
      (dolist (line (split-lines (pr-output run)))
        (when (and (< count 20) (search query line :test #'char-equal))
          (let ((location (ignore-errors (project-run-location line (pr-root run)))))
            (when (and location (not (member (first location) seen :test #'equal)))
              (push (first location) seen)
              (when (ignore-errors (probe-file (first location)))
                (incf count)
                (let ((button (make-instance 'gtk:button
                                             :child (label line :xalign 0.0 :ellipsize :end)
                                             :hexpand t :css-classes '("flat")
                                             :tooltip-text "Open this file location")))
                  (gobject:connect button :clicked
                                   (lambda (b) (declare (ignore b)) (show-project-run-location location)))
                  (gtk:list-box-append (rnp-locations page) button))))))))))

(defun select-project-run (run)
  (let ((page *runs-page*))
    (setf (rnp-selected page) run)
    (gtk:label-set-text (rnp-title page)
                        (if run (format nil "~a · ~a · ~,1f s~%~a~%~a"
                                        (pr-title run) (project-run-status-text run) (project-run-seconds run)
                                        (uiop:native-namestring (pr-root run)) (getf (pr-plan run) :description))
                            "Build, run tests, or start an app to record a run."))
    (gtk:widget-set-sensitive (rnp-rerun page) (project-run-rerunnable-p run))
    (gtk:widget-set-sensitive (rnp-stop page) (project-run-stoppable-p run))
    (refresh-run-output)))

(defun refresh-runs-page ()
  (let ((page *runs-page*))
    (setf (rnp-filling page) t)
    (unwind-protect
         (progn
           (gtk:list-box-remove-all (rnp-list page))
           (dolist (run *project-runs*)
             (multiple-value-bind (s m h) (decode-universal-time (pr-started run))
               (let ((row (make-instance 'gtk:list-box-row
                                        :child (label (format nil "~2,'0d:~2,'0d:~2,'0d · ~a · ~a · ~,1f s"
                                                              h m s (pr-title run) (project-run-status-text run)
                                                              (project-run-seconds run))
                                                      :xalign 0.0 :ellipsize :end
                                                      :margin-start 8 :margin-end 8 :margin-top 4 :margin-bottom 4))))
                 (gtk:list-box-append (rnp-list page) row)
                 (when (eq run (rnp-selected page)) (gtk:list-box-select-row (rnp-list page) row)))))
           (select-project-run (rnp-selected page)))
      (setf (rnp-filling page) nil))))

(defun project-run-rerunnable-p (run)
  (and run (not (eq (pr-status run) :running))
       (if (pr-available run) (funcall (pr-available run)) (null *build*))))

(defun project-run-stoppable-p (run)
  (and run (eq (pr-status run) :running) (not (pr-stop-requested run))
       (or (pr-stop run) (eq run (getf *build* :run)))))

(defun stop-project-run (run)
  (unless (project-run-stoppable-p run) (editor-error "This run cannot be stopped"))
  (setf (pr-stop-requested run) t)
  (handler-case
      (if (pr-stop run) (funcall (pr-stop run)) (call-command 'stop-build))
    (error (e) (setf (pr-stop-requested run) nil) (error e)))
  (when *runs-page* (refresh-runs-page)))

(defun capture-project-run-output (connection text)
  "Retain connection output while its runs are active; keep the original REPL too."
  (when connection
    (dolist (run *project-runs*)
      (when (and (eq connection (pr-connection run)) (eq :running (pr-status run))
                 (pr-capture-output run))
        (append-project-run-output run text :newline nil)))))

(defun disconnect-project-runs (connection reason)
  (dolist (run *project-runs*)
    (when (and connection (eq connection (pr-connection run)) (eq :running (pr-status run)))
      (append-project-run-output run (format nil "Disconnected: ~a" reason))
      (finish-project-run run nil))))

(defun rerun-project-run (run)
  (unless run (editor-error "Select a run first"))
  (unless (project-run-rerunnable-p run) (editor-error "This runner is busy"))
  (let ((root (pr-root run)) (plan (copy-tree (pr-plan run))) (title (pr-title run)))
    (unless (uiop:directory-exists-p root) (editor-error "The run's project folder no longer exists"))
    (save-project-then root
                       (lambda ()
                         (unless (project-run-rerunnable-p run) (editor-error "This runner is busy"))
                         (if (pr-rerun run) (funcall (pr-rerun run))
                             (run-project-process root plan :what title))))))

(defun make-runs-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :single))
         (output (make-instance 'gtk:text-view :editable nil :monospace t :wrap-mode :word-char
                                               :left-margin 8 :top-margin 4 :vexpand t))
         (output-scroller (make-instance 'gtk:scrolled-window :vexpand t :child output))
         (end-mark (let ((buffer (gtk:text-view-get-buffer output)))
                     (gtk:text-buffer-create-mark buffer "cadre-runs-end" (gtk:text-buffer-get-end-iter buffer) nil)))
         (search (make-instance 'gtk:search-entry :placeholder-text "Filter output…" :hexpand t))
         (title (label "" :xalign 0.0 :wrap t :wrap-mode :word-char :selectable t :margin-start 8 :margin-end 8))
         (rerun (make-instance 'gtk:button :label "Rerun" :tooltip-text "Save files and rerun this activity in its original project"))
         (stop (make-instance 'gtk:button :label "Stop"))
         (locations (make-instance 'gtk:list-box :selection-mode :none))
         (widget (gtk:build
                   (gtk:box :orientation :vertical :spacing 4
                     (gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 4
                       search
                       (gtk:box :spacing 4 rerun stop))
                     (gtk:scrolled-window :height-request 100 :child list)
                     title
                     output-scroller
                     (gtk:scrolled-window :max-content-height 80 :propagate-natural-height t :child locations)))))
    (setf *runs-page* (make-runs-page :list list :output output :output-scroller output-scroller
                                     :end-mark end-mark :search search :title title
                                     :rerun rerun :stop stop :locations locations :selected (first *project-runs*)))
    (gobject:connect list :row-selected
                     (lambda (box row) (declare (ignore box))
                       (when (and row (not (rnp-filling *runs-page*)))
                         (select-project-run (nth (gtk:list-box-row-get-index row) *project-runs*)))))
    (gobject:connect search :search-changed (lambda (entry) (declare (ignore entry)) (refresh-run-output)))
    (gobject:connect rerun :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (rerun-project-run (rnp-selected *runs-page*))
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    (gobject:connect stop :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (stop-project-run (rnp-selected *runs-page*))
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    (refresh-runs-page)
    widget))

(define-command show-runs ()
  "Show recent process builds and tests, with separate output and rerun controls."
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "runs"))
