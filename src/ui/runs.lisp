;;;; runs.lisp — recent project processes, with their own output and controls.

(in-package #:cadre-ui)

(defstruct (project-run (:conc-name pr-))
  title root plan (started (get-universal-time))
  (start (get-internal-real-time)) end exit-code (status :running)
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (truncated nil))

(defvar *project-runs* nil "Recent project processes, newest first; this session only.")
(defparameter *project-run-limit* 30)
(defparameter *project-run-output-limit* (* 256 1024))
(defvar *build* nil)
(defvar *runs-page* nil)

(defstruct (runs-page (:conc-name rnp-))
  list output search title rerun stop locations selected (filling nil) (output-pending nil))

(defun project-run-seconds (run)
  (/ (- (or (pr-end run) (get-internal-real-time)) (pr-start run))
     (float internal-time-units-per-second)))

(defun project-run-status-text (run)
  (case (pr-status run)
    (:running "Running") (:passed "Finished") (:stopped "Stopped")
    (otherwise (format nil "Failed~@[ (exit ~a)~]" (pr-exit-code run)))))

(defun begin-project-run (root plan title)
  (let ((run (make-project-run :root root :plan (copy-tree plan) :title title)))
    (push run *project-runs*)
    (when (> (length *project-runs*) *project-run-limit*)
      (setf *project-runs* (subseq *project-runs* 0 *project-run-limit*)))
    (when *runs-page*
      (setf (rnp-selected *runs-page*) run)
      (refresh-runs-page))
    run))

(defun append-project-run-output (run line)
  "Bound retained output; the Output page still receives every line."
  (unless (pr-truncated run)
    (let* ((output (pr-output run))
           (remaining (max 0 (- *project-run-output-limit* (length output))))
           (count (min remaining (length line))))
      (loop for i below count do (vector-push-extend (char line i) output))
      (if (> remaining (length line))
          (vector-push-extend #\Newline output)
          (setf (pr-truncated run) t))))
  (when (and *runs-page* (eq run (rnp-selected *runs-page*)))
    (let ((page *runs-page*))
      (unless (rnp-output-pending page)
        (setf (rnp-output-pending page) t)
        (glib:timeout-add glib:+priority-default+ 100
                          (lambda ()
                            (setf (rnp-output-pending page) nil)
                            (when (eq page *runs-page*) (refresh-run-output))
                            nil))))))

(defun finish-project-run (run code &key stopped)
  (when (eq (pr-status run) :running)
    (setf (pr-end run) (get-internal-real-time)
          (pr-exit-code run) code
          (pr-status run) (cond (stopped :stopped) ((eql code 0) :passed) (t :failed))))
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
                     (format nil "~%[Retained output limit reached; full output is on the Output page.]~%")
                     ""))))

(defun project-run-location (line root)
  "Read a conventional file:line[:column] diagnostic. Positions are one-based."
  (cl-ppcre:register-groups-bind (file row column)
      ("^\\s*(.+?):([0-9]+)(?::([0-9]+))?(?::|\\s|$)" line)
    (let ((row (parse-integer row)) (column (if column (parse-integer column) 1)))
      (when (and (plusp row) (plusp column))
        (list (merge-pathnames file root) row column)))))

(defun show-project-run-location (location)
  (destructuring-bind (file row column) location
    (open-file-path file :then (lambda (view) (goto-line-column view (1- row) (1- column) :extend nil)))))

(defun refresh-run-output ()
  (let* ((page *runs-page*) (run (rnp-selected page))
         (query (gtk:editable-get-text (rnp-search page))))
    (gtk:text-buffer-set-text (gtk:text-view-get-buffer (rnp-output page))
                             (if run (project-run-output-text run query) "") -1)
    (gtk:list-box-remove-all (rnp-locations page))
    (when run
      (let ((seen nil) (count 0))
        (dolist (line (split-lines (pr-output run)))
          (when (and (< count 20) (search query line :test #'char-equal))
            (let ((location (ignore-errors (project-run-location line (pr-root run)))))
              (when (and location (probe-file (first location)) (not (member location seen :test #'equal)))
                (push location seen)
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
                            "Build or run tests in a new Lisp to record a run."))
    (gtk:widget-set-sensitive (rnp-rerun page) (and run (not (eq (pr-status run) :running)) (null *build*)))
    (gtk:widget-set-sensitive (rnp-stop page) (and run (eq run (getf *build* :run))))
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

(defun rerun-project-run (run)
  (unless run (editor-error "Select a run first"))
  (when *build* (editor-error "~a is already running" (getf *build* :what)))
  (when (eq (pr-status run) :running) (editor-error "This run is still running"))
  (let ((root (pr-root run)) (plan (copy-tree (pr-plan run))) (title (pr-title run)))
    (unless (uiop:directory-exists-p root) (editor-error "The run's project folder no longer exists"))
    (save-project-then root (lambda () (run-project-process root plan :what title)))))

(defun make-runs-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :single))
         (output (make-instance 'gtk:text-view :editable nil :monospace t :wrap-mode :word-char
                                               :left-margin 8 :top-margin 4 :vexpand t))
         (search (make-instance 'gtk:search-entry :placeholder-text "Filter output…" :hexpand t))
         (title (label "" :xalign 0.0 :wrap t :wrap-mode :word-char :selectable t :margin-start 8 :margin-end 8))
         (rerun (make-instance 'gtk:button :label "Rerun" :tooltip-text "Save files and rerun this process in its original project"))
         (stop (make-instance 'gtk:button :label "Stop"))
         (locations (make-instance 'gtk:list-box :selection-mode :none))
         (widget (gtk:build
                   (gtk:box :orientation :vertical :spacing 4
                     (gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 4
                       search
                       (gtk:box :spacing 4 rerun stop))
                     (gtk:scrolled-window :height-request 100 :child list)
                     title
                     (gtk:scrolled-window :vexpand t :child output)
                     (gtk:scrolled-window :max-content-height 80 :propagate-natural-height t :child locations)))))
    (setf *runs-page* (make-runs-page :list list :output output :search search :title title
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
    (gobject:connect stop :clicked (lambda (b) (declare (ignore b)) (call-command 'stop-build)))
    (refresh-runs-page)
    widget))

(define-command show-runs ()
  "Show recent process builds and tests, with separate output and rerun controls."
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "runs"))
