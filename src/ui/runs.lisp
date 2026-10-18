;;;; runs.lisp — recent project processes, with their own output and controls.

(in-package #:cadre-ui)

(defstruct (project-run (:conc-name pr-))
  title root plan (started (get-universal-time))
  (start (get-internal-real-time)) end exit-code (status :running)
  (output (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
  (truncated nil) rerun stop available connection (stop-requested nil)
  ;; Whether what CONNECTION prints is this run's output too (not for a GTK
  ;; app, whose output comes from its Lisp's own stream).
  (capture-output t)
  ;; Read back from runs.sexp: from an earlier session of Cadre.
  (restored nil))

(defvar *project-runs* nil "Recent project processes, newest first; this session only.")
(defparameter *project-run-limit* 30)
(defparameter *project-run-output-limit* (* 256 1024))
(defparameter *project-run-saved-output-limit* (* 64 1024)
  "Characters of each run's output saved for later sessions (its last ones).")
(defvar *project-runs-loaded* nil "Whether runs.sexp has been read in this session.")
(defvar *build* nil)
(defvar *runs-page* nil)

(defstruct (runs-page (:conc-name rnp-))
  list output output-scroller end-mark search title rerun stop clear locations selected
  ;; The project filter: its drop-down and the roots it offers (nil: all).
  project-filter (project-roots (list nil))
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
      ;; A new run shows, under the filter if it's this project's.
      (let ((root (selected-project-root *runs-page*)))
        (when (and root (not (equal (project-root-key root) (project-root-key (pr-root run)))))
          (gtk:drop-down-set-selected (rnp-project-filter *runs-page*) 0)))
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
          (pr-status run) (cond ((or stopped (pr-stop-requested run)) :stopped) ((eql code 0) :passed) (t :failed)))
    (save-project-runs))
  (when *runs-page* (refresh-runs-page))
  run)

;;; Runs kept across sessions, in runs.sexp in the state directory: the
;;; finished ones, each with the end of its output. A restored run can be run
;;; again only if it was a process (its plan names a program); the closures
;;; that rerun or stop the others don't outlive the session.

(defun project-runs-file () (merge-pathnames "runs.sexp" (state-directory)))

(defun plain-data-p (value)
  "Whether VALUE is acyclic strings, numbers, keywords and lists of them."
  (let ((visiting (make-hash-table :test 'eq)))
    (labels ((plain (item)
               (typecase item
                 ((or string real keyword null) t)
                 (cons (unless (gethash item visiting)
                         (setf (gethash item visiting) t)
                         (unwind-protect (and (plain (car item)) (plain (cdr item)))
                           (remhash item visiting))))
                 (t nil))))
      (plain value))))

(defun project-run-record (run)
  "RUN, finished, as plain data for runs.sexp."
  (let* ((output (pr-output run))
         (limit *project-run-saved-output-limit*)
         (cut (> (length output) limit))
         (plan (pr-plan run)))
    (list :title (pr-title run) :root (namestring (pr-root run))
          :plan (if (plain-data-p plan) plan (list :description (princ-to-string (getf plan :description))))
          :started (pr-started run) :seconds (project-run-seconds run)
          :exit-code (and (integerp (pr-exit-code run)) (pr-exit-code run))
          :status (pr-status run)
          :output (if cut (subseq output (- (length output) limit)) (copy-seq output))
          :output-cut cut :truncated (and (pr-truncated run) t))))

(defun valid-run-record-p (record)
  (and (data-plist-p record)
       (stringp (getf record :title)) (stringp (getf record :root))
       (data-plist-p (getf record :plan)) (plain-data-p (getf record :plan))
       (integerp (getf record :started))
       (realp (getf record :seconds)) (not (minusp (getf record :seconds)))
       (typep (getf record :exit-code) '(or null integer))
       (member (getf record :status) '(:passed :failed :stopped))
       (stringp (getf record :output))))

(defun restore-project-run (record)
  (let* ((text (concatenate 'string
                            (if (getf record :output-cut) (format nil "[Earlier output wasn't kept.]~%") "")
                            (getf record :output)))
         (output (make-array (length text) :element-type 'character :adjustable t :fill-pointer (length text)
                                           :initial-contents text)))
    (make-project-run :title (getf record :title)
                      :root (uiop:ensure-directory-pathname (getf record :root))
                      :plan (getf record :plan) :started (getf record :started)
                      :start 0 :end (round (* (getf record :seconds) internal-time-units-per-second))
                      :exit-code (getf record :exit-code) :status (getf record :status)
                      :output output :truncated (getf record :truncated) :restored t)))

(defun save-project-runs ()
  "Write the finished runs to runs.sexp, once the earlier ones have been
read (load-project-runs), so they aren't written over."
  (when *project-runs-loaded*
   (handler-case
      (let ((records (loop for run in *project-runs*
                           unless (eq (pr-status run) :running) collect (project-run-record run))))
        (write-private-state (project-runs-file)
                             (lambda (out)
                               (with-standard-io-syntax
                                 (let ((*package* (find-package :keyword)))
                                   (prin1 (list :version 1 :runs records) out)
                                   (terpri out))))))
    (error (e) (message "Could not save the Runs history: ~a" e)))))

(defun read-project-runs (&optional (file (project-runs-file)))
  "Read saved runs, skipping malformed records without losing valid neighbors."
  (ignore-errors
    (with-open-file (in file :external-format :utf-8 :if-does-not-exist nil)
      (when in
        (with-standard-io-syntax
          (let* ((*read-eval* nil) (*package* (find-package :keyword)) (data (read in nil nil)))
            (when (and (data-plist-p data) (eql 1 (getf data :version))
                       (proper-data-list-p (getf data :runs)))
              (loop for record in (getf data :runs)
                    for run = (ignore-errors (when (valid-run-record-p record) (restore-project-run record)))
                    when run collect run))))))))

(defun load-project-runs ()
  "Add the runs of earlier sessions after this session's, once."
  (unless *project-runs-loaded*
    (setf *project-runs-loaded* t)
    (let ((restored (read-project-runs)))
      (setf *project-runs*
            (append *project-runs*
                    (subseq restored 0 (min (length restored)
                                            (max 0 (- *project-run-limit* (length *project-runs*))))))))))

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

(defun run-output-locations (run query &key (limit 20))
  "Up to LIMIT distinct file:line places that RUN printed on lines matching
QUERY, in files that exist, as (line location) each. Each file is checked on
disk once, however many places name it."
  (let ((seen '()) (exists (make-hash-table :test 'equal)) (found '()))
    (dolist (line (split-lines (pr-output run)) (nreverse found))
      (when (>= (length found) limit) (return (nreverse found)))
      (when (search query line :test #'char-equal)
        (let ((location (ignore-errors (project-run-location line (pr-root run)))))
          (when (and location (not (member location seen :test #'equal)))
            (push location seen)
            (let ((file (namestring (first location))))
              (when (multiple-value-bind (known present) (gethash file exists)
                      (if present known
                          (setf (gethash file exists) (and (ignore-errors (probe-file file)) t))))
                (push (list line location) found)))))))))

(defun refresh-run-locations (page run query)
  "List the file:line places RUN printed (run-output-locations)."
  (gtk:list-box-remove-all (rnp-locations page))
  (when run
    (loop for (line location) in (run-output-locations run query)
          do (let ((button (make-instance 'gtk:button
                                          :child (label line :xalign 0.0 :ellipsize :end)
                                          :hexpand t :css-classes '("flat")
                                          :tooltip-text "Open this file location"))
                   (location location))
               (gobject:connect button :clicked
                                (lambda (b) (declare (ignore b)) (show-project-run-location location)))
               (gtk:list-box-append (rnp-locations page) button)))))

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

(defun project-root-key (root) (namestring (uiop:ensure-directory-pathname root)))

(defun clearable-project-runs (&optional root)
  "The finished runs, of ROOT's project or of all: what clearing forgets."
  (remove-if (lambda (run) (eq (pr-status run) :running)) (visible-project-runs root)))

(defun clear-project-runs (&optional root)
  "Forget the finished runs of ROOT's project (all of them, if ROOT is nil),
here and in runs.sexp. Runs still going are kept. Returns how many went."
  (load-project-runs)
  (let ((cleared (clearable-project-runs root)))
    (setf *project-runs* (remove-if (lambda (run) (member run cleared)) *project-runs*))
    (save-project-runs)
    (when *runs-page* (refresh-runs-page))
    (length cleared)))

(defun visible-project-runs (root)
  "The runs the Runs page lists: all of them, or ROOT's when the project filter chooses one."
  (if root
      (let ((key (project-root-key root)))
        (remove-if-not (lambda (run) (string= key (project-root-key (pr-root run)))) *project-runs*))
      *project-runs*))

(defun project-run-roots ()
  "The projects of the runs, most recent first, as directory pathnames."
  (let ((roots '()))
    (dolist (run *project-runs* (nreverse roots))
      (unless (member (project-root-key (pr-root run)) roots :key #'project-root-key :test #'string=)
        (push (uiop:ensure-directory-pathname (pr-root run)) roots)))))

(defun project-root-label (root roots)
  "ROOT's folder name, or its whole path if another of ROOTS has that name too."
  (let ((name (car (last (pathname-directory root)))))
    (if (< 1 (count name roots :key (lambda (r) (car (last (pathname-directory r)))) :test #'equal))
        (uiop:native-namestring root)
        name)))

(defun project-run-row-label (run show-project)
  (multiple-value-bind (s m h day month year) (decode-universal-time (pr-started run))
    (multiple-value-bind (s2 m2 h2 today this-month this-year) (decode-universal-time (get-universal-time))
      (declare (ignore s2 m2 h2))
      (format nil "~:[~*~*~;~a ~d · ~]~2,'0d:~2,'0d:~2,'0d · ~@[~a · ~]~a · ~a · ~,1f s"
              (not (and (= day today) (= month this-month) (= year this-year)))
              (aref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month)) day
              h m s
              (and show-project (car (last (pathname-directory (uiop:ensure-directory-pathname (pr-root run))))))
              (pr-title run) (project-run-status-text run) (project-run-seconds run)))))

(defun selected-project-root (page)
  (nth (gtk:drop-down-get-selected (rnp-project-filter page)) (rnp-project-roots page)))

(defun refresh-project-filter (page)
  "Offer All projects and each project with runs, keeping the one chosen."
  (let ((roots (cons nil (project-run-roots)))
        (chosen (selected-project-root page)))
    (unless (equal (mapcar (lambda (r) (and r (project-root-key r))) roots)
                   (mapcar (lambda (r) (and r (project-root-key r))) (rnp-project-roots page)))
      (setf (rnp-project-roots page) roots)
      (gtk:drop-down-set-model (rnp-project-filter page)
                               (gtk:string-list-new
                                (cons "All projects" (mapcar (lambda (r) (project-root-label r (rest roots))) (rest roots)))))
      (gtk:drop-down-set-selected (rnp-project-filter page)
                                  (or (and chosen (position (project-root-key chosen) roots
                                                            :key (lambda (r) (and r (project-root-key r)))
                                                            :test #'equal))
                                      0)))))

(defun refresh-runs-page ()
  (let ((page *runs-page*))
    (setf (rnp-filling page) t)
    (unwind-protect
         (progn
           (refresh-project-filter page)
           (let* ((root (selected-project-root page))
                  (runs (visible-project-runs root)))
             (unless (member (rnp-selected page) runs)
               (setf (rnp-selected page) (first runs)))
             (gtk:list-box-remove-all (rnp-list page))
             (gtk:widget-set-sensitive (rnp-clear page) (and (clearable-project-runs root) t))
             (dolist (run runs)
               (let ((row (make-instance 'gtk:list-box-row
                                         :child (label (project-run-row-label run (null root))
                                                       :xalign 0.0 :ellipsize :end
                                                       :margin-start 8 :margin-end 8 :margin-top 4 :margin-bottom 4))))
                 (gtk:list-box-append (rnp-list page) row)
                 (when (eq run (rnp-selected page)) (gtk:list-box-select-row (rnp-list page) row)))))
           (select-project-run (rnp-selected page)))
      (setf (rnp-filling page) nil))))

(defun project-run-rerunnable-p (run)
  "Whether RUN can be run again now: it has a way to (its rerun function, or
a plan naming a program, for a run restored from an earlier session) and its
runner is free."
  (and run (not (eq (pr-status run) :running))
       (or (pr-rerun run) (getf (pr-plan run) :program))
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
         (project-filter (make-instance 'gtk:drop-down :model (gtk:string-list-new '("All projects"))
                                                       :tooltip-text "Show the runs of one project"))
         (title (label "" :xalign 0.0 :wrap t :wrap-mode :word-char :selectable t :margin-start 8 :margin-end 8))
         (rerun (make-instance 'gtk:button :label "Rerun" :tooltip-text "Save files and rerun this activity in its original project"))
         (stop (make-instance 'gtk:button :label "Stop"))
         (clear (make-instance 'gtk:button :icon-name "edit-clear-all-symbolic" :css-classes '("flat")
                                           :tooltip-text "Clear finished runs (of the project shown)"))
         (locations (make-instance 'gtk:list-box :selection-mode :none))
         (widget (gtk:build
                   (gtk:box :orientation :vertical :spacing 4
                     (gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 8 :margin-top 4
                       search
                       (gtk:box :spacing 4 rerun stop project-filter clear))
                     (gtk:scrolled-window :height-request 100 :child list)
                     title
                     output-scroller
                     (gtk:scrolled-window :max-content-height 80 :propagate-natural-height t :child locations)))))
    (setf *runs-page* (make-runs-page :list list :output output :output-scroller output-scroller
                                     :end-mark end-mark :search search :title title
                                     :project-filter project-filter
                                     :rerun rerun :stop stop :clear clear :locations locations :selected (first *project-runs*)))
    (gobject:connect list :row-selected
                     (lambda (box row) (declare (ignore box))
                       (when (and row (not (rnp-filling *runs-page*)))
                         (select-project-run (nth (gtk:list-box-row-get-index row)
                                                  (visible-project-runs (selected-project-root *runs-page*)))))))
    (gobject:connect project-filter "notify::selected"
                     (lambda (d pspec) (declare (ignore d pspec))
                       (unless (rnp-filling *runs-page*) (refresh-runs-page))))
    (gobject:connect search :search-changed (lambda (entry) (declare (ignore entry)) (refresh-run-output)))
    (gobject:connect rerun :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (rerun-project-run (rnp-selected *runs-page*))
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    (gobject:connect clear :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (call-command 'clear-runs)
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    (gobject:connect stop :clicked
                     (lambda (b) (declare (ignore b))
                       (handler-case (stop-project-run (rnp-selected *runs-page*))
                         (editor-error (e) (message "~a" (editor-error-message e))))))
    (load-project-runs)
    (setf (rnp-selected *runs-page*) (first *project-runs*))
    (refresh-runs-page)
    widget))

(define-command clear-runs ()
  "Forget the finished runs shown on the Runs page (one project's, or all),
after asking. Runs still going are kept."
  (let* ((root (and *runs-page* (selected-project-root *runs-page*)))
         (count (length (clearable-project-runs root))))
    (when (zerop count) (editor-error "There are no finished runs to clear"))
    (let ((dialog (adw:alert-dialog-new
                   "Clear finished runs?"
                   (format nil "Forget ~d finished run~:p of ~a, here and in later sessions. Runs still going are kept."
                           count (if root (project-root-label root (project-run-roots)) "all projects")))))
      (adw:alert-dialog-add-response dialog "cancel" "Cancel")
      (adw:alert-dialog-add-response dialog "clear" "Clear Runs")
      (adw:alert-dialog-set-response-appearance dialog "clear" :destructive)
      (adw:alert-dialog-set-close-response dialog "cancel")
      (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                 (lambda (response)
                   (when (string= response "clear")
                     (message "Cleared ~d finished run~:p" (clear-project-runs root))))))))

(define-command show-runs ()
  "Show recent process builds and tests, with separate output and rerun controls."
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "runs"))
