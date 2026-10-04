;;;; perf.lisp — measure Cadre against its performance budgets:
;;;;   make perf
;;;; Opens a real window on generated files and times what users feel:
;;;;
;;;;  - a key press to its repaint in a 10,000-line Lisp file (budget 16 ms)
;;;;  - scrolling that file a page at a time (16 ms a page)
;;;;  - opening a 1 MB Lisp file until it is painted (300 ms)
;;;;  - with the JavaScript grammar installed: opening and typing in 10,000
;;;;    lines of JavaScript, and the stall after an edit in 45,000 lines
;;;;  - a project of 5,000 files: the explorer, a 1,000-file folder, and the
;;;;    file list for Quick Open, without stalling (no stall over 100 ms)
;;;;
;;;; A key's time is the work it does (Cadre's key handling and the edit's
;;;; signal handlers) plus the next frame's layout and paint; the wait for the
;;;; display's next refresh is left out, as it doesn't depend on Cadre. The
;;;; keys are typed once, unmeasured, before the samples: the first use of a
;;;; code path costs extra once per session.
;;;; Throughout, a timer that should fire every 5 ms notices when the main
;;;; loop is busy: the longest gap, less 5 ms, is the longest stall, which is
;;;; what deferred work (highlighting, fold ranges, the outline…) costs.
;;;;
;;;; Prints a table, writes it to build/perf/latest.txt, and exits 1 if a
;;;; budget is missed. With CADRE_PERF_PROFILE=1 it also runs SBCL's
;;;; statistical profiler over the measurements and prints where time went.

(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings: a checkout next to Cadre if there is one, otherwise
;; wherever Quicklisp finds them (~/quicklisp/local-projects/).
(let ((gtk4 (probe-file "../gtk4/gtk4.asd")))
  (when gtk4 (push (uiop:pathname-directory-pathname gtk4) asdf:*central-registry*)))
(ql:quickload :cadre :silent t)
(when (equal (uiop:getenv "CADRE_PERF_PROFILE") "1") (require :sb-sprof))

(defpackage #:cadre-perf (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-perf)

(defvar *profile* (equal (uiop:getenv "CADRE_PERF_PROFILE") "1"))

(defvar *out* (merge-pathnames "build/perf/" (truename ".")))
(defvar *root* (merge-pathnames (format nil "cadre-perf-~d/" (get-universal-time))
                                (uiop:temporary-directory)))
(defvar *results* '() "(name value budget unit detail), newest first.")

(defun now-ms ()
  (/ (get-internal-real-time) (/ internal-time-units-per-second 1000.0d0)))

(defun result (name value budget &optional (unit "ms") detail)
  (push (list name value budget unit detail) *results*)
  (format t "~&~:[MISS~;ok  ~] ~a: ~,1f ~a~@[ (budget ~a)~]~@[ — ~a~]~%"
          (or (null budget) (<= value budget)) name value unit budget detail))

(defun percentile (values p)
  (let ((sorted (sort (copy-list values) #'<)))
    (nth (min (1- (length sorted)) (floor (* p (length sorted)))) sorted)))

;;; Generated files

(defun write-defun (out i)
  (format out "(defun fn-~d (a b &optional (c 1))~%  \"Docstring for fn-~d.\"~%  (let ((x (+ a b))~%        (y (* a c)))~%    (when (> x y)~%      (format t \"~~a ~~a~~%\" x y))~%    (loop for k from 0 below x~%          collect (list k (fn-helper k y)))))~%~%~%" i i))

(defun write-lisp-file (path &key lines bytes)
  "A file of ten-line definitions: LINES lines, or at least BYTES bytes."
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for i from 0
          while (if lines (< (* i 10) lines) (< (file-position out) bytes))
          do (write-defun out i)))
  path)

(defun write-tree (directory)
  "5,000 files: 40 folders of 100, and one folder of 1,000."
  (dotimes (d 40)
    (dotimes (f 100)
      (let ((path (merge-pathnames (format nil "pkg~2,'0d/file~3,'0d.lisp" d f) directory)))
        (ensure-directories-exist path)
        (with-open-file (out path :direction :output :if-exists :supersede)
          (format out "(defun f-~d-~d () ~d)~%" d f f)))))
  (dotimes (f 1000)
    (let ((path (merge-pathnames (format nil "many/item~4,'0d.txt" f) directory)))
      (ensure-directories-exist path)
      (with-open-file (out path :direction :output :if-exists :supersede)
        (format out "item ~d~%" f))))
  directory)

(defvar *files* (merge-pathnames "files/" *root*) "The project Cadre starts on.")
(defvar *big* (write-lisp-file (merge-pathnames "big.lisp" *files*) :lines 10000))
(defvar *mb* (write-lisp-file (merge-pathnames "mb.lisp" *files*) :bytes (* 1024 1024)))
(defvar *tree* (write-tree (merge-pathnames "tree/" *root*)))

(defun write-js-file (path lines)
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (loop for i from 0 while (< (* i 10) lines)
          do (format out "// Function ~d~%export function fn~d(a, b = 1) {~%  const x = a + b;~%  if (x > ~d) {~%    console.log(`big ${x}`);~%  }~%  return [x, { key: \"value\", n: ~d }];~%}~%~%~%" i i i i i)))
  path)

(defvar *js* (write-js-file (merge-pathnames "big.js" *files*) 10000))
(defvar *big-js* (write-js-file (merge-pathnames "huge.js" *files*) 45000))
(defvar *grammars* (tree-sitter-language-installed-p "javascript"))

;;; Watching the main loop and the frame clock

(defvar *stall-max* 0)
(defvar *stall-last* nil)

(defun start-stall-watch ()
  (setf *stall-last* (now-ms))
  (glib:timeout-add glib:+priority-default+ 5
                    (lambda ()
                      (let ((now (now-ms)))
                        (setf *stall-max* (max *stall-max* (- now *stall-last* 5))
                              *stall-last* now))
                      t)))

(defun reset-stalls () (setf *stall-max* 0 *stall-last* (now-ms)))

(defun after-next-paint (widget function)
  "Call FUNCTION with the start and end (ms) of WIDGET's next frame's work."
  (let* ((clock (gtk:widget-get-frame-clock widget))
         (start nil)
         (before nil)
         (after nil))
    (setf before (gobject:connect clock "before-paint" (lambda (c) (declare (ignore c)) (setf start (now-ms))))
          after (gobject:connect clock "after-paint"
                                 (lambda (c)
                                   (declare (ignore c))
                                   (gobject:disconnect clock before)
                                   (gobject:disconnect clock after)
                                   (let ((end (now-ms)))
                                     ;; Defer: don't run the next step inside the frame.
                                     (glib:idle-add glib:+priority-default+
                                                    (lambda () (funcall function (or start end) end) nil))))))
    (gtk:widget-queue-draw widget)))

;;; Steps: each is a function of a continuation

(defvar *steps* '())

(defmacro step* ((next) &body body)
  `(push (lambda (,next) ,@body) *steps*))

(defun run-steps (steps)
  (if steps
      (funcall (first steps) (lambda () (glib:idle-add glib:+priority-default+ (lambda () (run-steps (rest steps)) nil))))
      (finish)))

(defun wait-until (predicate then &key (timeout 30000))
  (let ((deadline (+ (now-ms) timeout)))
    (glib:timeout-add glib:+priority-default+ 10
                      (lambda ()
                        (cond ((funcall predicate) (funcall then) nil)
                              ((> (now-ms) deadline)
                               (format t "~&Timed out waiting.~%")
                               (funcall then) nil)
                              (t t))))))

(defun pause (ms then)
  (glib:timeout-add glib:+priority-default+ ms (lambda () (funcall then) nil)))

;;; Typing

(defun keyval (name) (gdk:keyval-from-name name))

(defun type-key (name text)
  "Press the key NAME as GTK would deliver it: through Cadre's key handling,
and, if Cadre doesn't take it, as the text view would."
  (let ((buffer (buffer-text (current-buffer))))
    (unless (cadre-ui::handle-key *window* (keyval name) nil)
      (cond (text (gtk:text-buffer-insert-interactive-at-cursor buffer text -1 t))
            ((string= name "BackSpace")
             (gtk:text-buffer-backspace buffer (cadre-ui::cursor-iter buffer) t t))))))

(defparameter *keys*
  '(("a" "a") ("b" "b") ("c" "c") ("parenleft" "(") ("x" "x") ("space" " ") ("y" "y")
    ("parenright" ")") ("Return" nil) ("BackSpace" nil) ("BackSpace" nil) ("BackSpace" nil))
  "Typing at a place: letters, a list, a newline, deleting.")

(defun measure-keys (keys samples then)
  "Type KEYS, one every 80 ms; push (work frame) for each onto SAMPLES (a cons)."
  (if (null keys)
      (funcall then)
      (destructuring-bind (name text) (first keys)
        (let ((t0 (now-ms)))
          (type-key name text)
          (let ((t1 (now-ms)))
            (after-next-paint (view-text-view (current-view))
                              (lambda (start end)
                                (push (list (- t1 t0) (- end start) name) (car samples))
                                (pause 80 (lambda () (measure-keys (rest keys) samples then))))))))))

(defun goto (line)
  (cadre-ui::goto-line-column (current-view) line 0 :extend nil)
  (let* ((buffer (buffer-text (current-buffer)))
         (it (cadre-ui::cursor-iter buffer)))
    (unless (gtk:text-iter-ends-line it) (gtk:text-iter-forward-to-line-end it))
    (gtk:text-buffer-place-cursor buffer it))
  (cadre-ui::scroll-to-cursor (current-view)))

(defun key-result (name samples budget)
  (let* ((totals (mapcar (lambda (s) (+ (first s) (second s))) samples))
         (worst (find (reduce #'max totals) samples :key (lambda (s) (+ (first s) (second s))))))
    (result name (percentile totals 0.95) budget "ms"
            (format nil "95th percentile of ~d keys; median ~,1f, worst ~,1f (~a: work ~,1f + frame ~,1f); work ~,1f + frame ~,1f at the median"
                    (length totals) (percentile totals 0.5) (reduce #'max totals)
                    (third worst) (first worst) (second worst)
                    (percentile (mapcar #'first samples) 0.5) (percentile (mapcar #'second samples) 0.5)))))

;;; The steps

(defvar *startup-start* nil)
(defvar *startup-paint* nil)

(step* (next)
  (result "Start-up to the first frame" (- *startup-paint* *startup-start*) nil "ms" "from cadre-ui:main")
  (start-stall-watch)
  (let ((t0 (now-ms)))
    (open-file-path *big*
                    :then (lambda (view)
                            (after-next-paint (view-text-view view)
                                              (lambda (start end)
                                                (declare (ignore start))
                                                (result "Opening a 10,000-line Lisp file" (- end t0) 300 "ms"
                                                        (format nil "~d KB" (round (with-open-file (in *big*) (file-length in)) 1024)))
                                                (pause 1000 next)))))))

(step* (next)
  ;; Warm up: the first use of a code path binds foreign functions and fills
  ;; CLOS caches (a one-time 20 ms or so), which isn't what typing costs.
  (goto 7003)
  (pause 300 (lambda () (measure-keys *keys* (list '()) (lambda () (pause 500 next))))))

(dolist (place '((3 "near the top") (5003 "in the middle") (9993 "near the end")))
  (destructuring-bind (line where) place
    (step* (next)
      (goto line)
      (pause 300 (lambda ()
                   (reset-stalls)
                   (let ((samples (list '())))
                     (measure-keys *keys* samples
                                   (lambda ()
                                     (key-result (format nil "A key press to its repaint, ~a of 10,000 lines" where)
                                                 (car samples) 16)
                                     (pause 800 (lambda ()
                                                  (result (format nil "Longest stall while typing ~a" where) *stall-max* 100)
                                                  (funcall next)))))))))))

(step* (next)
  ;; A lone double quote turns the rest of the file into a string.
  (goto 5003)
  (pause 300 (lambda ()
               (reset-stalls)
               (let* ((buffer (buffer-text (current-buffer)))
                      (t0 (now-ms)))
                 (gtk:text-buffer-insert-at-cursor buffer "\"" -1)
                 (let ((t1 (now-ms)))
                   (after-next-paint (view-text-view (current-view))
                                     (lambda (start end)
                                       (result "Opening a string in the middle of 10,000 lines" (+ (- t1 t0) (- end start)) 16)
                                       (pause 1200 (lambda ()
                                                     (result "Longest stall after opening the string" *stall-max* 100)
                                                     (gtk:text-buffer-backspace buffer (cadre-ui::cursor-iter buffer) t t)
                                                     (pause 1200 next))))))))))

(step* (next)
  (goto 0)
  (pause 300 (lambda ()
               (reset-stalls)
               (let ((samples (list '())) (count 40))
                 (labels ((page (n)
                            (if (zerop n)
                                (let ((totals (mapcar (lambda (s) (+ (first s) (second s))) (car samples))))
                                  (result "Scrolling a page down in 10,000 lines" (percentile totals 0.95) 16 "ms"
                                          (format nil "95th percentile of ~d pages; median ~,1f, worst ~,1f"
                                                  count (percentile totals 0.5) (reduce #'max totals)))
                                  (result "Longest stall while scrolling" *stall-max* 100)
                                  (funcall next))
                                (let ((t0 (now-ms)))
                                  (call-command 'cadre-ui::scroll-down-page)
                                  (let ((t1 (now-ms)))
                                    (after-next-paint (view-text-view (current-view))
                                                      (lambda (start end)
                                                        (push (list (- t1 t0) (- end start)) (car samples))
                                                        (pause 50 (lambda () (page (1- n)))))))))))
                   (page count))))))

(step* (next)
  (if (not *grammars*)
      (progn (format t "~&(no JavaScript grammar: skipping the JavaScript measurements)~%") (funcall next))
      (let ((t0 (now-ms)))
        (open-file-path *js*
                        :then (lambda (view)
                                (after-next-paint (view-text-view view)
                                                  (lambda (start end)
                                                    (declare (ignore start))
                                                    (result "Opening a 10,000-line JavaScript file" (- end t0) 300 "ms")
                                                    (pause 1000 next))))))))

(step* (next)
  (if (not *grammars*)
      (funcall next)
      (progn
        (goto 5005)
        (pause 300 (lambda ()
                     (reset-stalls)
                     (let ((samples (list '())))
                       (measure-keys *keys* samples
                                     (lambda ()
                                       (key-result "A key press to its repaint, in the middle of 10,000 lines of JavaScript"
                                                   (car samples) 16)
                                       (pause 800 (lambda ()
                                                    (result "Longest stall while typing JavaScript" *stall-max* 100)
                                                    (funcall next)))))))))))

(step* (next)
  (if (not *grammars*)
      (funcall next)
      (open-file-path *big-js*
                      :then (lambda (view)
                              (declare (ignore view))
                              (pause 1500 (lambda ()
                                            (goto 22005)
                                            (pause 300 (lambda ()
                                                         (reset-stalls)
                                                         (type-key "x" "x")
                                                         ;; The parse (about 70 ms here) runs on a thread.
                                                         (pause 1000 (lambda ()
                                                                       (result "Longest stall after an edit in 45,000 lines of JavaScript"
                                                                               *stall-max* 100)
                                                                       (funcall next)))))))))))

(step* (next)
  (reset-stalls)
  (let ((t0 (now-ms)))
    (open-file-path *mb*
                    :then (lambda (view)
                            (after-next-paint (view-text-view view)
                                              (lambda (start end)
                                                (declare (ignore start))
                                                (result "Opening a 1 MB Lisp file" (- end t0) 300 "ms"
                                                        (format nil "~d lines" (gtk:text-buffer-get-line-count (buffer-text (view-buffer view)))))
                                                (pause 1500 (lambda ()
                                                              (result "Longest stall after opening it" *stall-max* 100)
                                                              (funcall next)))))))))

(step* (next)
  (let* ((t0 (now-ms))
         (count (length (cadre-ui::quick-open-files *tree*))))
    (result "Listing 5,000 files for Quick Open" (- (now-ms) t0) 300 "ms" (format nil "~d files" count)))
  (funcall next))

(defun explorer-model ()
  (let* ((scrolled (adw:bin-get-child (cadre-ui::window-explorer-holder *window*)))
         (list-view (gtk:scrolled-window-get-child scrolled)))
    (values (gtk:list-view-get-model list-view) list-view)))

(defun explorer-count ()
  (gio:list-model-get-n-items (explorer-model)))

(step* (next)
  (reset-stalls)
  (let ((t0 (now-ms)))
    (open-project *tree*)
    (wait-until (lambda () (>= (explorer-count) 41))
                (lambda ()
                  (after-next-paint (nth-value 1 (explorer-model))
                                    (lambda (start end)
                                      (declare (ignore start))
                                      (result "Showing a 5,000-file project in the explorer" (- end t0) 300 "ms"
                                              (format nil "~d top-level entries" (explorer-count)))
                                      (pause 1500 (lambda ()
                                                    (result "Longest stall opening the project" *stall-max* 100)
                                                    (funcall next)))))))))

(step* (next)
  (reset-stalls)
  (let* ((selection (explorer-model))
         (tree (gtk:single-selection-get-model selection))
         (row (loop for i below (gio:list-model-get-n-items tree)
                    for row = (gtk:tree-list-model-get-row tree i)
                    when (string= "many" (gio:file-info-get-name (gtk:tree-list-row-get-item row)))
                      return row))
         (t0 (now-ms)))
    (gtk:tree-list-row-set-expanded row t)
    (wait-until (lambda () (>= (explorer-count) 1041))
                (lambda ()
                  (after-next-paint (nth-value 1 (explorer-model))
                                    (lambda (start end)
                                      (declare (ignore start))
                                      (result "Opening a 1,000-file folder in the explorer" (- end t0) 300 "ms")
                                      (pause 1500 (lambda ()
                                                    (result "Longest stall opening the folder" *stall-max* 100)
                                                    (funcall next)))))))))

(setf *steps* (reverse *steps*))

;;; Reporting

(defun finish ()
  (let* ((results (reverse *results*))
         (missed (remove-if (lambda (r) (or (null (third r)) (<= (second r) (third r)))) results))
         (report (with-output-to-string (s)
                   (format s "Cadre performance, ~a~%~a ~a, ~a~%~%"
                           (multiple-value-bind (sec min hour day month year) (decode-universal-time (get-universal-time))
                             (format nil "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour min sec))
                           (lisp-implementation-type) (lisp-implementation-version) (machine-type))
                   (format s "~68a ~10@a ~8@a~%" "Measurement" "Result" "Budget")
                   (format s "~v,,,'-a~%" 88 "")
                   (dolist (r results)
                     (destructuring-bind (name value budget unit detail) r
                       (format s "~68a ~7,1f ~2a ~:[        ~;~:*~5d ~2a~]~:[  MISSED~;~]~%" name value unit budget unit
                               (or (null budget) (<= value budget)))
                       (when detail (format s "    ~a~%" detail))))
                   (format s "~%~:[All budgets met.~;~:*~d budget~:p missed.~]~%" (and missed (length missed))))))
    (when *profile*
      (funcall (intern "STOP-PROFILING" "SB-SPROF"))
      (funcall (intern "REPORT" "SB-SPROF") :type :flat :max 40))
    (format t "~&~%~a" report)
    (ensure-directories-exist *out*)
    (with-open-file (o (merge-pathnames "latest.txt" *out*) :direction :output :if-exists :supersede)
      (write-string report o))
    (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if missed 1 0))))

;;; Run, with a fresh config directory so first-run questions are skipped.
(let ((config (merge-pathnames (format nil "cadre-perf-config-~d/" (get-universal-time))
                                (uiop:temporary-directory))))
  (ensure-directories-exist (merge-pathnames "cadre/" config))
  (with-open-file (o (merge-pathnames "cadre/settings.sexp" config) :direction :output)
    (prin1 '(:keybinding-profile :standard) o))
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config) 1)
  (sb-posix:setenv "XDG_STATE_HOME" (namestring (merge-pathnames "state/" config)) 1))

;; Catch the window's first frame.
(glib:timeout-add glib:+priority-default+ 1
                  (lambda ()
                    (if (and *window* (gtk:widget-get-realized (cadre-ui::window-gtk-window *window*)))
                        (progn (after-next-paint (cadre-ui::window-gtk-window *window*)
                                                 (lambda (start end)
                                                   (declare (ignore start))
                                                   (setf *startup-paint* end)
                                                   (pause 1500 (lambda ()
                                                                 (when *profile*
                                                                   (funcall (intern "START-PROFILING" "SB-SPROF")
                                                                            :max-samples 200000 :mode :cpu :sample-interval 0.001
                                                                            :threads :all))
                                                                 (run-steps *steps*)))))
                               nil)
                        t)))
(setf *startup-start* (now-ms))
(cadre-ui:main :project *files* :init-file nil :quit-after 300)
(format t "~&Timed out.~%")
(uiop:quit 1)
