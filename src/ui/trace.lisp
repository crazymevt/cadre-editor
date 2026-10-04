;;;; trace.lisp — the Trace page: trace functions and see their calls
;;;;
;;;; Tracing uses Swank's trace dialog (swank-trace-dialog), which records
;;;; each call of a traced function with its arguments and values instead of
;;;; printing them. The page shows the calls as a tree, callers above the
;;;; calls they made; clicking an argument or a value inspects it, and
;;;; clicking a call with calls inside folds it. While the page is showing it
;;;; asks the Lisp for new calls every second, so a running program's calls
;;;; appear as they happen.

(in-package #:cadre-ui)

(defstruct (trace-page (:conc-name tp-))
  widget list title
  (tree (make-trace-tree))
  (collapsed (make-hash-table))
  (specs '())                           ; the traced functions' names
  (fetching nil)
  (timer nil))

(defvar *trace-page* nil)
(defvar *trace-rows* (make-hash-table :test 'eq) "Trace row → trace-call.")

(defparameter *trace-lines-shown* 1000
  "The most calls the Trace page shows; Clear makes room for new ones.")

(defun trace-dialog (name &rest arguments)
  "The form calling swank-trace-dialog's NAME with ARGUMENTS."
  (apply #'swank-call (format nil "swank-trace-dialog:~a" name) arguments))

(defun trace-spec-form (name)
  "The form that reads NAME, a function name as text, in the other Lisp."
  (list (remote-symbol "swank-trace-dialog:dialog-toggle-trace")
        (list (remote-symbol "swank::from-string") name)))

(defun make-trace-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (title (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t :css-classes '("heading")
                                          :label "Nothing traced"))
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :spacing 4 :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4
                       title
                       (gtk:button :icon-name "list-add-symbolic" :css-classes '("flat")
                                   :tooltip-text "Trace a function…"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'trace-function)))
                       (gtk:button :icon-name "view-refresh-symbolic" :css-classes '("flat")
                                   :tooltip-text "Fetch new calls"
                                   :on-clicked (lambda (b) (declare (ignore b)) (refresh-traces)))
                       (gtk:button :icon-name "edit-clear-all-symbolic" :css-classes '("flat")
                                   :tooltip-text "Forget the calls recorded so far"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'clear-traces)))
                       (gtk:button :label "Untrace All" :css-classes '("flat")
                                   :tooltip-text "Stop tracing every function"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'untrace-all-functions))))
                     (gtk:separator)
                     (gtk:scrolled-window :vexpand t :child list)))))
    (setf *trace-page* (make-trace-page :widget widget :list list :title title))
    (gtk:list-box-set-activate-on-single-click list t)
    (gobject:connect list :row-activated
                     (lambda (lb row)
                       (declare (ignore lb))
                       (let ((call (gethash row *trace-rows*)))
                         (when (and call (trace-call-children call))
                           (let ((collapsed (tp-collapsed *trace-page*)))
                             (if (gethash (trace-call-id call) collapsed)
                                 (remhash (trace-call-id call) collapsed)
                                 (setf (gethash (trace-call-id call) collapsed) t)))
                           (fill-trace-list *trace-page*)))))
    (fill-trace-list *trace-page*)
    widget))

;;; Showing the calls

(defun trace-part-button (call text part-id kind)
  "A link showing TEXT, an argument or value of CALL; clicking inspects it."
  (let ((button (gtk:build
                  (gtk:button :css-classes '("flat" "cadre-link" "cadre-trace-part")
                              :tooltip-text (format nil "Inspect ~a" text)
                    (gtk:label :label text :ellipsize :end :max-width-chars 40 :css-classes '("monospace"))))))
    (gobject:connect button :clicked
                     (lambda (b) (declare (ignore b))
                       (with-connection (connection)
                         (rex connection (trace-dialog "inspect-trace-part" (trace-call-id call) part-id kind)
                              :on-ok (lambda (reply) (show-inspection reply))))))
    button))

(defun trace-row (call depth collapsed)
  (let* ((box (make-instance 'gtk:box :margin-start (+ 6 (* 16 depth))))
         (row (make-instance 'gtk:list-box-row :child box)))
    (gtk:box-append box (label (cond ((null (trace-call-children call)) " ")
                                     (collapsed "▸")
                                     (t "▾"))
                               :width-chars 2 :css-classes '("dim-label")))
    (gtk:box-append box (label (format nil "(~a" (trace-call-name call)) :css-classes '("monospace" "cadre-trace-name")))
    (loop for arg in (trace-call-args call)
          for i from 0
          do (gtk:box-append box (trace-part-button call arg i :arg)))
    (gtk:box-append box (label ")" :css-classes '("monospace")))
    (ecase (trace-call-state call)
      (:running (gtk:box-append box (label "  running…" :css-classes '("dim-label"))))
      (:unwound (gtk:box-append box (label "  ⇏ exited non-locally" :css-classes '("dim-label"))))
      (:returned
       (gtk:box-append box (label "  ⇒" :css-classes '("dim-label")))
       (if (trace-call-results call)
           (loop for value in (trace-call-results call)
                 for i from 0
                 do (gtk:box-append box (trace-part-button call value i :retval)))
           (gtk:box-append box (label " no values" :css-classes '("dim-label"))))))
    (when (and collapsed (trace-call-children call))
      (gtk:box-append box (label (format nil "  (~d call~:p inside)" (length (trace-call-children call)))
                                 :css-classes '("dim-label"))))
    (gtk:widget-set-tooltip-text row (trace-call-text call))
    (setf (gethash row *trace-rows*) call)
    row))

(defun fill-trace-list (page)
  (let ((list (tp-list page))
        (tree (tp-tree page)))
    (clrhash *trace-rows*)
    (gtk:list-box-remove-all list)
    (gtk:label-set-text (tp-title page)
                        (if (tp-specs page)
                            (format nil "Tracing ~{~a~^, ~}" (tp-specs page))
                            "Nothing traced"))
    (let ((lines (trace-tree-lines tree :collapsed (tp-collapsed page) :limit *trace-lines-shown*)))
      (if (null lines)
          (gtk:list-box-append list (label (if (tp-specs page)
                                               "No calls yet: run something that calls a traced function"
                                               "Trace a function (Trace in its right-click menu, or the + above) to record its calls here")
                                           :margin-start 8 :wrap t :css-classes '("dim-label")))
          (dolist (line lines)
            (destructuring-bind (call depth) line
              (gtk:list-box-append list (trace-row call depth (gethash (trace-call-id call) (tp-collapsed page)))))))
      (when (>= (length lines) *trace-lines-shown*)
        (gtk:list-box-append list (label (format nil "Showing the first ~d calls of ~d. Clear to see newer ones."
                                                 *trace-lines-shown* (trace-tree-count tree))
                                         :margin-start 8 :css-classes '("dim-label")))))
    (when *window*
      (panel-set-title (window-panel *window*) "trace"
                       (if (plusp (trace-tree-count tree))
                           (format nil "Trace (~d)" (trace-tree-count tree))
                           "Trace")))))

;;; Asking the Lisp

(defun fetch-traces (page connection &optional (changed nil))
  "Ask for calls not fetched yet, a batch at a time, then show them."
  (setf (tp-fetching page) t)
  (rex connection (trace-dialog "report-partial-tree" (trace-tree-key (tp-tree page)))
       :on-ok (lambda (reply)
                (destructuring-bind (entries remaining &rest more) reply
                  (declare (ignore more))
                  (multiple-value-bind (new updated) (trace-tree-add (tp-tree page) entries)
                    (let ((changed (or changed (plusp new) (plusp updated))))
                      (if (and (plusp remaining) (connection-open-p connection))
                          (fetch-traces page connection changed)
                          (progn (setf (tp-fetching page) nil)
                                 (when changed (fill-trace-list page))))))))
       :on-abort (lambda (reason)
                   (setf (tp-fetching page) nil)
                   (message "Could not fetch traces: ~a" reason))))

(defun refresh-trace-specs (page connection &optional then)
  (rex connection (trace-dialog "report-specs")
       :on-ok (lambda (specs)
                (setf (tp-specs page) (mapcar #'trace-spec-name specs))
                (fill-trace-list page)
                (when then (funcall then)))))

(defun refresh-traces ()
  "Fetch the traced functions and new calls."
  (let ((page *trace-page*))
    (when (and page (connected-p) (not (tp-fetching page)))
      (let ((connection *connection*))
        (refresh-trace-specs page connection (lambda () (fetch-traces page connection)))))))

(defun trace-page-showing-p ()
  (and *window* (equal (panel-visible-name (window-panel *window*)) "trace")
       (gtk:widget-get-mapped (tp-widget *trace-page*))))

(defun start-trace-polling ()
  "While the Trace page shows and something is traced, fetch new calls every second."
  (let ((page *trace-page*))
    (unless (tp-timer page)
      (setf (tp-timer page)
            (glib:timeout-add glib:+priority-default+ 1000
                              (lambda ()
                                (cond ((and (trace-page-showing-p) (tp-specs page) (connected-p))
                                       (unless (tp-fetching page) (fetch-traces page *connection*))
                                       t)
                                      (t (setf (tp-timer page) nil) nil))))))))

(defun show-trace-page ()
  (panel-show (window-panel *window*) "trace")
  (set-panel-visible *window* t)
  (refresh-traces)
  (start-trace-polling))

(defun trace-page-shown ()
  "The Trace tab was chosen."
  (refresh-traces)
  (start-trace-polling))

(defun traces-disconnected ()
  (when *trace-page*
    (setf (tp-specs *trace-page*) '() (tp-fetching *trace-page*) nil)
    (fill-trace-list *trace-page*)))

;;; Commands

(defun toggle-trace-of (name package)
  (with-connection (connection)
    (rex connection (trace-spec-form name) :package package
         :on-ok (lambda (reply)
                  (message "~a" (substitute #\Space #\Newline (string-trim '(#\Space #\Newline) (princ-to-string reply))))
                  (refresh-trace-specs *trace-page* connection
                                       (lambda ()
                                         (when (tp-specs *trace-page*) (show-trace-page))))))))

(define-command toggle-trace ()
  "Trace the function at the cursor, recording its calls on the Trace page
(or stop tracing it)."
  (call-with-symbol-name "Trace or untrace the function" #'toggle-trace-of))

(define-command trace-function ()
  "Trace or untrace a function by name, such as foo, (setf foo) or (method bar (string))."
  (let ((view (current-view)))
    (open-picker (window-picker *window*)
                 :placeholder "Trace or untrace the function named"
                 :on-choose (lambda (text)
                              (unless (string= (string-trim " " text) "")
                                (toggle-trace-of (string-trim " " text)
                                                 (if (and view (eq (buffer-major-mode (view-buffer view)) 'lisp-mode))
                                                     (view-package view)
                                                     (and (connected-p) (connection-package *connection*)))))))))

(define-command show-traces ()
  "Show the Trace page: the calls of the traced functions."
  (show-trace-page))

(define-command clear-traces ()
  "Forget the calls recorded so far (the functions stay traced)."
  (with-connection (connection)
    (rex connection (trace-dialog "clear-trace-tree")
         :on-ok (lambda (reply)
                  (declare (ignore reply))
                  (let ((page *trace-page*))
                    (setf (tp-tree page) (make-trace-tree :key (1+ (trace-tree-key (tp-tree page)))))
                    (clrhash (tp-collapsed page))
                    (fill-trace-list page)
                    (message "Cleared the recorded calls"))))))

(define-command untrace-all-functions ()
  "Stop tracing every traced function."
  (with-connection (connection)
    (rex connection (trace-dialog "dialog-untrace-all")
         :on-ok (lambda (reply)
                  (message "Untraced ~:[nothing~;~:*~d function~:p~]" (and (consp reply) (length reply)))
                  (refresh-trace-specs *trace-page* connection)))))
