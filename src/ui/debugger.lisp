;;;; debugger.lisp — the Debugger page: condition, restarts, backtrace
;;;;
;;;; When an evaluation signals an error, the Lisp's thread waits in the
;;;; debugger and sends :debug. The page shows the condition, the restarts
;;;; (click one, or press its number) and the backtrace. Frame locals,
;;;; evaluating in a frame and the inspector come in M3.

(in-package #:cadre-ui)

(defstruct (debug-level (:conc-name dl-))
  connection thread level condition restarts frames)

(defvar *debug-levels* '() "Debugger levels waiting, innermost first.")
(defvar *debugger-box* nil)

(defun make-debugger-widget ()
  (setf *debugger-box* (make-instance 'gtk:box :orientation :vertical :spacing 6
                                               :margin-start 12 :margin-end 12 :margin-top 8 :margin-bottom 8))
  (debugger-render)
  (make-instance 'gtk:scrolled-window :child *debugger-box* :vexpand t))

(defun clear-box (box)
  (loop for child = (gtk:widget-get-first-child box)
        while child do (gtk:box-remove box child)))

(defun debugger-render ()
  (when *debugger-box*
    (clear-box *debugger-box*)
    (let ((level (first *debug-levels*)))
      (if (null level)
          (gtk:box-append *debugger-box*
                          (make-instance 'adw:status-page :icon-name "dialog-information-symbolic"
                                                          :title "No errors"
                                                          :description "When an evaluation signals an error, it is shown here."
                                                          :css-classes '("compact")))
          (destructuring-bind (text type &rest more) (dl-condition level)
            (declare (ignore more))
            (gtk:box-append *debugger-box*
                            (make-instance 'gtk:label :label text :xalign 0.0 :wrap t :selectable t
                                                      :css-classes '("title-4")))
            (gtk:box-append *debugger-box*
                            (make-instance 'gtk:label :label (format nil "~a — level ~d, thread ~a"
                                                                     (string-trim " " type) (dl-level level) (dl-thread level))
                                                      :xalign 0.0 :css-classes '("dim-label")))
            (gtk:box-append *debugger-box* (make-instance 'gtk:label :label "Restarts" :xalign 0.0
                                                                     :margin-top 6 :css-classes '("heading")))
            (loop for (name description) in (dl-restarts level)
                  for n from 0
                  do (let ((n n))
                       (gtk:box-append *debugger-box*
                                       (gtk:build
                                         (gtk:button :css-classes '("flat")
                                                     :on-clicked (lambda (b) (declare (ignore b)) (invoke-restart-number n))
                                           (gtk:label :label (format nil "~d: [~a] ~a" n name description)
                                                      :xalign 0.0 :wrap t))))))
            (gtk:box-append *debugger-box* (make-instance 'gtk:label :label "Backtrace" :xalign 0.0
                                                                     :margin-top 6 :css-classes '("heading")))
            (gtk:box-append *debugger-box*
                            (make-instance 'gtk:label
                                           :label (format nil "~{~a~^~%~}"
                                                          (mapcar (lambda (frame) (format nil "~3d: ~a" (first frame) (second frame)))
                                                                  (dl-frames level)))
                                           :xalign 0.0 :selectable t :wrap t :css-classes '("monospace"))))))))

(defun debugger-enter (connection thread level condition restarts frames &rest more)
  (declare (ignore more))
  (setf *debug-levels* (cons (make-debug-level :connection connection :thread thread :level level
                                               :condition condition :restarts restarts :frames frames)
                             (remove-if (lambda (d) (and (equal (dl-thread d) thread) (>= (dl-level d) level)))
                                        *debug-levels*)))
  (debugger-render)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "debugger")
  (panel-set-title (window-panel *window*) "debugger" "Debugger ●")
  (message "Error: ~a" (first condition)))

(defun debugger-return (thread level)
  (setf *debug-levels* (remove-if (lambda (d) (and (equal (dl-thread d) thread) (>= (dl-level d) level)))
                                  *debug-levels*))
  (debugger-render)
  (unless *debug-levels*
    (panel-set-title (window-panel *window*) "debugger" "Debugger")
    (when (string= (panel-visible-name (window-panel *window*)) "debugger")
      (panel-show (window-panel *window*) "repl"))))

(defun debugger-clear ()
  (setf *debug-levels* '())
  (debugger-render)
  (when *window* (panel-set-title (window-panel *window*) "debugger" "Debugger")))

(defun invoke-restart-number (n)
  (let ((level (or (first *debug-levels*) (editor-error "Not in the debugger"))))
    (when (>= n (length (dl-restarts level))) (editor-error "No restart ~d" n))
    (rex (dl-connection level)
         (swank-call "swank:invoke-nth-restart-for-emacs" (dl-level level) n)
         :thread (dl-thread level)
         :on-abort (lambda (reason) (declare (ignore reason))))))

(define-command debugger-abort ()
  "Leave the debugger with the restart that returns to the top level."
  (let* ((level (or (first *debug-levels*) (editor-error "Not in the debugger")))
         (abort (or (position-if (lambda (r) (search "ABORT" (first r))) (dl-restarts level))
                    (1- (length (dl-restarts level))))))
    (invoke-restart-number abort)))
