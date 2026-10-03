;;;; debugger.lisp — the Debugger page, as SLDB
;;;;
;;;; When an evaluation signals an error, the Lisp's thread waits in the
;;;; debugger and sends :debug. The page shows the condition, the restarts
;;;; (click one, or press its number; a aborts, c continues) and the
;;;; backtrace. Opening a frame shows its locals (click a value to inspect
;;;; it) and lets you evaluate in the frame, show its source, restart it or
;;;; return from it. Nested errors stack up as levels; the innermost shows.

(in-package #:cadre-ui)

(defstruct (debug-level (:conc-name dl-))
  connection thread level condition restarts
  (frames '())                          ; frame structures fetched so far
  (complete nil))                       ; true once every frame is fetched

(defvar *debug-levels* '() "Debugger levels waiting, innermost first.")
(defvar *debugger-box* nil)

(defparameter *frames-per-fetch* 40)

(defun make-debugger-widget ()
  (setf *debugger-box* (make-instance 'gtk:box :orientation :vertical :spacing 6
                                               :margin-start 12 :margin-end 12 :margin-top 8 :margin-bottom 8))
  (let ((scroller (make-instance 'gtk:scrolled-window :child *debugger-box* :vexpand t))
        (keys (gtk:event-controller-key-new)))
    (gobject:connect keys :key-pressed
                     (lambda (c keyval keycode state)
                       (declare (ignore c keycode))
                       (debugger-key keyval state)))
    (gtk:widget-add-controller scroller keys)
    (debugger-render)
    scroller))

(defun debugger-key (keyval state)
  "Digits choose a restart, a aborts, c continues; only without modifiers."
  (let ((char (gdk:keyval-to-unicode keyval)))
    (when (and *debug-levels* (null (intersection (modifier-list state) '(:control-mask :alt-mask :super-mask :meta-mask)))
               (plusp char))
      (let ((c (code-char char)))
        (handler-case
            (cond ((digit-char-p c) (invoke-restart-number (digit-char-p c)) t)
                  ((char= c #\a) (debugger-abort) t)
                  ((char= c #\c) (debugger-continue) t))
          (editor-error (e) (message "~a" (editor-error-message e)) t))))))

(defun clear-box (box)
  (loop for child = (gtk:widget-get-first-child box)
        while child do (gtk:box-remove box child)))

(defun label (text &rest options)
  (apply #'make-instance 'gtk:label :label text :xalign 0.0 options))

(defun restart-position (level name)
  (position-if (lambda (r) (string-equal name (string-trim "*" (first r)))) (dl-restarts level)))

(defun debugger-render ()
  (when *debugger-box*
    (clear-box *debugger-box*)
    (let ((level (first *debug-levels*)))
      (if (null level)
          (gtk:box-append *debugger-box*
                          (make-instance 'adw:status-page :icon-name "cadre-bug-symbolic"
                                                          :title "No errors"
                                                          :description "When an evaluation signals an error, it is shown here."
                                                          :css-classes '("compact")))
          (render-level level)))))

(defun render-level (level)
  (destructuring-bind (text type &rest more) (dl-condition level)
    (declare (ignore more))
    (gtk:box-append *debugger-box* (label text :wrap t :selectable t :css-classes '("title-4")))
    (gtk:box-append *debugger-box*
                    (label (format nil "~a — level ~d, thread ~a~@[ (~d more waiting)~]"
                                   (string-trim " " type) (dl-level level) (dl-thread level)
                                   (and (rest *debug-levels*) (length (rest *debug-levels*))))
                           :css-classes '("dim-label")))
    (gtk:box-append *debugger-box*
                    (gtk:build
                      (gtk:box :spacing 6 :margin-top 4
                        (gtk:button :label "_Continue" :use-underline t
                                    :sensitive (and (restart-position level "CONTINUE") t)
                                    :tooltip-text "Invoke the CONTINUE restart (c)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'debugger-continue)))
                        (gtk:button :label "_Abort" :use-underline t :css-classes '("destructive-action")
                                    :tooltip-text "Return to the top level (a)"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'debugger-abort)))
                        (gtk:button :label "Inspect Condition"
                                    :on-clicked (lambda (b) (declare (ignore b)) (inspect-condition level)))
                        (gtk:button :label "Ask Claude" :tooltip-text "Send the error and backtrace to Claude"
                                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'ask-claude-about-error))))))
    (gtk:box-append *debugger-box* (label "Restarts" :margin-top 6 :css-classes '("heading")))
    (loop for (name description) in (dl-restarts level)
          for n from 0
          do (let ((n n))
               (gtk:box-append *debugger-box*
                               (gtk:build
                                 (gtk:button :css-classes '("flat")
                                             :on-clicked (lambda (b) (declare (ignore b)) (invoke-restart-number n))
                                   (gtk:label :label (format nil "~d: [~a] ~a" n name description)
                                              :xalign 0.0 :wrap t))))))
    (gtk:box-append *debugger-box* (label "Backtrace" :margin-top 6 :css-classes '("heading")))
    (let ((frames (make-instance 'gtk:box :orientation :vertical :spacing 2)))
      (dolist (frame (dl-frames level))
        (gtk:box-append frames (frame-widget level frame)))
      (gtk:box-append *debugger-box* frames)
      (unless (dl-complete level)
        (let ((more (make-instance 'gtk:button :label "More Frames" :halign :start :css-classes '("flat"))))
          (gobject:connect more :clicked (lambda (b) (declare (ignore b)) (fetch-more-frames level frames more)))
          (gtk:box-append *debugger-box* more))))))

(defun frame-widget (level frame)
  "An expander for FRAME; opening it fetches the frame's locals."
  (let* ((title (label (format nil "~3d: ~a" (frame-number frame) (frame-description frame))
                       :ellipsize :end :css-classes '("monospace")))
         (expander (make-instance 'gtk:expander :label-widget title)))
    (gtk:widget-set-tooltip-text title (frame-description frame))
    (gobject:connect expander "notify::expanded"
                     (lambda (e pspec)
                       (declare (ignore pspec))
                       (when (and (gtk:expander-get-expanded e) (null (gtk:expander-get-child e)))
                         (gtk:expander-set-child e (frame-details level frame)))))
    expander))

(defun frame-rex (level form &key on-ok)
  "Send FORM in LEVEL's thread, if LEVEL is still waiting."
  (unless (member level *debug-levels*) (editor-error "That debugger level has returned"))
  (rex (dl-connection level) form :thread (dl-thread level) :on-ok on-ok))

(defun frame-details (level frame)
  (let* ((n (frame-number frame))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 4 :margin-start 28 :margin-bottom 6))
         (locals (make-instance 'gtk:grid :column-spacing 12 :row-spacing 2))
         (result (label "" :wrap t :selectable t :visible nil :css-classes '("monospace" "cadre-inline-result")))
         (entry (make-instance 'gtk:entry :placeholder-text "Evaluate in this frame" :hexpand t
                                          :css-classes '("monospace"))))
    (gtk:box-append box locals)
    (gtk:box-append box
                    (gtk:build
                      (gtk:box :spacing 6
                        (gtk:button :label "Source" :tooltip-text "Show the frame's source"
                                    :on-clicked (lambda (b) (declare (ignore b)) (show-frame-source level n)))
                        (gtk:button :label "Restart" :sensitive (frame-restartable frame)
                                    :tooltip-text "Restart the frame (call its function again)"
                                    :on-clicked (lambda (b) (declare (ignore b))
                                                  (frame-rex level (swank-call "swank::restart-frame" n))))
                        (gtk:button :label "Return…" :tooltip-text "Return a value from the frame"
                                    :on-clicked (lambda (b) (declare (ignore b)) (return-from-frame level n)))
                        (gtk:button :label "Disassemble"
                                    :on-clicked (lambda (b) (declare (ignore b))
                                                  (frame-rex level (swank-call "swank:sldb-disassemble" n)
                                                             :on-ok (lambda (text)
                                                                      (show-help (format nil "Frame ~d" n) text))))))))
    (gtk:box-append box entry)
    (gtk:box-append box result)
    (gobject:connect entry :activate
                     (lambda (e)
                       (let ((string (gtk:editable-get-text e)))
                         (unless (string= (string-trim " " string) "")
                           (frame-rex level (swank-call "swank:eval-string-in-frame" string n
                                                        (connection-package (dl-connection level)) 3 200)
                                      :on-ok (lambda (value)
                                               (gtk:label-set-text result (format nil "⇒ ~a" (result-text value)))
                                               (gtk:widget-set-visible result t)))))))
    (gtk:grid-attach locals (label "Fetching locals…" :css-classes '("dim-label")) 0 0 2 1)
    (frame-rex level (swank-call "swank:frame-locals-and-catch-tags" n)
               :on-ok (lambda (reply) (fill-locals level n locals reply)))
    box))

(defun fill-locals (level n grid reply)
  (loop for child = (gtk:widget-get-first-child grid) while child do (gtk:grid-remove grid child))
  (multiple-value-bind (locals tags) (parse-frame-locals reply)
    (when (and (null locals) (null tags))
      (gtk:grid-attach grid (label "No locals" :css-classes '("dim-label")) 0 0 2 1))
    (loop for (name value) in locals
          for i from 0
          do (let ((i i)
                   (button (gtk:build
                             (gtk:button :css-classes '("flat" "cadre-link") :halign :start
                                         :tooltip-text "Inspect"
                               (gtk:label :label value :xalign 0.0 :ellipsize :end :max-width-chars 100
                                          :css-classes '("monospace"))))))
               (gobject:connect button :clicked
                                (lambda (b) (declare (ignore b))
                                  (frame-rex level (swank-call "swank:inspect-frame-var" n i)
                                             :on-ok (lambda (reply) (show-inspection reply :thread (dl-thread level))))))
               (gtk:grid-attach grid (label name :css-classes '("monospace" "dim-label")) 0 i 1 1)
               (gtk:grid-attach grid button 1 i 1 1)))
    (when tags
      (gtk:grid-attach grid (label (format nil "Catch tags: ~{~a~^, ~}" tags) :css-classes '("dim-label"))
                       0 (length locals) 2 1))))

(defun fetch-more-frames (level frames-box button)
  (let ((start (length (dl-frames level))))
    (frame-rex level (swank-call "swank:backtrace" start (+ start *frames-per-fetch*))
               :on-ok (lambda (reply)
                        (let ((new (parse-frames reply)))
                          (setf (dl-frames level) (append (dl-frames level) new))
                          (dolist (frame new) (gtk:box-append frames-box (frame-widget level frame)))
                          (when (< (length new) *frames-per-fetch*)
                            (setf (dl-complete level) t)
                            (gtk:widget-set-visible button nil)))))))

(defun show-frame-source (level n)
  (frame-rex level (swank-call "swank::frame-source-location" n)
             :on-ok (lambda (location) (goto-location (parse-location location)))))

(defun return-from-frame (level n)
  (open-picker (window-picker *window*)
               :placeholder (format nil "Return from frame ~d with the value of" n) :text "nil"
               :on-choose (lambda (text)
                            (frame-rex level (swank-call "swank:sldb-return-from-frame" n text)
                                       :on-ok (lambda (reply) (when (stringp reply) (message "~a" reply)))))))

(defun inspect-condition (level)
  (frame-rex level (swank-call "swank:inspect-current-condition")
             :on-ok (lambda (reply) (show-inspection reply :thread (dl-thread level)))))

;;; Entering and leaving

(defun debugger-enter (connection thread level condition restarts frames &rest more)
  (declare (ignore more))
  (let ((new (make-debug-level :connection connection :thread thread :level level
                               :condition condition :restarts restarts
                               :frames (parse-frames frames))))
    ;; Swank sends the first frames; a short list is the whole backtrace.
    (setf (dl-complete new) (< (length frames) 20))
    (setf *debug-levels* (cons new (remove-if (lambda (d) (and (equal (dl-thread d) thread) (>= (dl-level d) level)))
                                              *debug-levels*))))
  (debugger-render)
  (set-panel-visible *window* t)
  (panel-show (window-panel *window*) "debugger")
  (panel-set-title (window-panel *window*) "debugger" "Debugger ●")
  (focus-debugger)
  (message "Error: ~a" (first condition)))

(defun focus-debugger ()
  "Give the first restart the focus, so digits choose restarts."
  (when *debugger-box*
    (loop for child = (gtk:widget-get-first-child *debugger-box*) then (gtk:widget-get-next-sibling child)
          while child
          when (and (typep child 'gtk:button) (member "flat" (gtk:widget-get-css-classes child) :test #'string=))
            do (gtk:widget-grab-focus child) (return))))

(defun debugger-return (thread level)
  (setf *debug-levels* (remove-if (lambda (d) (and (equal (dl-thread d) thread) (>= (dl-level d) level)))
                                  *debug-levels*))
  (debugger-render)
  (image-changed)
  (unless *debug-levels*
    (panel-set-title (window-panel *window*) "debugger" "Debugger")
    (panel-hide-page (window-panel *window*) "debugger")))

(defun debugger-clear ()
  (setf *debug-levels* '())
  (debugger-render)
  (when *window*
    (panel-set-title (window-panel *window*) "debugger" "Debugger")
    (panel-hide-page (window-panel *window*) "debugger")))

(defun invoke-restart-number (n)
  (let ((level (or (first *debug-levels*) (editor-error "Not in the debugger"))))
    (when (>= n (length (dl-restarts level))) (editor-error "No restart ~d" n))
    (rex (dl-connection level)
         (swank-call "swank:invoke-nth-restart-for-emacs" (dl-level level) n)
         :thread (dl-thread level)
         :on-abort (lambda (reason) (declare (ignore reason))))))

(define-command debugger-abort ()
  "Leave the debugger with the restart that returns to the top level."
  (let* ((level (or (first *debug-levels*) (editor-error "Not in the debugger"))))
    (invoke-restart-number (or (restart-position level "ABORT") (1- (length (dl-restarts level)))))))

(define-command debugger-continue ()
  "Leave the debugger with the CONTINUE restart."
  (let* ((level (or (first *debug-levels*) (editor-error "Not in the debugger"))))
    (invoke-restart-number (or (restart-position level "CONTINUE") (editor-error "There is no CONTINUE restart")))))
