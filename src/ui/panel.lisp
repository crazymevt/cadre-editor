;;;; panel.lisp — the panel: REPL, Problems and Output, below or beside the editor
;;;;
;;;; M0 has the Output page (a log of messages); the REPL and Problems pages
;;;; are placeholders until M2.

(in-package #:cadre-ui)

(defclass panel ()
  ((widget :reader panel-widget)
   (stack :reader panel-stack)
   (output :reader panel-output :documentation "The Output page's text buffer.")))

(defun placeholder-page (icon title description)
  (make-instance 'adw:status-page :icon-name icon :title title :description description
                                  :css-classes '("compact")))

(defun make-panel (&key on-hide)
  (let* ((panel (make-instance 'panel))
         (output-view (make-instance 'gtk:text-view :editable nil :monospace t
                                                    :cursor-visible nil :wrap-mode :word-char
                                                    :left-margin 8 :top-margin 4))
         (stack (make-instance 'gtk:stack :vexpand t :hexpand t)))
    (gtk:stack-add-titled stack (placeholder-page "cadre-terminal-symbolic" "REPL"
                                                  "Connecting to a Lisp arrives in M2.")
                          "repl" "REPL")
    (gtk:stack-add-titled stack (placeholder-page "dialog-warning-symbolic" "No problems"
                                                  "Compiler notes appear here once Cadre can compile (M2).")
                          "problems" "Problems")
    (gtk:stack-add-titled stack (make-instance 'gtk:scrolled-window :child output-view)
                          "output" "Output")
    (gtk:stack-set-visible-child-name stack "output")
    (setf (slot-value panel 'stack) stack
          (slot-value panel 'output) (gtk:text-view-get-buffer output-view)
          (slot-value panel 'widget)
          (gtk:build
            (gtk:box :orientation :vertical :css-classes '("cadre-panel")
              (gtk:box :spacing 6 :margin-start 6 :margin-end 6 :margin-top 4 :margin-bottom 4
                (gtk:stack-switcher :stack stack)
                (gtk:box :hexpand t)
                (gtk:button :icon-name "window-close-symbolic" :tooltip-text "Hide the panel"
                            :css-classes '("flat")
                            :on-clicked (lambda (b) (declare (ignore b))
                                          (when on-hide (funcall on-hide)))))
              (gtk:separator)
              stack)))
    panel))

(defun panel-log (panel string)
  "Add STRING as a line at the end of PANEL's Output page."
  (let* ((buffer (panel-output panel))
         (time (multiple-value-bind (s m h) (get-decoded-time) (format nil "~2,'0d:~2,'0d:~2,'0d" h m s))))
    (gtk:text-buffer-insert buffer (gtk:text-buffer-get-end-iter buffer)
                            (format nil "[~a] ~a~%" time string) -1)))

(defun panel-output-string (panel)
  (text-string (panel-output panel)))
