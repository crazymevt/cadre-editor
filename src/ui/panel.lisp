;;;; panel.lisp — the panel, below or beside the editor
;;;;
;;;; Pages: the REPL, Problems (compiler notes), the Debugger, the Inspector,
;;;; References (cross-references), Claude (a chat) and Output (a log of messages).

(in-package #:cadre-ui)

(defclass panel ()
  ((widget :reader panel-widget)
   (stack :reader panel-stack)
   (holders :initform '() :accessor panel-holders :documentation "Page name → adw:bin holding its content.")
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
    (dolist (page '(("repl" "REPL") ("problems" "Problems") ("debugger" "Debugger")
                    ("inspector" "Inspector") ("references" "References") ("claude" "Claude")))
      (let ((holder (make-instance 'adw:bin :vexpand t)))
        (push (cons (first page) holder) (panel-holders panel))
        (gtk:stack-add-titled stack holder (first page) (second page))))
    (gtk:stack-add-titled stack (make-instance 'gtk:scrolled-window :child output-view)
                          "output" "Output")
    (gtk:stack-set-visible-child-name stack "output")
    (setf (slot-value panel 'stack) stack
          (slot-value panel 'output) (gtk:text-view-get-buffer output-view)
          (slot-value panel 'widget)
          (gtk:build
            (gtk:box :orientation :vertical :css-classes '("cadre-panel")
              (gtk:box :spacing 6 :margin-start 6 :margin-end 6 :margin-top 4 :margin-bottom 4
                ;; In a scroller, so a narrow panel scrolls its tabs instead
                ;; of forcing the panel wider.
                (gtk:scrolled-window :hscrollbar-policy :external :vscrollbar-policy :never
                                     :propagate-natural-width t :hexpand t
                  (gtk:stack-switcher :stack stack :halign :start))
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

(defun panel-log-raw (panel string)
  "Add STRING, a line of the Lisp's own output, to the Output page."
  (let ((buffer (panel-output panel)))
    (gtk:text-buffer-insert buffer (gtk:text-buffer-get-end-iter buffer) (format nil "~a~%" string) -1)))

(defun panel-set-page-child (panel name widget)
  (adw:bin-set-child (cdr (assoc name (panel-holders panel) :test #'string=)) widget))

(defun panel-show (panel name)
  (gtk:stack-set-visible-child-name (panel-stack panel) name))

(defun panel-visible-name (panel)
  (or (gtk:stack-get-visible-child-name (panel-stack panel)) ""))

(defun panel-set-title (panel name title)
  (let ((page (gtk:stack-get-page (panel-stack panel) (gtk:stack-get-child-by-name (panel-stack panel) name))))
    (gtk:stack-page-set-title page title)))

(defun panel-output-string (panel)
  (text-string (panel-output panel)))
