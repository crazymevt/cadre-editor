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

(defparameter *panel-pages-shown-when-used* '("debugger" "inspector" "references" "history")
  "Pages whose tab appears only once they have something to show, so a
narrow panel (the vertical layout) has room for the others.")

(defun make-panel (&key on-hide)
  (let* ((panel (make-instance 'panel))
         (output-view (make-instance 'gtk:text-view :editable nil :monospace t
                                                    :cursor-visible nil :wrap-mode :word-char
                                                    :left-margin 8 :top-margin 4))
         (stack (make-instance 'gtk:stack :vexpand t :hexpand t))
         (switcher (make-instance 'gtk:stack-switcher :stack stack :halign :start
                                                      :css-classes '("cadre-panel-switcher")))
         ;; In a scroller, so a narrow panel scrolls its tabs instead of
         ;; forcing the panel wider.
         (tabs (make-instance 'gtk:scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :never
                                                   :propagate-natural-width t :hexpand t :child switcher)))
    (dolist (page '(("repl" "REPL") ("problems" "Problems") ("debugger" "Debugger")
                    ("inspector" "Inspector") ("references" "References") ("history" "History")
                    ("claude" "Claude")))
      (let ((holder (make-instance 'adw:bin :vexpand t)))
        (push (cons (first page) holder) (panel-holders panel))
        (gtk:stack-add-titled stack holder (first page) (second page))))
    (gtk:stack-add-titled stack (make-instance 'gtk:scrolled-window :child output-view)
                          "output" "Output")
    (gtk:stack-set-visible-child-name stack "output")
    (setup-tab-scrolling tabs switcher stack)
    ;; Tabs as wide as their labels, not all as wide as the widest.
    (gtk:box-layout-set-homogeneous (gtk:widget-get-layout-manager switcher) nil)
    ;; Pages with nothing to show yet stay out of the switcher (see PANEL-SHOW).
    (dolist (name *panel-pages-shown-when-used*)
      (gtk:stack-page-set-visible (gtk:stack-get-page stack (gtk:stack-get-child-by-name stack name)) nil))
    (setf (slot-value panel 'stack) stack
          (slot-value panel 'output) (gtk:text-view-get-buffer output-view)
          (slot-value panel 'widget)
          (gtk:build
            (gtk:box :orientation :vertical :css-classes '("cadre-panel")
              (gtk:box :spacing 6 :margin-start 6 :margin-end 6 :margin-top 4 :margin-bottom 4
                tabs
                (gtk:button :icon-name "window-close-symbolic" :tooltip-text "Hide the panel"
                            :css-classes '("flat")
                            :on-clicked (lambda (b) (declare (ignore b))
                                          (when on-hide (funcall on-hide)))))
              (gtk:separator)
              stack)))
    panel))

(defun setup-tab-scrolling (scroller switcher stack)
  "Let the mouse wheel scroll the tabs sideways, and keep the selected tab in view."
  (let ((adjustment (gtk:scrolled-window-get-hadjustment scroller))
        (wheel (gtk:event-controller-scroll-new '(:vertical :horizontal))))
    (gobject:connect wheel :scroll
                     (lambda (controller dx dy)
                       (declare (ignore controller))
                       (gtk:adjustment-set-value adjustment (+ (gtk:adjustment-get-value adjustment)
                                                               (* 30 (+ dx dy))))
                       t))
    (gtk:widget-add-controller scroller wheel)
    (gobject:connect stack "notify::visible-child"
                     (lambda (s pspec)
                       (declare (ignore s pspec))
                       (glib:idle-add glib:+priority-default-idle+
                                      (lambda ()
                                        (loop for button = (gtk:widget-get-first-child switcher)
                                                then (gtk:widget-get-next-sibling button)
                                              while button
                                              when (and (typep button 'gtk:toggle-button)
                                                        (gtk:toggle-button-get-active button))
                                                do (multiple-value-bind (ok x)
                                                       (gtk:widget-translate-coordinates button switcher 0d0 0d0)
                                                     (when ok
                                                       (gtk:adjustment-clamp-page
                                                        adjustment x (+ x (gtk:widget-get-width button)))))
                                                   (return))
                                        nil))))))

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

(defun panel-page (panel name)
  (gtk:stack-get-page (panel-stack panel) (gtk:stack-get-child-by-name (panel-stack panel) name)))

(defun panel-show (panel name)
  "Show page NAME, adding its tab if it was hidden."
  (gtk:stack-page-set-visible (panel-page panel name) t)
  (gtk:stack-set-visible-child-name (panel-stack panel) name))

(defun panel-hide-page (panel name)
  "Take page NAME's tab away until it is shown again."
  (when (string= (panel-visible-name panel) name)
    (gtk:stack-set-visible-child-name (panel-stack panel) "repl"))
  (gtk:stack-page-set-visible (panel-page panel name) nil))

(defun panel-visible-name (panel)
  (or (gtk:stack-get-visible-child-name (panel-stack panel)) ""))

(defun panel-set-title (panel name title)
  (let ((page (gtk:stack-get-page (panel-stack panel) (gtk:stack-get-child-by-name (panel-stack panel) name))))
    (gtk:stack-page-set-title page title)))

(defun panel-output-string (panel)
  (text-string (panel-output panel)))
