;;;; completion.lisp — the completion popup at the cursor
;;;;
;;;; A popover under the cursor lists the completions, with the call or
;;;; kind of the chosen one below. Typing keeps going to the editor and
;;;; narrows the list; Up and Down choose, Return or Tab insert (Return
;;;; just ends the line if what's typed is already the choice), Escape
;;;; closes. The completions come from hints.lisp.

(in-package #:cadre-ui)

(defstruct (completion-popup (:conc-name cp-))
  popover list-view store selection view start items detail
  qualifier)                            ; the package part (gtk:) ITEMS were found for

(defvar *completion* nil "The open completion popup, or nil.")
(defvar *typed-key* nil
  "(buffer . text) while a key typed into an editor is going in, so the
change it makes can start completion.")

(defun completion-open-p () (and *completion* (gtk:widget-get-visible (cp-popover *completion*))))

(defun close-completion ()
  (when *completion*
    (let ((popover (cp-popover *completion*)))
      (setf *completion* nil)
      (gtk:popover-popdown popover)
      (gtk:widget-unparent popover))))

(defun cursor-rectangle (view)
  (let* ((text-view (view-text-view view))
         (rect (gtk:text-view-get-iter-location text-view (cursor-iter (view-gtk-buffer view)))))
    (multiple-value-bind (x y)
        (gtk:text-view-buffer-to-window-coords text-view :widget (gdk:rectangle-x rect) (gdk:rectangle-y rect))
      (gdk:make-rectangle :x x :y y :width 1 :height (gdk:rectangle-height rect)))))

(defun fill-completions (popup items)
  (gio:list-store-remove-all (cp-store popup))
  (dolist (item items) (gio:list-store-append (cp-store popup) (gobject:make-lisp-object item)))
  (when items (gtk:single-selection-set-selected (cp-selection popup) 0))
  (update-completion-detail)
  ;; GTK can hide the popover while its size changes (as when the window
  ;; isn't active); with new items, it shows again.
  (when (and items (not (gtk:widget-get-visible (cp-popover popup))))
    (gtk:popover-popup (cp-popover popup))))

(defun selected-completion ()
  (let* ((popup *completion*)
         (position (and popup (gtk:single-selection-get-selected (cp-selection popup)))))
    (when (and position (/= position gtk:+invalid-list-position+))
      (gobject:lisp-object-value (gio:list-model-get-item (cp-store popup) position)))))

(defun update-completion-detail ()
  "Show the call or kind of the chosen completion below the list."
  (let ((popup *completion*))
    (when (and popup (cp-detail popup))
      (let* ((completion (selected-completion))
             (signature (and completion (completion-signature (first completion) (view-buffer (cp-view popup))))))
        (gtk:widget-set-visible (cp-detail popup) (and signature t))
        (when signature
          (gtk:label-set-markup (cp-detail popup) (signature-markup signature)))))))

(defun show-completions (view start completions)
  (close-completion)
  (let* ((store (gio:make-list-store))
         (selection (gtk:single-selection-new store))
         (list-view (gtk:list-view-new
                     selection
                     (gtk:make-factory
                      :setup (lambda ()
                               (gtk:build
                                 (gtk:box :spacing 16 :margin-start 4 :margin-end 4
                                   (gtk:label :xalign 0.0 :hexpand t :css-classes '("monospace"))
                                   (gtk:label :xalign 1.0 :css-classes '("dim-label" "caption")))))
                      :bind (lambda (row completion)
                              (let ((name (gtk:widget-get-first-child row)))
                                (gtk:label-set-text name (first completion))
                                (gtk:label-set-text (gtk:widget-get-next-sibling name)
                                                    (completion-kind (fourth completion))))))))
         (detail (make-instance 'gtk:label :xalign 0.0 :wrap t :max-width-chars 48 :visible nil
                                           :margin-start 6 :margin-end 6 :margin-top 4 :margin-bottom 2
                                           :css-classes '("caption" "monospace")))
         (popover (make-instance 'gtk:popover :autohide nil :has-arrow nil :position :bottom
                                              :can-focus nil
                                              :child (gtk:build
                                                       (gtk:box :orientation :vertical
                                                         (make-instance 'gtk:scrolled-window
                                                                        :child list-view :hscrollbar-policy :never
                                                                        :min-content-width 320
                                                                        :max-content-height 260
                                                                        :propagate-natural-height t)
                                                         detail)))))
    (gtk:widget-set-can-focus list-view nil)
    (gtk:widget-set-parent popover (view-text-view view))
    (gtk:popover-set-pointing-to popover (cursor-rectangle view))
    (setf *completion* (make-completion-popup :popover popover :list-view list-view :store store
                                              :selection selection :view view :start start
                                              :items completions :detail detail
                                              :qualifier (prefix-qualifier (prefix-before-cursor view))))
    (gobject:connect selection "notify::selected" (lambda (&rest args) (declare (ignore args))
                                                    (update-completion-detail)))
    ;; GTK may hide the popover itself while resizing it; if this popup is
    ;; still wanted, show it again.
    (gobject:connect popover :closed
                     (lambda (p)
                       (glib:idle-add glib:+priority-default-idle+
                                      (lambda ()
                                        (let ((popup *completion*))
                                          (when (and popup (eq (cp-popover popup) p)
                                                     (not (gtk:widget-get-visible p))
                                                     (plusp (gio:list-model-get-n-items (cp-store popup)))
                                                     (gtk:window-is-active (window-gtk-window *window*)))
                                            (gtk:popover-popup p)))
                                        nil))))
    (gobject:connect list-view :activate (lambda (lv position) (declare (ignore lv))
                                           (gtk:single-selection-set-selected selection position)
                                           (accept-completion)))
    (fill-completions *completion* completions)
    (gtk:popover-popup popover)))

(defun move-completion (delta)
  (let* ((selection (cp-selection *completion*))
         (n (gio:list-model-get-n-items (cp-store *completion*)))
         (current (gtk:single-selection-get-selected selection)))
    (when (plusp n)
      (let ((new (mod (+ (if (= current gtk:+invalid-list-position+) 0 current) delta) n)))
        (gtk:single-selection-set-selected selection new)
        (gtk:list-view-scroll-to (cp-list-view *completion*) new '(:none) nil)))))

(defun accept-completion ()
  (let ((popup *completion*)
        (completion (selected-completion)))
    (when completion
      (close-completion)
      (replace-prefix (cp-view popup) (cp-start popup) (first completion)))))

(defun completion-typed-p ()
  "Whether what's typed is already the chosen completion."
  (let ((completion (selected-completion)))
    (and completion (string= (prefix-before-cursor (cp-view *completion*)) (first completion)))))

(defun completion-key (keyval &optional state)
  "Handle KEYVAL if the completion popup wants it. Returns t if used. A
chord (⌘↩, C-s …) closes the popup and goes on to its command."
  (when (completion-open-p)
    (let ((name (gdk:keyval-name keyval)))
      (cond ((and (intersection (modifier-list state) '(:control-mask :alt-mask :super-mask :meta-mask))
                  (not (member name *modifier-key-names* :test #'string=)))
             (close-completion) nil)
            ((string= name "Down") (move-completion 1) t)
            ((string= name "Up") (move-completion -1) t)
            ((string= name "Page_Down") (move-completion 8) t)
            ((string= name "Page_Up") (move-completion -8) t)
            ((and (member name '("Return" "KP_Enter") :test #'string=) (completion-typed-p))
             (close-completion) nil)
            ((member name '("Return" "KP_Enter" "Tab") :test #'string=) (accept-completion) t)
            ((string= name "Escape") (close-completion) t)
            (t nil)))))

(defun completion-text-changed (view)
  "After an edit in VIEW: narrow the popup's list, or close it."
  (when (and *completion* (eq view (cp-view *completion*)))
    (multiple-value-bind (prefix start) (prefix-before-cursor view)
      (cond ((or (/= start (cp-start *completion*)) (string= prefix ""))
             (close-completion))
            ;; A package prefix typed or changed (gt → gtk:): the list was
            ;; for other symbols; find them again.
            ((not (same-qualifier-p (prefix-qualifier prefix) (cp-qualifier *completion*)))
             (close-completion)
             (when (auto-complete-p view) (start-completion view :auto t)))
            (t
             (let ((items (rank-completions prefix (cp-items *completion*))))
               (cond (items
                      (fill-completions *completion* items)
                      (gtk:popover-set-pointing-to (cp-popover *completion*) (cursor-rectangle view))
                      (schedule-completion-refresh view))
                     (t
                      ;; Nothing left here, but the connected Lisp may know more.
                      (close-completion)
                      (when (and (connected-p) (auto-complete-p view))
                        (start-completion view :auto t))))))))))

(defun completion-buffer-changed (buffer)
  "After BUFFER changes: narrow the open popup, or, if the change was a
symbol character typed, start completion soon."
  (let ((typed (and *typed-key* (eq (car *typed-key*) buffer) (cdr *typed-key*))))
    (setf *typed-key* nil)
    (cond ((and *completion* (eq (view-buffer (cp-view *completion*)) buffer))
           (completion-text-changed (cp-view *completion*)))
          ((and typed (= (length typed) 1) (not (terminating-char-p (char typed 0)))
                (not (whitespace-char-p (char typed 0))))
           (let ((view (focused-view *window*)))
             (when (and view (eq (view-buffer view) buffer))
               (schedule-auto-complete view))))
          (t (cancel-auto-complete)))))

(defun completion-cursor-moved (view)
  "Close the popup if VIEW's cursor has left the symbol being completed."
  (when (and *completion* (eq view (cp-view *completion*)))
    (multiple-value-bind (prefix start) (prefix-before-cursor view)
      (when (or (/= start (cp-start *completion*)) (string= prefix ""))
        (close-completion)))))
