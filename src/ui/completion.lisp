;;;; completion.lisp — the completion popup at the cursor
;;;;
;;;; A popover under the cursor lists the completions. Typing keeps going to
;;;; the editor and narrows the list; Up and Down choose, Return or Tab
;;;; insert, Escape closes.

(in-package #:cadre-ui)

(defstruct (completion-popup (:conc-name cp-))
  popover list-view store selection view start items)

(defvar *completion* nil "The open completion popup, or nil.")

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
  (when items (gtk:single-selection-set-selected (cp-selection popup) 0)))

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
         (popover (make-instance 'gtk:popover :autohide nil :has-arrow nil :position :bottom
                                              :can-focus nil
                                              :child (make-instance 'gtk:scrolled-window
                                                                    :child list-view :hscrollbar-policy :never
                                                                    :min-content-width 320
                                                                    :max-content-height 260
                                                                    :propagate-natural-height t))))
    (gtk:widget-set-can-focus list-view nil)
    (gtk:widget-set-parent popover (view-text-view view))
    (gtk:popover-set-pointing-to popover (cursor-rectangle view))
    (setf *completion* (make-completion-popup :popover popover :list-view list-view :store store
                                              :selection selection :view view :start start
                                              :items completions))
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
  (let* ((popup *completion*)
         (position (gtk:single-selection-get-selected (cp-selection popup))))
    (when (/= position gtk:+invalid-list-position+)
      (let ((completion (gobject:lisp-object-value (gio:list-model-get-item (cp-store popup) position))))
        (close-completion)
        (replace-prefix (cp-view popup) (cp-start popup) (first completion))))))

(defun completion-key (keyval)
  "Handle KEYVAL if the completion popup wants it. Returns t if used."
  (when (completion-open-p)
    (let ((name (gdk:keyval-name keyval)))
      (cond ((string= name "Down") (move-completion 1) t)
            ((string= name "Up") (move-completion -1) t)
            ((string= name "Page_Down") (move-completion 8) t)
            ((string= name "Page_Up") (move-completion -8) t)
            ((member name '("Return" "KP_Enter" "Tab") :test #'string=) (accept-completion) t)
            ((string= name "Escape") (close-completion) t)
            (t nil)))))

(defun completion-text-changed (view)
  "After an edit in VIEW: narrow the popup's list, or close it."
  (when (and *completion* (eq view (cp-view *completion*)))
    (multiple-value-bind (prefix start) (prefix-before-cursor view)
      (if (or (/= start (cp-start *completion*)) (string= prefix ""))
          (close-completion)
          (let ((items (fuzzy-filter prefix (cp-items *completion*) :key #'first)))
            (if items
                (progn (fill-completions *completion* items)
                       (gtk:popover-set-pointing-to (cp-popover *completion*) (cursor-rectangle view)))
                (close-completion)))))))

(defun completion-buffer-changed (buffer)
  (when (and *completion* (eq (view-buffer (cp-view *completion*)) buffer))
    (completion-text-changed (cp-view *completion*))))
