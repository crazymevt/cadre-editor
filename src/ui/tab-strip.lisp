;;;; tab-strip.lisp — compact editor tabs
;;;;
;;;; Each editor group's tabs, drawn by Cadre over the group's adw:tab-view
;;;; (which still holds the pages): a tab is as wide as its title and one
;;;; line tall. Click selects, middle-click or × closes, right-click offers
;;;; Close, Close Others, Split and Move. Dragging a tab onto another tab
;;;; (in this group or another) moves it there; onto the empty end of a
;;;; strip, to the end. The strip scrolls sideways when the tabs don't fit,
;;;; and the button at its end lists every open tab.

(in-package #:cadre-ui)

(defstruct (tab-strip (:conc-name strip-) (:constructor %make-tab-strip))
  group widget box scroller (tabs '()))  ; tabs: (page . tab widget)

(defvar *tab-widgets* (make-hash-table :test 'eq)
  "Tab widget → (group . page), for dropping a dragged tab.")
(defvar *strip-widgets* (make-hash-table :test 'eq)
  "Strip box → group, for dropping a tab past the last one.")

(defun make-tab-strip (win group)
  (let* ((box (make-instance 'gtk:box :spacing 1 :css-classes '("cadre-tab-box")))
         (filler (make-instance 'gtk:box :hexpand t))
         (row (gtk:build (gtk:box box filler)))
         (scroller (make-instance 'gtk:scrolled-window :hscrollbar-policy :external :vscrollbar-policy :never
                                                       :hexpand t :child row))
         (strip (%make-tab-strip :group group :box box :scroller scroller)))
    (setf (gethash row *strip-widgets*) group
          (gethash filler *strip-widgets*) group)
    (setf (strip-widget strip)
          (gtk:build
            (gtk:box :css-classes '("cadre-tabs")
              scroller
              (command-button "cadre-tabs-symbolic" "Show all open tabs" 'switch-to-buffer :id :tabs-button))))
    (let ((adjustment (gtk:scrolled-window-get-hadjustment scroller))
          (wheel (gtk:event-controller-scroll-new '(:vertical :horizontal))))
      (gobject:connect wheel :scroll
                       (lambda (controller dx dy)
                         (declare (ignore controller))
                         (gtk:adjustment-set-value adjustment (+ (gtk:adjustment-get-value adjustment) (* 30 (+ dx dy))))
                         t))
      (gtk:widget-add-controller scroller wheel)
      ;; When the strip's size changes (a split, a resize), keep the selected tab in view.
      (gobject:connect adjustment :changed (lambda (&rest args) (declare (ignore args))
                                             (scroll-to-selected-tab strip))))
    (let ((tabs (group-tab-view group))
          (refresh (lambda (&rest args) (declare (ignore args)) (refresh-tab-strip win strip))))
      (gobject:connect tabs :page-attached
                       (lambda (tv page position)
                         (declare (ignore tv position))
                         ;; Titles change as buffers are modified and saved.
                         (gobject:connect page "notify::title" refresh)
                         (refresh-tab-strip win strip)))
      (gobject:connect tabs :page-detached refresh)
      (gobject:connect tabs :page-reordered refresh)
      (gobject:connect tabs "notify::selected-page"
                       (lambda (&rest args) (declare (ignore args))
                         (update-tab-selection strip)
                         (scroll-to-selected-tab strip))))
    strip))

(defun tab-close-page (group page)
  (adw:tab-view-close-page (group-tab-view group) page))

(defun make-tab (win strip page selected)
  (let* ((group (strip-group strip))
         (title (adw:tab-page-get-title page))
         ;; Short names are never shortened (the strip scrolls instead);
         ;; long ones may be, down to 20 characters.
         (label (make-instance 'gtk:label :label title :ellipsize :middle
                                          :width-chars (min (length title) 20)
                                          :max-width-chars 32 :single-line-mode t))
         (close (make-instance 'gtk:button :icon-name "window-close-symbolic" :valign :center
                                           :css-classes '("flat" "cadre-tab-close") :tooltip-text "Close"))
         (tab (gtk:build (gtk:box :spacing 2 :css-classes (if selected '("cadre-tab" "selected") '("cadre-tab"))
                           label close))))
    (gtk:widget-set-tooltip-text tab (adw:tab-page-get-tooltip page))
    (setf (gethash tab *tab-widgets*) (cons group page))
    (gobject:connect close :clicked (lambda (b) (declare (ignore b)) (tab-close-page group page)))
    (let ((click (gtk:gesture-click-new)))
      (gtk:gesture-single-set-button click 0)
      (gobject:connect click :pressed
                       (lambda (gesture n x y)
                         (declare (ignore n))
                         (case (gtk:gesture-single-get-current-button gesture)
                           (1 (select-tab win group page))
                           (2 (tab-close-page group page))
                           (3 (select-tab win group page)
                              (show-tab-menu tab x y)))))
      (gtk:widget-add-controller tab click))
    (let ((drag (gtk:gesture-drag-new)))
      (gobject:connect drag :drag-end
                       (lambda (gesture dx dy)
                         (when (> (+ (abs dx) (abs dy)) 12)
                           (multiple-value-bind (ok sx sy) (gtk:gesture-drag-get-start-point gesture)
                             (when ok (drop-tab win group page tab (+ sx dx) (+ sy dy)))))))
      (gtk:widget-add-controller tab drag))
    tab))

(defun refresh-tab-strip (win strip)
  "Draw STRIP's tabs again from its group's pages."
  (let* ((group (strip-group strip))
         (box (strip-box strip))
         (selected (adw:tab-view-get-selected-page (group-tab-view group))))
    (loop for (nil . tab) in (strip-tabs strip) do (remhash tab *tab-widgets*))
    (clear-box box)
    (setf (strip-tabs strip)
          (loop for page in (group-pages group)
                collect (let ((tab (make-tab win strip page (eq page selected))))
                          (gtk:box-append box tab)
                          (cons page tab))))
    (scroll-to-selected-tab strip)))

(defun update-tab-selection (strip)
  (let ((selected (adw:tab-view-get-selected-page (group-tab-view (strip-group strip)))))
    (loop for (page . tab) in (strip-tabs strip)
          do (if (eq page selected)
                 (gtk:widget-add-css-class tab "selected")
                 (gtk:widget-remove-css-class tab "selected")))))

(defun scroll-to-selected-tab (strip)
  ;; After a short wait, so new tabs have their sizes.
  (glib:timeout-add glib:+priority-default+ 50
                 (lambda ()
                   (let* ((selected (adw:tab-view-get-selected-page (group-tab-view (strip-group strip))))
                          (tab (cdr (assoc selected (strip-tabs strip))))
                          (adjustment (gtk:scrolled-window-get-hadjustment (strip-scroller strip))))
                     (when tab
                       (multiple-value-bind (ok x) (gtk:widget-translate-coordinates tab (strip-box strip) 0d0 0d0)
                         (when ok (gtk:adjustment-clamp-page adjustment x (+ x (gtk:widget-get-width tab)))))))
                   nil)))

(defun select-tab (win group page)
  (adw:tab-view-set-selected-page (group-tab-view group) page)
  (activate-group win group)
  (let ((view (page-view win page)))
    (when view (focus-view view))))

;;; Moving tabs by dragging

(defun widget-owner (widget table)
  "The first of WIDGET and its parents that TABLE knows, and its entry."
  (loop for w = widget then (gtk:widget-get-parent w)
        while w
        do (let ((entry (gethash w table)))
             (when entry (return (values w entry))))))

(defun drop-tab (win from-group page tab x y)
  "PAGE's TAB was dragged to (X, Y) in TAB's coordinates: move it to the tab or strip there."
  (let ((root (window-gtk-window win)))
    (multiple-value-bind (ok rx ry) (gtk:widget-translate-coordinates tab root x y)
      (when ok
        (let ((target (gtk:widget-pick root rx ry '(:default))))
          (multiple-value-bind (over entry) (and target (widget-owner target *tab-widgets*))
            (declare (ignore over))
            (multiple-value-bind (strip-widget to-group) (and target (not entry) (widget-owner target *strip-widgets*))
              (declare (ignore strip-widget))
              (let* ((to-group (or (car entry) to-group))
                     (to-tabs (and to-group (group-tab-view to-group)))
                     (position (cond (entry (adw:tab-view-get-page-position to-tabs (cdr entry)))
                                     (to-tabs (adw:tab-view-get-n-pages to-tabs)))))
                (cond ((null to-group))
                      ((eq to-group from-group)
                       (unless (eq (cdr entry) page)
                         (adw:tab-view-reorder-page to-tabs page (min position (1- (adw:tab-view-get-n-pages to-tabs))))))
                      (t (move-page-to-group win from-group page to-group position)))))))))))

(defun move-page-to-group (win from page to position)
  (let ((view (page-view win page)))
    (if (and view (find to (buffer-views win (view-buffer view)) :key #'view-group))
        ;; TO already shows the buffer: show that tab, and close this one.
        (progn (show-buffer win (view-buffer view) :group to)
               (close-view win view))
        (progn (adw:tab-view-transfer-page (group-tab-view from) page (group-tab-view to) position)
               (select-tab win to page)))))

;;; The tab's menu

(defun show-tab-menu (tab x y)
  (let ((menu (gio:menu-new)))
    (command-item menu "Close" 'close-tab)
    (command-item menu "Close Others" 'close-other-tabs)
    (let ((split (gio:menu-new)))
      (command-item split "Split Right" 'split-right)
      (command-item split "Split Down" 'split-below)
      (command-item split "Move to Next Group" 'move-tab-to-next-group)
      (gio:menu-append-section menu nil split))
    (let ((popover (gtk:popover-menu-new-from-model menu)))
      (gtk:widget-set-parent popover tab)
      (gtk:popover-set-pointing-to popover (gdk:make-rectangle :x (round x) :y (round y) :width 1 :height 1))
      (gtk:popover-set-has-arrow popover nil)
      (gobject:connect popover :closed (lambda (p)
                                         (glib:idle-add glib:+priority-default-idle+
                                                        (lambda () (gtk:widget-unparent p) nil))))
      (gtk:popover-popup popover))))

(define-command close-other-tabs ()
  "Close every tab in this group but the selected one."
  (let* ((win *window*)
         (view (current-tab-view))
         (group (view-group view)))
    (dolist (page (group-pages group))
      (unless (eq page (view-page win view))
        (tab-close-page group page)))))
