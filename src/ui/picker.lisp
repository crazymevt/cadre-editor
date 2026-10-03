;;;; picker.lisp — a popover to pick from a list by typing: the command
;;;; palette, quick open, switching buffers, and one-line prompts
;;;;
;;;; Typing filters the items with fuzzy matching; Up and Down move, Enter
;;;; chooses, Escape closes. With no items it is a plain prompt: Enter
;;;; passes the text typed.

(in-package #:cadre-ui)

(defclass picker ()
  ((popover :reader picker-popover)
   (entry :reader picker-entry)
   (list-view :reader picker-list-view)
   (scroller :reader picker-scroller)
   (store :reader picker-store)
   (selection :reader picker-selection)
   (items :initform '() :accessor picker-items)
   (label :initform #'princ-to-string :accessor picker-label)
   (detail :initform nil :accessor picker-detail)
   (on-choose :initform nil :accessor picker-on-choose)))

(defparameter *picker-limit* 300 "The most items a picker shows at once.")

(defun make-picker-row ()
  (gtk:build
    (gtk:box :spacing 12 :margin-start 6 :margin-end 6 :margin-top 3 :margin-bottom 3
      (gtk:label :xalign 0.0 :hexpand t :ellipsize :middle)
      (gtk:label :xalign 1.0 :css-classes '("dim-label" "caption")))))

(defun make-picker (parent)
  "A picker whose popover points at PARENT (a widget)."
  (let* ((picker (make-instance 'picker))
         (store (gio:make-list-store))
         (selection (gtk:single-selection-new store))
         (entry (make-instance 'gtk:search-entry :search-delay 0 :hexpand t))
         (list-view (gtk:list-view-new
                     selection
                     (gtk:make-factory
                      :setup 'make-picker-row
                      :bind (lambda (row item)
                              (let ((name (gtk:widget-get-first-child row)))
                                (gtk:label-set-text name (funcall (picker-label picker) item))
                                (gtk:label-set-text (gtk:widget-get-next-sibling name)
                                                    (or (and (picker-detail picker)
                                                             (funcall (picker-detail picker) item))
                                                        "")))))))
         (scroller (make-instance 'gtk:scrolled-window :child list-view
                                                       :hscrollbar-policy :never
                                                       :min-content-height 120
                                                       :max-content-height 380
                                                       :propagate-natural-height t))
         (popover (make-instance 'gtk:popover :has-arrow nil :position :bottom
                                              :child (gtk:build
                                                       (gtk:box :orientation :vertical :spacing 6
                                                                :width-request 560
                                                         entry scroller)))))
    (gtk:widget-add-css-class list-view "navigation-sidebar")
    (gtk:widget-set-parent popover parent)
    (setf (slot-value picker 'popover) popover
          (slot-value picker 'entry) entry
          (slot-value picker 'list-view) list-view
          (slot-value picker 'scroller) scroller
          (slot-value picker 'store) store
          (slot-value picker 'selection) selection)
    (gobject:connect entry :search-changed (lambda (e) (declare (ignore e)) (refilter picker)))
    (gobject:connect entry :activate (lambda (e) (declare (ignore e)) (choose picker)))
    (gobject:connect entry :stop-search (lambda (e) (declare (ignore e)) (close-picker picker)))
    (gobject:connect list-view :activate (lambda (lv position) (declare (ignore lv))
                                           (gtk:single-selection-set-selected selection position)
                                           (choose picker)))
    (let ((keys (gtk:event-controller-key-new)))
      (gobject:connect keys :key-pressed
                       (lambda (c keyval keycode state)
                         (declare (ignore c keycode state))
                         (let ((name (gdk:keyval-name keyval)))
                           (cond ((string= name "Down") (move-selection picker 1) t)
                                 ((string= name "Up") (move-selection picker -1) t)
                                 ((string= name "Page_Down") (move-selection picker 10) t)
                                 ((string= name "Page_Up") (move-selection picker -10) t)
                                 (t nil)))))
      (gtk:widget-add-controller entry keys))
    (gobject:connect popover :closed (lambda (p) (declare (ignore p))
                                       (let ((view (and *window* (selected-view *window*))))
                                         (when view (focus-view view)))))
    picker))

(defun move-selection (picker delta)
  (let* ((selection (picker-selection picker))
         (n (gio:list-model-get-n-items (picker-store picker)))
         (current (gtk:single-selection-get-selected selection)))
    (when (plusp n)
      (let ((new (max 0 (min (1- n) (if (= current gtk:+invalid-list-position+) 0 (+ current delta))))))
        (gtk:single-selection-set-selected selection new)
        (gtk:list-view-scroll-to (picker-list-view picker) new '(:none) nil)))))

(defun refilter (picker)
  (let* ((store (picker-store picker))
         (pattern (gtk:editable-get-text (picker-entry picker)))
         (matches (if (string= pattern "")
                      (let ((items (picker-items picker)))
                        (if (> (length items) *picker-limit*) (subseq items 0 *picker-limit*) items))
                      (fuzzy-filter pattern (picker-items picker)
                                    :key (picker-label picker) :limit *picker-limit*))))
    (gio:list-store-remove-all store)
    (dolist (item matches)
      (gio:list-store-append store (gobject:make-lisp-object item)))
    (when matches
      (gtk:single-selection-set-selected (picker-selection picker) 0)
      (gtk:list-view-scroll-to (picker-list-view picker) 0 '(:none) nil))))

(defun choose (picker)
  (let* ((selection (picker-selection picker))
         (position (gtk:single-selection-get-selected selection))
         (on-choose (picker-on-choose picker))
         (item (cond ((null (picker-items picker)) (gtk:editable-get-text (picker-entry picker)))
                     ((/= position gtk:+invalid-list-position+)
                      (gobject:lisp-object-value (gio:list-model-get-item (picker-store picker) position))))))
    (close-picker picker)
    (when (and item on-choose)
      (handler-case (funcall on-choose item)
        (editor-error (e) (message "~a" (editor-error-message e)))))))

(defun close-picker (picker)
  (gtk:popover-popdown (picker-popover picker)))

(defun open-picker (picker &key items (label #'princ-to-string) detail on-choose
                                (placeholder "") (text ""))
  "Show PICKER with ITEMS (a list), shown with LABEL and DETAIL (functions
of an item returning strings). ON-CHOOSE gets the chosen item, or the text
typed when ITEMS is empty."
  (setf (picker-items picker) items
        (picker-label picker) label
        (picker-detail picker) detail
        (picker-on-choose picker) on-choose)
  (gtk:widget-set-visible (picker-scroller picker) (and items t))
  (gtk:search-entry-set-placeholder-text (picker-entry picker) placeholder)
  (gtk:editable-set-text (picker-entry picker) text)
  (refilter picker)
  (gtk:popover-popup (picker-popover picker))
  (gtk:widget-grab-focus (picker-entry picker)))
