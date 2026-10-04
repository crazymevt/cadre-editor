;;;; git-history.lisp — a project's or a file's commits, blame in the
;;;; gutter, and stashes
;;;;
;;;; History is a panel page: the commits newest first (more on request);
;;;; clicking one shows it as a diff. Blame annotates each run of lines in
;;;; the gutter with the commit that last changed it (unsaved edits show as
;;;; not committed yet), and clicking an annotation shows that commit.
;;;; Stashes are listed on the Source Control page, with Apply, Pop and Drop.

(in-package #:cadre-ui)

;;; Showing a commit

(defun show-commit (root hash &key path)
  (git-async (lambda () (git-show-text root hash :path path))
             (lambda (text)
               (show-diff-buffer (format nil "*Commit ~a~@[ ~a~]*" (subseq hash 0 (min 7 (length hash)))
                                         (and path (file-namestring path)))
                                 text))))

;;; The History page

(defstruct (history (:conc-name hist-))
  widget list title root path (count 200) (commits '()))

(defvar *history* nil)
(defvar *history-rows* (make-hash-table :test 'eq) "History row → commit plist, or :more.")

(defun make-history-widget ()
  (let* ((list (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (title (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :css-classes '("heading") :label "No history yet"))
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :margin-start 10 :margin-end 6 :margin-top 4 :margin-bottom 4 title)
                     (gtk:separator)
                     (gtk:scrolled-window :vexpand t :child list)))))
    (setf *history* (make-history :widget widget :list list :title title))
    (gtk:list-box-set-activate-on-single-click list t)
    (gobject:connect list :row-activated
                     (lambda (lb row)
                       (declare (ignore lb))
                       (let ((item (gethash row *history-rows*))
                             (h *history*))
                         (cond ((eq item :more) (load-history h :more t))
                               (item (show-commit (hist-root h) (getf item :hash) :path (hist-path h)))))))
    widget))

(defun history-row (commit)
  (let ((row (make-instance 'gtk:list-box-row
                            :child (gtk:build
                                     (gtk:box :orientation :vertical :margin-start 8 :margin-end 8 :margin-top 2 :margin-bottom 2
                                       (gtk:label :label (getf commit :subject) :xalign 0.0 :ellipsize :end)
                                       (gtk:label :label (format nil "~a · ~a · ~a" (getf commit :short) (getf commit :author)
                                                                 (relative-time (getf commit :time)))
                                                  :xalign 0.0 :ellipsize :end :css-classes '("dim-label" "caption")))))))
    (setf (gethash row *history-rows*) commit)
    row))

(defun fill-history (h)
  (let ((list (hist-list h)))
    (loop for row = (gtk:list-box-get-row-at-index list 0) then (gtk:widget-get-next-sibling row)
          while row do (remhash row *history-rows*))
    (gtk:list-box-remove-all list)
    (if (null (hist-commits h))
        (gtk:list-box-append list (make-instance 'gtk:label :label "No commits" :xalign 0.0 :margin-start 8
                                                            :css-classes '("dim-label")))
        (dolist (commit (hist-commits h)) (gtk:list-box-append list (history-row commit))))
    ;; A full page may have more after it.
    (when (and (hist-commits h) (zerop (mod (length (hist-commits h)) (hist-count h))))
      (let ((more (make-instance 'gtk:list-box-row
                                 :child (make-instance 'gtk:label :label "Show more…" :xalign 0.0 :margin-start 8
                                                                  :css-classes '("accent")))))
        (setf (gethash more *history-rows*) :more)
        (gtk:list-box-append list more)))))

(defun load-history (h &key more)
  (let ((root (hist-root h))
        (path (hist-path h))
        (skip (if more (length (hist-commits h)) 0))
        (count (hist-count h)))
    (git-async (lambda () (git-log root :path path :count count :skip skip))
               (lambda (commits)
                 (setf (hist-commits h) (if more (append (hist-commits h) commits) commits))
                 (fill-history h)))))

(defun show-history-page (root &key path)
  (let ((h *history*))
    (setf (hist-root h) root (hist-path h) path (hist-commits h) '())
    (gtk:label-set-text (hist-title h) (if path (format nil "History of ~a" path)
                                           (format nil "History of ~a" (or *git-branch* "the project"))))
    (panel-set-title (window-panel *window*) "history" (if path (format nil "History: ~a" (file-namestring path)) "History"))
    (set-panel-visible *window* t)
    (panel-show (window-panel *window*) "history")
    (load-history h)))

(defun forget-history ()
  "Empty the History page and take its tab away (the project changed)."
  (let ((h *history*))
    (when h
      (setf (hist-root h) nil (hist-path h) nil (hist-commits h) '())
      (gtk:label-set-text (hist-title h) "No history yet")
      (fill-history h)
      (when *window*
        (panel-set-title (window-panel *window*) "history" "History")
        (panel-hide-page (window-panel *window*) "history")))))

(define-command show-history ()
  "Show the project's commits, newest first."
  (show-history-page (git-root-or-error)))

(define-command show-file-history ()
  "Show the commits that changed this file."
  (let ((gf (current-git-file (current-view))))
    (show-history-page (gf-root gf) :path (gf-relative gf))))

;;; Blame in the gutter

(defparameter *blame-width-chars* 26)

(defun blame-on-p (buffer) (buffer-local buffer :blame-on))

(defun blame-width (view)
  "The pixels the blame column takes in VIEW's gutter, or 0."
  (if (blame-on-p (view-buffer view))
      (let ((layout (gtk:widget-create-pango-layout (view-text-view view)
                                                    (make-string *blame-width-chars* :initial-element #\8))))
        (+ (pango:layout-get-pixel-size layout) 12))
      0))

(defun refresh-blame (buffer)
  "Work out who changed each line of BUFFER (as edited), on a thread."
  (let ((gf (buffer-git buffer)))
    (when (and gf (blame-on-p buffer))
      (let ((text (buffer-string buffer)))
        (git-async (lambda () (git-blame (gf-root gf) (gf-relative gf) text))
                   (lambda (blame)
                     (when (blame-on-p buffer)
                       (setf (buffer-local buffer :blame) blame)
                       (redraw-gutters buffer)))
                   (lambda (error)
                     (setf (buffer-local buffer :blame-on) nil)
                     (resize-gutters buffer)
                     (message "Blame: ~a" error)))))))

(defun resize-gutters (buffer)
  (when *window*
    (dolist (view (buffer-views *window* buffer))
      (when (view-gutter view)
        (setf (view-gutter-digits view) 0)  ; so the width is worked out again
        (update-gutter-width view)
        (gtk:widget-queue-draw (view-gutter view))))))

(define-command toggle-blame ()
  "Show who last changed each line, in the gutter (or stop showing it)."
  (let* ((view (current-view))
         (buffer (view-buffer view)))
    (current-git-file view)
    (setf (buffer-local buffer :blame-on) (not (blame-on-p buffer))
          (buffer-local buffer :blame) nil)
    (resize-gutters buffer)
    (if (blame-on-p buffer)
        (progn (refresh-blame buffer) (message "Blame on: click an annotation to see its commit"))
        (message "Blame off"))))

(defun blame-text (entry)
  (if (getf entry :uncommitted)
      "Not committed yet"
      (let ((author (getf entry :author "")))
        (format nil "~a ~a ~a" (getf entry :short)
                (if (> (length author) 9) (subseq author 0 9) author)
                (relative-time (getf entry :time 0))))))

(defun draw-blame-line (view cr layout line y)
  "Draw LINE's blame at Y in VIEW's gutter: only on the first line of each
run of lines from the same commit."
  (let ((blame (buffer-local (view-buffer view) :blame)))
    (when (and blame (< line (length blame)))
      (let ((entry (aref blame line))
            (previous (and (plusp line) (aref blame (1- line))))
            (color (gtk:widget-get-color (view-gutter view))))
        (unless (and previous (equal (getf previous :hash) (getf entry :hash)))
          (let ((text (blame-text entry)))
            (pango:layout-set-text layout (if (> (length text) *blame-width-chars*)
                                              (subseq text 0 *blame-width-chars*)
                                              text)
                                   -1)
            (cairo:set-source-rgba cr (gdk:rgba-red color) (gdk:rgba-green color) (gdk:rgba-blue color)
                                   (if (getf entry :uncommitted) 0.35d0 0.6d0))
            (cairo:move-to cr 8d0 (float y 1d0))
            (pango-cairo:show-layout cr layout)))))))

(defun blame-at-line (buffer line)
  (let ((blame (buffer-local buffer :blame)))
    (and blame (< line (length blame)) (aref blame line))))

;;; Stashes


(define-command stash-changes ()
  "Put your uncommitted changes (and new files) aside in a stash."
  (let ((root (git-root-or-error)))
    (when (some #'buffer-needs-saving-p (buffer-list))
      (message "Unsaved changes stay in their buffers; only saved files are stashed"))
    (open-picker (window-picker *window*)
                 :placeholder "Stash message (optional)"
                 :on-choose (lambda (text)
                              (git-run root (lambda () (git-stash-push root :message (string-trim " " text)))
                                       (lambda (r) (declare (ignore r)) (message "Stashed your changes")))))))

(defun choose-stash (prompt then)
  (let ((root (git-root-or-error)))
    (git-async (lambda () (git-stashes root))
               (lambda (stashes)
                 (unless stashes (editor-error "No stashes"))
                 (open-picker (window-picker *window*)
                              :items stashes
                              :label (lambda (s) (getf s :subject))
                              :detail (lambda (s) (getf s :ref))
                              :placeholder prompt
                              :on-choose (lambda (s) (funcall then root s)))))))

(defun apply-stash-entry (root stash &key pop)
  (git-run root (lambda () (if pop (git-stash-pop root (getf stash :ref)) (git-stash-apply root (getf stash :ref))))
           (lambda (r) (declare (ignore r)) (message "~:[Applied~;Popped~] ~a" pop (getf stash :ref)))))

(defun drop-stash-entry (root stash)
  (ask-yes (format nil "Drop ~a?" (getf stash :ref)) (format nil "“~a” is lost." (getf stash :subject)) "_Drop"
           (lambda () (git-run root (lambda () (git-stash-drop root (getf stash :ref)))
                               (lambda (r) (declare (ignore r)) (message "Dropped ~a" (getf stash :ref)))))))

(define-command apply-stash ()
  "Bring a stash's changes back, keeping the stash."
  (choose-stash "Apply which stash?" (lambda (root s) (apply-stash-entry root s))))

(define-command pop-stash ()
  "Bring a stash's changes back and drop it."
  (choose-stash "Pop which stash?" (lambda (root s) (apply-stash-entry root s :pop t))))

(define-command drop-stash ()
  "Throw a stash away (asking first)."
  (choose-stash "Drop which stash?" (lambda (root s) (drop-stash-entry root s))))

(defun fill-stash-list (list root)
  (gtk:list-box-remove-all list)
  (when *git-stash-list*
    (gtk:list-box-append list (make-instance 'gtk:list-box-row :activatable nil :selectable nil
                                                               :child (sc-section-title "Stashes" (length *git-stash-list*))))
    (dolist (stash *git-stash-list*)
      (let* ((buttons (make-instance 'gtk:box))
             (row (make-instance 'gtk:list-box-row :activatable nil
                                 :child (gtk:build (gtk:box :spacing 6 :margin-start 10 :margin-end 6
                                                     (gtk:label :label (getf stash :subject) :xalign 0.0 :hexpand t
                                                                :ellipsize :end :tooltip-text (getf stash :ref))
                                                     buttons)))))
        (flet ((button (text tooltip action)
                 (let ((b (make-instance 'gtk:button :label text :tooltip-text tooltip
                                                     :css-classes '("flat" "caption" "cadre-sc-button"))))
                   (gobject:connect b :clicked (lambda (x) (declare (ignore x)) (funcall action)))
                   (gtk:box-append buttons b))))
          (button "Apply" "Apply, keeping the stash" (lambda () (apply-stash-entry root stash)))
          (button "Pop" "Apply and drop the stash" (lambda () (apply-stash-entry root stash :pop t)))
          (button "Drop" "Throw the stash away" (lambda () (drop-stash-entry root stash))))
        (gtk:list-box-append list row)))))
