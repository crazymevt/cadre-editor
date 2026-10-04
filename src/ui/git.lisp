;;;; git.lisp — Git in the editor: changed lines in the gutter, diffs,
;;;; status colors in the explorer, the branch in the status bar, and the
;;;; Source Control page for staging, discarding and committing
;;;;
;;;; Git runs on threads (core's git.lisp); results come back to the GTK
;;;; thread. Branches can be switched, made and deleted, and the current
;;;; one fetched, pulled and pushed (never asking for a password: see
;;;; core's git-network). A file's buffer keeps its text at HEAD, and its changed lines
;;;; are worked out again a moment after each edit, so the gutter shows
;;;; unsaved changes too. Clicking a mark in the gutter shows what was
;;;; there before, and can put it back.

(in-package #:cadre-ui)

(define-option *git-gutter* t boolean
  "Mark lines changed since the last commit in the editor's gutter."
  :category "Editing")

;;; Running git off the GTK thread

(defun git-async (thunk &optional then (on-error (lambda (message) (message "Git: ~a" message))))
  "Run THUNK on a thread; call THEN with its value on the GTK thread, or
ON-ERROR with the message of an error it signals."
  (sb-thread:make-thread
   (lambda ()
     (let ((result nil) (failure nil))
       (handler-case (setf result (funcall thunk))
         (editor-error (e) (setf failure (editor-error-message e)))
         (error (e) (setf failure (princ-to-string e))))
       (glib:call-in-main-thread
        (lambda ()
          (if failure
              (when on-error (funcall on-error failure))
              (when then (funcall then result)))))))
   :name "cadre git"))

(defvar *git-roots* (make-hash-table :test 'equal) "Folder namestring → work tree root, or :none.")

(defun git-root-for (path)
  "The root of PATH's work tree (cached by folder), or nil. Runs git the first time."
  (let* ((directory (namestring (if (uiop:directory-pathname-p path) path (uiop:pathname-directory-pathname path))))
         (cached (gethash directory *git-roots*)))
    (cond ((eq cached :none) nil)
          (cached cached)
          (t (let ((root (ignore-errors (git-toplevel directory))))
               (setf (gethash directory *git-roots*) (or root :none))
               root)))))

(defun project-git-root ()
  (let ((project (and *window* (window-project *window*))))
    (and project (git-root-for project))))

;;; Changed lines in a buffer

(defstruct (git-file (:conc-name gf-))
  root relative head-lines (hunks '()) timer (has-head nil))

(defun buffer-git (buffer) (buffer-local buffer :git))

(defun git-attach (buffer)
  "Start following BUFFER's file in Git (if it is in a work tree)."
  (let ((file (buffer-file buffer)))
    (when (and file (typep (buffer-text buffer) 'gtk:text-buffer))
      (git-async (lambda ()
                   (let ((root (git-root-for file)))
                     (and root
                          (let ((relative (git-relative-path root file)))
                            (list root relative (git-head-text root relative))))))
                 (lambda (found)
                   (when (and found (member buffer (buffer-list)))
                     (destructuring-bind (root relative head) found
                       (setf (buffer-local buffer :git)
                             (make-git-file :root root :relative relative :has-head (and head t)
                                            :head-lines (and head (coerce (split-text-lines head) 'vector))))
                       (unless (buffer-local buffer :git-handler)
                         (setf (buffer-local buffer :git-handler)
                               (gobject:connect (buffer-text buffer) :changed
                                                (lambda (b) (declare (ignore b)) (schedule-git-hunks buffer)))))
                       (refresh-git-hunks buffer))))
                 nil))))

(defun head-text-of (gf)
  (format nil "~{~a~^~%~}" (coerce (gf-head-lines gf) 'list)))

(defun refresh-git-hunks (buffer)
  "Work out BUFFER's changed lines (on a thread) and redraw its gutters."
  (let ((gf (buffer-git buffer)))
    (when (and gf *git-gutter*)
      (if (not (gf-has-head gf))
          (setf (gf-hunks gf) '())
          (let ((old (head-text-of gf))
                (new (buffer-string buffer)))
            (git-async (lambda () (line-changes old new))
                       (lambda (hunks)
                         (when (eq gf (buffer-git buffer))
                           (setf (gf-hunks gf) hunks)
                           (redraw-gutters buffer)))
                       nil))))))

(defun schedule-git-hunks (buffer)
  (let ((gf (buffer-git buffer)))
    (when gf
      (when (gf-timer gf) (glib:source-remove (gf-timer gf)))
      (setf (gf-timer gf)
            (glib:timeout-add glib:+priority-default+ 400
                              (lambda ()
                                (setf (gf-timer gf) nil)
                                (when (member buffer (buffer-list))
                                  (refresh-git-hunks buffer)
                                  (refresh-blame buffer))
                                nil))))))

(defun redraw-gutters (buffer)
  (when *window*
    (dolist (view (buffer-views *window* buffer))
      (when (view-gutter view) (gtk:widget-queue-draw (view-gutter view))))))

(defun reload-git-heads (&optional root)
  "Read the HEAD text again for every buffer (in ROOT), after a commit or checkout."
  (dolist (buffer (buffer-list))
    (let ((gf (buffer-git buffer)))
      (when (and gf (or (null root) (equal (namestring (gf-root gf)) (namestring root))))
        (setf (buffer-local buffer :git) nil)
        (git-attach buffer)))))

(add-hook '*buffer-created-hook* 'git-attach)

;;; Drawing and clicking the marks

(defun hunk-lines (hunk)
  "The lines (from 0) a hunk marks: first and last, or for a deletion the line after it (twice)."
  (destructuring-bind (old-start old-count new-start new-count) hunk
    (declare (ignore old-start old-count))
    (if (zerop new-count)
        (values new-start new-start)
        (values (1- new-start) (+ new-start new-count -2)))))

(defun git-color (kind)
  (hex-rgba (theme-color (ecase kind (:added :git-added) (:modified :git-modified) (:deleted :git-deleted))
                         :foreground "#888888")))

(defun draw-git-marks (view cr)
  "Mark the changed lines at the gutter's left edge."
  (let ((gf (buffer-git (view-buffer view))))
    (when (and gf *git-gutter* (gf-hunks gf))
      (let* ((text-view (view-text-view view))
             (gtk-buffer (view-gtk-buffer view))
             (count (gtk:text-buffer-get-line-count gtk-buffer)))
        (dolist (hunk (gf-hunks gf))
          (multiple-value-bind (first last) (hunk-lines hunk)
            (let* ((kind (hunk-kind hunk))
                   (color (git-color kind)))
              (cairo:set-source-rgba cr (gdk:rgba-red color) (gdk:rgba-green color) (gdk:rgba-blue color) 1d0)
              (if (eq kind :deleted)
                  ;; A small wedge on the line boundary where lines went.
                  (multiple-value-bind (y height)
                      (gtk:text-view-get-line-yrange text-view (line-iter gtk-buffer (min first (1- count))))
                    (let ((y (if (>= first count) (+ y height) y)))
                      (multiple-value-bind (wx wy) (gtk:text-view-buffer-to-window-coords text-view :left 0 y)
                        (declare (ignore wx))
                        (cairo:move-to cr 0d0 (float (- wy 4) 1d0))
                        (cairo:line-to cr 5d0 (float wy 1d0))
                        (cairo:line-to cr 0d0 (float (+ wy 4) 1d0))
                        (cairo:close-path cr)
                        (cairo:fill cr))))
                  (let ((first (min first (1- count))) (last (min last (1- count))))
                    (multiple-value-bind (y1) (gtk:text-view-get-line-yrange text-view (line-iter gtk-buffer first))
                      (multiple-value-bind (y2 h2) (gtk:text-view-get-line-yrange text-view (line-iter gtk-buffer last))
                        (multiple-value-bind (wx wy1) (gtk:text-view-buffer-to-window-coords text-view :left 0 y1)
                          (declare (ignore wx))
                          (multiple-value-bind (wx wy2) (gtk:text-view-buffer-to-window-coords text-view :left 0 (+ y2 h2))
                            (declare (ignore wx))
                            (cairo:rectangle cr 0d0 (float wy1 1d0) 3d0 (float (- wy2 wy1) 1d0))
                            (cairo:fill cr))))))))))))))

(defun hunk-at-line (gf line)
  (find-if (lambda (hunk) (multiple-value-bind (first last) (hunk-lines hunk) (<= first line last)))
           (gf-hunks gf)))

(defun hunk-old-lines (gf hunk)
  (destructuring-bind (old-start old-count new-start new-count) hunk
    (declare (ignore new-start new-count))
    (loop for i from (1- old-start) repeat old-count
          when (< i (length (gf-head-lines gf))) collect (aref (gf-head-lines gf) i))))

(defun revert-hunk (view hunk)
  "Put back what HUNK changed in VIEW's buffer, as one undo step."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (gf (buffer-git (view-buffer view)))
         (old (hunk-old-lines gf hunk))
         (count (gtk:text-buffer-get-line-count gtk-buffer)))
    (destructuring-bind (old-start old-count new-start new-count) hunk
      (declare (ignore old-start old-count))
      (with-user-action (gtk-buffer)
        (if (zerop new-count)
            ;; Lines were deleted after line NEW-START: insert them before the next line.
            (if (< new-start count)
                (insert-text-at gtk-buffer (gtk:text-iter-get-offset (line-iter gtk-buffer new-start))
                                (format nil "~{~a~%~}" old))
                (insert-text-at gtk-buffer (text-length gtk-buffer) (format nil "~%~{~a~^~%~}" old)))
            (let* ((first (1- new-start))
                   (last (+ first new-count -1))
                   (start (gtk:text-iter-get-offset (line-iter gtk-buffer first)))
                   (end (if (< (1+ last) count)
                            (gtk:text-iter-get-offset (line-iter gtk-buffer (1+ last)))
                            (text-length gtk-buffer)))
                   (at-end (>= (1+ last) count)))
              (replace-text-between gtk-buffer start end
                                    (if at-end
                                        (format nil "~{~a~^~%~}" old)
                                        (format nil "~{~a~%~}" old)))))))
    (schedule-git-hunks (view-buffer view))))

(defun show-hunk-popover (view hunk y)
  (let* ((gf (buffer-git (view-buffer view)))
         (gtk-buffer (view-gtk-buffer view))
         (old (hunk-old-lines gf hunk))
         (new (multiple-value-bind (first last) (hunk-lines hunk)
                (if (eq (hunk-kind hunk) :deleted) '()
                    (loop for l from first to last collect (text-line-string gtk-buffer l)))))
         (text (format nil "~{- ~a~%~}~{+ ~a~%~}" old new))
         (box (make-instance 'gtk:box :orientation :vertical :spacing 6 :margin-start 6 :margin-end 6
                                      :margin-top 6 :margin-bottom 6))
         (label (make-instance 'gtk:label :label (string-right-trim '(#\Newline) text) :xalign 0.0
                                          :selectable t :css-classes '("monospace")))
         (buttons (make-instance 'gtk:box :spacing 6 :halign :end))
         (popover (make-instance 'gtk:popover :child box :position :right)))
    (flet ((button (text action &optional classes)
             (let ((b (make-instance 'gtk:button :label text :css-classes classes)))
               (gobject:connect b :clicked (lambda (x) (declare (ignore x))
                                             (gtk:popover-popdown popover)
                                             (funcall action)))
               (gtk:box-append buttons b))))
      (gtk:box-append box (make-instance 'gtk:label :label (format nil "~:(~a~) since the last commit" (hunk-kind hunk))
                                                    :xalign 0.0 :css-classes '("heading")))
      (gtk:box-append box (make-instance 'gtk:scrolled-window :child label :propagate-natural-height t
                                                              :propagate-natural-width t :max-content-height 300
                                                              :max-content-width 640))
      (button "Stage" (lambda () (stage-hunk view hunk)))
      (button "Revert" (lambda () (revert-hunk view hunk)) '("destructive-action"))
      (gtk:box-append box buttons))
    (gtk:widget-set-parent popover (view-gutter view))
    (gtk:popover-set-pointing-to popover (gdk:make-rectangle :x 2 :y (round y) :width 1 :height 1))
    (gobject:connect popover :closed (lambda (p) (glib:idle-add glib:+priority-default-idle+
                                                                (lambda () (gtk:widget-unparent p) nil))))
    (gtk:popover-popup popover)))

(defun setup-git-gutter-clicks (view)
  "Clicking a change mark in VIEW's gutter shows the change."
  (let ((gutter (view-gutter view)))
    (when gutter
      (let ((click (gtk:gesture-click-new)))
        (gobject:connect click :pressed
                         (lambda (gesture n x y)
                           (declare (ignore gesture n))
                           (let ((gf (buffer-git (view-buffer view))))
                             (when gf
                               (let* ((text-view (view-text-view view))
                                      (by (nth-value 1 (gtk:text-view-window-to-buffer-coords text-view :left 0 (round y))))
                                      (line (gtk:text-iter-get-line (gtk:text-view-get-line-at-y text-view by)))
                                      (blame (and (< 6 x (blame-width view)) (blame-at-line (view-buffer view) line)))
                                      (hunk (hunk-at-line gf line)))
                                 (cond (blame (if (getf blame :uncommitted)
                                                  (message "Not committed yet")
                                                  (show-commit (gf-root gf) (getf blame :hash))))
                                       (hunk (show-hunk-popover view hunk y))))))))
        (gtk:widget-add-controller gutter click)))))

;;; Commands on changes

(defun current-git-file (view)
  (or (buffer-git (view-buffer view)) (editor-error "~a isn't in a Git repository" (buffer-name (view-buffer view)))))

(defun goto-change (direction)
  (let* ((view (current-view))
         (gf (current-git-file view))
         (line (cursor-line-column view))
         (starts (sort (mapcar (lambda (h) (values (hunk-lines h))) (gf-hunks gf)) #'<))
         (target (if (plusp direction)
                     (or (find-if (lambda (s) (> s line)) starts) (first starts))
                     (or (find-if (lambda (s) (< s line)) starts :from-end t) (car (last starts))))))
    (unless target (editor-error "No changes since the last commit"))
    (goto-line-column view (min target (1- (gtk:text-buffer-get-line-count (view-gtk-buffer view)))) 0)))

(define-command git-next-change ()
  "Go to the next line changed since the last commit."
  (goto-change 1))

(define-command git-previous-change ()
  "Go to the previous line changed since the last commit."
  (goto-change -1))

(define-command git-revert-change ()
  "Put back the change (since the last commit) at the cursor."
  (let* ((view (current-view))
         (gf (current-git-file view))
         (hunk (or (hunk-at-line gf (cursor-line-column view)) (editor-error "No change at the cursor"))))
    (revert-hunk view hunk)))

(defun show-diff-buffer (title text &key root relative kind)
  "Show the unified diff TEXT in a buffer named TITLE, colored. With ROOT,
RELATIVE and KIND (:worktree, :staged or :head), it's a file's live diff."
  (let ((buffer (or (find-buffer title) (make-buffer :name title :text (make-gtk-text) :major-mode 'git-diff-mode))))
    (setf (buffer-local buffer :diff-root) root
          (buffer-local buffer :diff-relative) relative
          (buffer-local buffer :diff-kind) kind)
    (fill-diff-buffer buffer (if (string= text "") "No changes." text))
    (let ((view (show-buffer *window* buffer)))
      (gtk:text-view-set-editable (view-text-view view) nil)
      (gtk:text-buffer-place-cursor (buffer-text buffer) (gtk:text-buffer-get-start-iter (buffer-text buffer)))
      (when (member kind '(:worktree :staged))
        (message "~:[s stages~;u unstages~] the change at the cursor; n and p move between changes" (eq kind :staged)))
      view)))

(defun fill-diff-buffer (buffer text)
  (let ((gtk-buffer (buffer-text buffer)))
    (gtk:text-buffer-set-text gtk-buffer text -1)
    (let ((added (ensure-face-tag gtk-buffer "cadre-diff-added" :diff-added))
          (removed (ensure-face-tag gtk-buffer "cadre-diff-removed" :diff-removed))
          (header (ensure-face-tag gtk-buffer "cadre-diff-header" :md-markup)))
      (loop for line in (split-text-lines text)
            for n from 0
            for tag = (cond ((or (string= "+++" line :end2 (min 3 (length line)))
                                 (string= "---" line :end2 (min 3 (length line)))
                                 (string= "@@" line :end2 (min 2 (length line)))
                                 (string= "diff " line :end2 (min 5 (length line)))
                                 (string= "index " line :end2 (min 6 (length line))))
                             header)
                            ((and (plusp (length line)) (char= (char line 0) #\+)) added)
                            ((and (plusp (length line)) (char= (char line 0) #\-)) removed))
            when tag
              do (gtk:text-buffer-apply-tag gtk-buffer tag (line-iter gtk-buffer n) (line-end-iter gtk-buffer n))))
    (setf (buffer-modified-p buffer) nil)))

(defun open-git-diff (root relative &key (kind :head))
  "Show RELATIVE's changes: KIND :worktree (not staged yet), :staged, or
:head (all of them, against the last commit)."
  (git-async (lambda () (git-diff-text root relative :staged (eq kind :staged) :head (eq kind :head)))
             (lambda (text)
               (show-diff-buffer (format nil "*Diff ~a~a*" (file-namestring relative)
                                         (ecase kind (:worktree " (unstaged)") (:staged " (staged)") (:head "")))
                                 text :root root :relative relative :kind kind))))

(define-command git-diff-file ()
  "Show this file's changes since the last commit (as saved)."
  (let* ((view (current-view))
         (gf (current-git-file view)))
    (when (buffer-modified-p (view-buffer view)) (message "Showing the saved file; it has unsaved changes"))
    (open-git-diff (gf-root gf) (gf-relative gf) :kind :head)))

(define-command git-stage-file ()
  "Stage this file's saved changes."
  (let* ((view (current-view))
         (gf (current-git-file view)))
    (when (buffer-modified-p (view-buffer view)) (editor-error "Save ~a first" (buffer-name (view-buffer view))))
    (git-async (lambda () (git-stage (gf-root gf) (list (gf-relative gf))))
               (lambda (r) (declare (ignore r)) (message "Staged ~a" (gf-relative gf)) (git-changed (gf-root gf))))))

;;; Status: explorer colors, the branch, the Source Control page

(defvar *git-status* (make-hash-table :test 'equal) "Path namestring → (kind letter), for the project.")
(defvar *git-branch* nil)
(defvar *git-entries* '() "The project's status entries, from git-status.")
(defvar *git-head* nil)
(defvar *git-ahead* nil "Commits the branch is ahead of its upstream, or nil without one.")
(defvar *git-behind* nil)
(defvar *git-stash-list* '() "The project's stashes, from git-stashes.")
(defvar *git-operation* nil "A merge, rebase … under way (:merge, :rebase …), or nil.")
(defvar *git-merge-message* nil)
(defvar *git-busy* nil "What git is doing with a remote, while it does (\"Pushing\" …), or nil.")

(define-option *git-pull-mode* :ff-only (member :ff-only :merge :rebase)
  "How Pull brings in the upstream's commits: :ff-only (only when there's
nothing of yours to combine), :merge, or :rebase."
  :category "Editing")

(defun git-status-of (path)
  "PATH's Git status in the project, as (kind letter), or nil. (Looked up by
real path: /var and /private/var are one place on macOS.)"
  (gethash (namestring (real-path path)) *git-status*))

(defun status-letter (index worktree)
  (cond ((char= index #\?) "U")
        ((member #\U (list index worktree)) "!")
        ((char/= worktree #\Space) (string worktree))
        (t (string index))))

(defun git-changed (&optional (root (project-git-root)))
  "After git or the files changed: read the status again, and the HEAD texts if HEAD moved."
  (when root
    (git-async (lambda ()
                 (list (ignore-errors (git-status root)) (git-branch root) (git-head-id root)
                       (multiple-value-list (ignore-errors (git-ahead-behind root)))
                       (ignore-errors (git-stashes root))
                       (let ((op (ignore-errors (git-operation root))))
                         (list op (and (eq op :merge) (ignore-errors (git-merge-message root)))))))
               ;; (Without an upstream there's no ahead or behind: (nil).)
               (lambda (result)
                 (destructuring-bind (entries branch head (&optional ahead behind) stashes (operation merge-message)) result
                   (setf *git-ahead* ahead *git-behind* behind *git-stash-list* stashes
                         *git-operation* operation *git-merge-message* merge-message)
                   (let ((moved (and *git-head* head (string/= head *git-head*))))
                     (setf *git-entries* entries *git-branch* branch *git-head* head)
                     (clrhash *git-status*)
                     (dolist (e entries)
                       (destructuring-bind (index worktree path original) e
                         (declare (ignore original))
                         (let ((full (merge-pathnames path root)))
                           (setf (gethash (namestring full) *git-status*)
                                 (list (git-status-kind index worktree) (status-letter index worktree)))
                           ;; Folders above a change get a dot.
                           (loop for dir = (uiop:pathname-directory-pathname full)
                                   then (uiop:pathname-parent-directory-pathname dir)
                                 while (and (uiop:subpathp dir root) (not (equal (namestring dir) (namestring root))))
                                 do (unless (gethash (namestring dir) *git-status*)
                                      (setf (gethash (namestring dir) *git-status*) (list :folder "•")))))))
                     (when moved (reload-git-heads root))
                     (update-branch-label)
                     (redecorate-explorer)
                     (refresh-source-control))))
               (lambda (error)
                 (when *window* (panel-log (window-panel *window*) (format nil "Git status failed: ~a" error)))))))

(defun update-branch-label ()
  (let ((button (and *window* (gethash :status-branch *named-widgets*))))
    (when button
      (gtk:widget-set-visible button (and *git-branch* t))
      (when *git-branch*
        (gtk:label-set-text (gethash :status-branch-label *named-widgets*)
                            (format nil "~a~@[ ~a~]~:[~; ●~]" *git-branch* (sync-string) *git-entries*))))))

(defun sync-string ()
  "\"↑2 ↓1\" for the commits to push and pull, nil if none (or no upstream)."
  (cond (*git-busy* (format nil "~a…" *git-busy*))
        ((and *git-ahead* (or (plusp *git-ahead*) (plusp *git-behind*)))
         (format nil "~:[~;↑~:*~d~]~:[~; ~]~:[~;↓~:*~d~]"
                 (and (plusp *git-ahead*) *git-ahead*) (and (plusp *git-ahead*) (plusp *git-behind*))
                 (and (plusp *git-behind*) *git-behind*)))))

(defvar *explorer-rows* (make-hash-table :test 'eq) "Explorer row widget → its path, for redrawing status.")

(defun decorate-explorer-row (expander path)
  "Color the explorer row for PATH by its Git status."
  (setf (gethash expander *explorer-rows*) path)
  (let* ((box (gtk:tree-expander-get-child expander))
         (label (gtk:widget-get-next-sibling (gtk:widget-get-first-child box)))
         (status (gtk:widget-get-next-sibling label))
         (entry (git-status-of path)))
    (dolist (class '("cadre-git-modified" "cadre-git-added" "cadre-git-untracked" "cadre-git-deleted"
                     "cadre-git-conflict" "cadre-git-renamed" "cadre-git-folder"))
      (gtk:widget-remove-css-class label class)
      (gtk:widget-remove-css-class status class))
    (if entry
        (let ((class (format nil "cadre-git-~(~a~)" (first entry))))
          (gtk:widget-add-css-class label class)
          (gtk:widget-add-css-class status class)
          (gtk:label-set-text status (second entry)))
        (gtk:label-set-text status ""))))

(defun redecorate-explorer ()
  (loop for expander being the hash-keys of *explorer-rows* using (hash-value path)
        do (decorate-explorer-row expander path)))

;;; The Source Control page

(defstruct (source-control (:conc-name sc-))
  widget branch message staged changes not-repo body sync stashes conflicts banner)

(defvar *source-control* nil)
(defvar *sc-openers* (make-hash-table :test 'eq) "Source Control row → what clicking it opens.")

(defun sc-section-title (text count)
  (make-instance 'gtk:label :label (format nil "~a (~d)" text count) :xalign 0.0 :margin-start 10 :margin-top 8
                            :css-classes '("caption-heading" "dim-label")))

(defun sc-file-row (root entry staged)
  (destructuring-bind (index worktree path original) entry
    (declare (ignore original))
    (let* ((kind (git-status-kind index worktree))
           (name (make-instance 'gtk:label :label (file-namestring path) :xalign 0.0 :ellipsize :middle
                                           :css-classes (list (format nil "cadre-git-~(~a~)" kind))))
           (folder (make-instance 'gtk:label :label (let ((d (directory-namestring path))) (string-right-trim "/" d))
                                             :xalign 0.0 :hexpand t :ellipsize :start :css-classes '("dim-label" "caption")))
           (letter (make-instance 'gtk:label :label (if staged (string index) (status-letter index worktree))
                                             :css-classes (list (format nil "cadre-git-~(~a~)" kind))))
           (buttons (make-instance 'gtk:box :spacing 0))
           (row (make-instance 'gtk:list-box-row
                               :child (gtk:build (gtk:box :spacing 6 :margin-start 10 :margin-end 6
                                                   name folder buttons letter)))))
      (flet ((button (text tooltip action)
               (let ((b (make-instance 'gtk:button :label text :tooltip-text tooltip :css-classes '("flat" "cadre-sc-button"))))
                 (gobject:connect b :clicked (lambda (x) (declare (ignore x)) (funcall action)))
                 (gtk:box-append buttons b))))
        (if staged
            (button "−" "Unstage" (lambda () (git-run root (lambda () (git-unstage root (list path))))))
            (progn
              (button "↶" "Discard changes" (lambda () (confirm-discard root entry)))
              (button "+" "Stage" (lambda () (git-run root (lambda () (git-stage root (list path)))))))))
      (values row (lambda ()
                    (let ((file (merge-pathnames path root)))
                      (cond (staged (open-git-diff root path :kind :staged))
                            ((or (char= index #\?) (eq kind :conflict)) (open-file-path file))
                            (t (open-git-diff root path :kind :worktree)))))))))

(defun git-run (root thunk &optional done)
  "Run THUNK (which runs git) on a thread, then refresh everything."
  (git-async thunk (lambda (result)
                     (git-changed root)
                     (when done (funcall done result)))))

(defun confirm-discard (root entry)
  (destructuring-bind (index worktree path original) entry
    (declare (ignore worktree original))
    (let* ((untracked (char= index #\?))
           (dialog (adw:alert-dialog-new (format nil "Discard changes to ~a?" (file-namestring path))
                                         (if untracked
                                             "It isn't in Git yet, so it moves to the Trash."
                                             "Its unstaged changes are lost; it goes back to how it was staged or committed."))))
      (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
      (adw:alert-dialog-add-response dialog "discard" (if untracked "_Move to Trash" "_Discard"))
      (adw:alert-dialog-set-response-appearance dialog "discard" :destructive)
      (adw:alert-dialog-set-default-response dialog "cancel")
      (adw:alert-dialog-set-close-response dialog "cancel")
      (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                 (lambda (response)
                   (when (string= response "discard")
                     (if untracked
                         (progn (trash-path (merge-pathnames path root)) (git-changed root))
                         (git-run root (lambda () (git-discard root (list path)))))))))))

(defun make-source-control-widget ()
  (let* ((branch (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :max-width-chars 18))
         (sync (make-instance 'gtk:label :xalign 0.0 :hexpand t :css-classes '("dim-label" "caption")))
         (message-view (make-instance 'gtk:text-view :wrap-mode :word-char :top-margin 6 :bottom-margin 6
                                                     :left-margin 6 :right-margin 6 :accepts-tab nil
                                                     :css-classes '("cadre-commit-message")))
         (commit (make-instance 'gtk:button :label "Commit" :css-classes '("suggested-action")
                                            :tooltip-text "Commit the staged changes (Ctrl+Enter in the message)"))
         (staged (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (changes (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (stashes (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (conflicts (make-instance 'gtk:list-box :selection-mode :none :css-classes '("navigation-sidebar")))
         (banner (make-instance 'adw:bin))
         (not-repo (gtk:build
                     (gtk:box :orientation :vertical :spacing 8 :margin-start 12 :margin-end 12 :margin-top 12
                       (gtk:label :label "This folder isn't a Git repository." :xalign 0.0 :wrap t)
                       (gtk:button :label "Initialize Repository" :halign :start
                                   :on-clicked (lambda (b) (declare (ignore b))
                                                 (let ((project (window-project *window*)))
                                                   (when project
                                                     (git-async (lambda () (git-ok project "init" "-q"))
                                                                (lambda (r) (declare (ignore r))
                                                                  (clrhash *git-roots*)
                                                                  (git-changed)
                                                                  (dolist (b (buffer-list)) (git-attach b)))))))))))
         (body (gtk:build
                 (gtk:box :orientation :vertical :spacing 4
                   banner
                   (gtk:box :margin-start 8 :margin-end 8 :margin-top 4 :orientation :vertical :spacing 4
                     (gtk:label :label "Message (Ctrl+Enter commits)" :xalign 0.0
                                :css-classes '("dim-label" "caption"))
                     (gtk:frame :child message-view :height-request 60)
                     (gtk:box :spacing 6
                       (gtk:box :hexpand t :orientation :vertical commit)
                       (gtk:button :label "Stash" :tooltip-text "Put your uncommitted changes aside"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'stash-changes)))))
                   (gtk:scrolled-window :vexpand t :hscrollbar-policy :never
                                        :child (gtk:build (gtk:box :orientation :vertical conflicts staged changes stashes))))))
         (widget (gtk:build
                   (gtk:box :orientation :vertical
                     (gtk:box :margin-top 8 :margin-end 6
                       (gtk:label :label "SOURCE CONTROL" :xalign 0.0 :hexpand t :margin-start 12
                                  :css-classes '("caption-heading" "dim-label"))
                       (gtk:button :label "↻" :tooltip-text "Refresh" :css-classes '("flat")
                                   :on-clicked (lambda (b) (declare (ignore b)) (git-changed))))
                     ;; The branch and how it stands against its upstream, then the remote's buttons.
                     (gtk:box :spacing 4 :margin-start 8 :margin-end 6 :margin-top 2
                       (gtk:button :css-classes '("flat") :tooltip-text "Switch branch"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'switch-branch))
                         (gtk:box :spacing 4 (gtk:image :icon-name "cadre-git-symbolic") branch))
                       sync)
                     (gtk:box :spacing 4 :margin-start 8 :margin-end 8 :homogeneous t
                       (gtk:button :label "Fetch" :css-classes '("caption") :tooltip-text "Fetch from the remote"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'fetch-changes)))
                       (gtk:button :label "Pull" :css-classes '("caption") :tooltip-text "Pull the upstream's commits"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'pull-changes)))
                       (gtk:button :label "Push" :css-classes '("caption") :tooltip-text "Push your commits"
                                   :on-clicked (lambda (b) (declare (ignore b)) (call-command 'push-changes))))
                     not-repo
                     body))))
    (setf *source-control* (make-source-control :widget widget :branch branch :sync sync :stashes stashes
                                                :conflicts conflicts :banner banner
                                                :message message-view
                                                :staged staged :changes changes :not-repo not-repo :body body))
    (gobject:connect commit :clicked (lambda (b) (declare (ignore b)) (commit-from-page)))
    (dolist (list (list staged changes conflicts))
      (gtk:list-box-set-activate-on-single-click list t)
      (gobject:connect list :row-activated (lambda (lb row) (declare (ignore lb))
                                             (let ((open (gethash row *sc-openers*))) (when open (funcall open))))))
    (let ((keys (gtk:event-controller-key-new)))
      (gobject:connect keys :key-pressed
                       (lambda (c keyval keycode state)
                         (declare (ignore c keycode))
                         (when (and (member (gdk:keyval-name keyval) '("Return" "KP_Enter") :test #'string=)
                                    (intersection (modifier-list state) '(:control-mask :super-mask :meta-mask)))
                           (commit-from-page)
                           t)))
      (gtk:widget-add-controller message-view keys))
    (gtk:widget-set-visible body nil)
    widget))

(defun fill-change-list (list root title entries staged &key stage-all)
  (loop for row = (gtk:list-box-get-row-at-index list 0) then (gtk:widget-get-next-sibling row)
        while row do (remhash row *sc-openers*))
  (gtk:list-box-remove-all list)
  (let ((header (make-instance 'gtk:list-box-row :activatable nil :selectable nil)))
    (gtk:list-box-row-set-child
     header (if stage-all
                (gtk:build (gtk:box (sc-section-title title (length entries))
                             (gtk:box :hexpand t)
                             (gtk:button :label (if staged "Unstage All" "Stage All") :css-classes '("flat" "caption")
                                         :margin-end 6 :sensitive (and entries t)
                                         :on-clicked (lambda (b) (declare (ignore b))
                                                       (git-run root (lambda ()
                                                                       (if staged
                                                                           (git-unstage root (mapcar #'third entries))
                                                                           (git-ok root "add" "-A"))))))))
                (sc-section-title title (length entries))))
    (gtk:list-box-append list header))
  (dolist (entry entries)
    (multiple-value-bind (row open) (sc-file-row root entry staged)
      (setf (gethash row *sc-openers*) open)
      (gtk:list-box-append list row))))

(defun refresh-source-control ()
  (let ((sc *source-control*)
        (root (project-git-root)))
    (when sc
      (gtk:widget-set-visible (sc-not-repo sc) (and (window-project *window*) (null root)))
      (gtk:widget-set-visible (sc-body sc) (and root t))
      (gtk:label-set-text (sc-branch sc) (or (and root *git-branch*) ""))
      (gtk:label-set-text (sc-sync sc) (cond ((null root) "")
                                             ((sync-string))
                                             ((null *git-ahead*) "no upstream")
                                             (t "up to date")))
      (when root
        (let* ((conflicted (remove-if-not (lambda (e) (eq :conflict (git-status-kind (first e) (second e)))) *git-entries*))
               (others (remove-if (lambda (e) (member e conflicted)) *git-entries*))
               (staged (remove-if (lambda (e) (member (first e) '(#\Space #\?))) others))
               (changes (remove-if (lambda (e) (char= (second e) #\Space)) others)))
          (adw:bin-set-child (sc-banner sc) (and *git-operation* (operation-banner root *git-operation*)))
          (if conflicted
              (fill-change-list (sc-conflicts sc) root "Merge Conflicts" conflicted nil)
              (gtk:list-box-remove-all (sc-conflicts sc)))
          (fill-change-list (sc-staged sc) root "Staged Changes" staged t :stage-all t)
          (fill-change-list (sc-changes sc) root "Changes" changes nil :stage-all t)
          ;; While merging, the message git prepared is the commit's.
          (let ((input (gtk:text-view-get-buffer (sc-message sc))))
            (when (and *git-merge-message* (string= "" (text-string input)))
              (text-replace-contents input *git-merge-message*)))
          (fill-stash-list (sc-stashes sc) root))))))

(defun commit-from-page ()
  (let* ((sc *source-control*)
         (root (or (project-git-root) (editor-error "Not a Git repository")))
         (input (gtk:text-view-get-buffer (sc-message sc)))
         (message (string-trim '(#\Space #\Newline #\Tab) (text-string input)))
         (staged (remove-if (lambda (e) (member (first e) '(#\Space #\?))) *git-entries*)))
    (flet ((commit ()
             (git-run root (lambda () (git-commit root message))
                      (lambda (output)
                        (declare (ignore output))
                        (text-replace-contents input "")
                        (message "Committed: ~a" (first (split-text-lines message)))))))
      (cond ((string= message "") (message "Write a commit message first"))
            (staged (commit))
            ((null *git-entries*) (message "Nothing to commit"))
            (t (let ((dialog (adw:alert-dialog-new "Stage all changes and commit?"
                                                   "Nothing is staged. Cadre can stage every change, including new files, and commit them.")))
                 (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
                 (adw:alert-dialog-add-response dialog "all" "_Stage All and Commit")
                 (adw:alert-dialog-set-response-appearance dialog "all" :suggested)
                 (adw:alert-dialog-set-close-response dialog "cancel")
                 (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                            (lambda (response)
                              (when (string= response "all")
                                (git-run root (lambda () (git-ok root "add" "-A") (git-commit root message))
                                         (lambda (output)
                                           (declare (ignore output))
                                           (text-replace-contents input "")
                                           (message "Committed: ~a" (first (split-text-lines message))))))))))))))

(define-command show-source-control ()
  "Show the Source Control page: changed files, staging and committing."
  (show-sidebar-page *window* "git" :toggle nil)
  (git-changed)
  (when *source-control* (gtk:widget-grab-focus (sc-message *source-control*))))

(defun git-project-opened ()
  (clrhash *git-roots*)
  (setf *git-head* nil *git-branch* nil *git-entries* nil)
  (clrhash *git-status*)
  (git-changed)
  (update-branch-label))

(add-hook '*after-save-hook* (lambda (buffer)
                               (declare (ignore buffer))
                               (git-changed)))

;;; Branches

(defun git-root-or-error ()
  (or (project-git-root) (editor-error "This folder isn't a Git repository")))

(defun after-head-moved (root)
  "After switching or pulling: HEAD and the files may have changed."
  (git-changed root))

(define-command switch-branch ()
  "Switch to another branch (or make a new one)."
  (let ((root (git-root-or-error)))
    (git-async (lambda () (git-branches root))
               (lambda (branches)
                 (let* ((locals (remove-if (lambda (b) (getf b :remote)) branches))
                        (local-names (mapcar (lambda (b) (getf b :name)) locals))
                        ;; Remote branches with no local one of the same name.
                        (remotes (remove-if (lambda (b)
                                              (or (not (getf b :remote))
                                                  (member (subseq (getf b :name) (1+ (or (position #\/ (getf b :name)) -1)))
                                                          local-names :test #'string=)))
                                            branches))
                        (items (append (list (list :new t)) locals remotes)))
                   (open-picker (window-picker *window*)
                                :items items
                                :label (lambda (b) (cond ((getf b :new) "+ New branch…")
                                                         ((getf b :current) (format nil "~a  (current)" (getf b :name)))
                                                         (t (getf b :name))))
                                :detail (lambda (b) (cond ((getf b :new) "from here")
                                                          ((getf b :remote) "remote")
                                                          ((getf b :upstream) (format nil "tracks ~a" (getf b :upstream)))
                                                          (t "")))
                                :placeholder "Switch to a branch"
                                :on-choose (lambda (b)
                                             (cond ((getf b :new) (call-command 'create-branch))
                                                   ((getf b :current))
                                                   (t (switch-to-branch root (getf b :name) :track (getf b :remote)))))))))))

(defun switch-to-branch (root name &key create track)
  (git-run root (lambda () (git-switch root name :create create :track track))
           (lambda (output)
             (declare (ignore output))
             (after-head-moved root)
             (message "On branch ~a" (if track (subseq name (1+ (or (position #\/ name) -1))) name)))))

(define-command create-branch ()
  "Make a new branch from here and switch to it."
  (let ((root (git-root-or-error)))
    (ask-name "New branch name:"
              (lambda (name)
                (when (find-if (lambda (c) (member c '(#\Space #\~ #\^ #\: #\? #\* #\[ #\\))) name)
                  (editor-error "A branch name can't contain spaces or ~~^:?*[\\"))
                (switch-to-branch root name :create t)))))

(define-command delete-branch ()
  "Delete a local branch (asking first; again if it isn't merged)."
  (let ((root (git-root-or-error)))
    (git-async (lambda () (git-branches root))
               (lambda (branches)
                 (let ((deletable (remove-if (lambda (b) (or (getf b :remote) (getf b :current))) branches)))
                   (unless deletable (editor-error "No other local branches"))
                   (open-picker (window-picker *window*)
                                :items deletable
                                :label (lambda (b) (getf b :name))
                                :placeholder "Delete a branch"
                                :on-choose (lambda (b) (confirm-delete-branch root (getf b :name)))))))))

(defun ask-yes (heading body yes-label then)
  "Ask HEADING; call THEN if the answer is YES-LABEL (a destructive choice)."
  (let ((dialog (adw:alert-dialog-new heading body)))
    (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
    (adw:alert-dialog-add-response dialog "yes" yes-label)
    (adw:alert-dialog-set-response-appearance dialog "yes" :destructive)
    (adw:alert-dialog-set-default-response dialog "cancel")
    (adw:alert-dialog-set-close-response dialog "cancel")
    (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
               (lambda (response) (when (string= response "yes") (funcall then))))))

(defun confirm-delete-branch (root name)
  (ask-yes (format nil "Delete branch ~a?" name) "Its commits stay if another branch has them."
           "_Delete"
           (lambda ()
             (git-async (lambda () (git-delete-branch root name))
                        (lambda (r) (declare (ignore r)) (message "Deleted branch ~a" name) (git-changed root))
                        (lambda (error)
                          (if (search "not fully merged" error)
                              (ask-yes (format nil "~a isn't merged" name)
                                       "Its commits aren't on any other branch, so deleting it loses them."
                                       "_Delete Anyway"
                                       (lambda ()
                                         (git-run root (lambda () (git-delete-branch root name :force t))
                                                  (lambda (r) (declare (ignore r)) (message "Deleted branch ~a" name)))))
                              (message "Git: ~a" error)))))))

;;; Fetch, pull, push

(defun remote-operation (root label thunk done)
  "Run THUNK, which talks to a remote, showing LABEL (\"Pushing\") meanwhile."
  (when *git-busy* (editor-error "Git is still ~(~a~)" *git-busy*))
  (setf *git-busy* label)
  (update-branch-label)
  (refresh-source-control)
  (message "~a…" label)
  (flet ((finish () (setf *git-busy* nil) (git-changed root)))
    (git-async thunk
               (lambda (output) (finish) (funcall done output))
               (lambda (error)
                 (finish)
                 (message "~a failed: ~a" label
                          (if (or (search "could not read Username" error) (search "Permission denied" error)
                                  (search "terminal prompts disabled" error))
                              (format nil "~a (Cadre doesn't ask for passwords: set up a credential helper or ssh-agent)"
                                      (first (split-text-lines error)))
                              error))))))

(define-command fetch-changes ()
  "Fetch the remotes' branches (without changing yours)."
  (let ((root (git-root-or-error)))
    (remote-operation root "Fetching" (lambda () (git-fetch root))
                      (lambda (output) (declare (ignore output)) (message "Fetched")))))

(define-command pull-changes ()
  "Bring the upstream's commits into this branch (see *git-pull-mode*)."
  (let ((root (git-root-or-error)))
    (remote-operation root "Pulling" (lambda () (git-pull root :mode *git-pull-mode*))
                      (lambda (output)
                        (message "~a" (if (search "Already up to date" output) "Already up to date" "Pulled"))))))

(define-command push-changes ()
  "Push this branch's commits to its upstream (setting one up the first time)."
  (let ((root (git-root-or-error)))
    (git-async (lambda () (list (git-upstream root) (git-remotes root)))
               (lambda (found)
                 (destructuring-bind (upstream remotes) found
                   (flet ((push-to (remote)
                            (remote-operation root "Pushing" (lambda () (git-push root :set-upstream remote))
                                              (lambda (output) (declare (ignore output))
                                                (message "Pushed~@[ to ~a~]" remote)))))
                     (cond (upstream (push-to nil))
                           ((null remotes) (message "No remote to push to: add one with git remote add"))
                           ((or (null (rest remotes)) (member "origin" remotes :test #'string=))
                            (push-to (if (member "origin" remotes :test #'string=) "origin" (first remotes))))
                           (t (open-picker (window-picker *window*)
                                           :items remotes :placeholder "Push to which remote?"
                                           :on-choose #'push-to)))))))))
