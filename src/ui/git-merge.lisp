;;;; git-merge.lisp — staging one change at a time, and resolving merge
;;;; conflicts
;;;;
;;;; A change can be staged from its gutter mark, from the cursor, or in a
;;;; diff view: diffs of a file's unstaged or staged changes are live, so s
;;;; stages the change at the cursor, u unstages it, x discards it, n and p
;;;; move between changes, and g reads the diff again.
;;;;
;;;; In a file with conflict markers, each conflict is colored (yours,
;;;; theirs, the base) and has buttons above it to keep your side, theirs,
;;;; or both. The Source Control page lists conflicted files and, while a
;;;; merge or rebase is under way, offers to abort or go on with it.

(in-package #:cadre-ui)

;;; Staging from the editor

(defun stage-hunk-lines (view first last)
  "Stage the saved changes of VIEW's file on lines FIRST to LAST (from 1)."
  (let ((gf (current-git-file view)))
    (when (buffer-modified-p (view-buffer view))
      (editor-error "Save ~a first: only saved changes can be staged" (buffer-name (view-buffer view))))
    (git-async (lambda () (git-stage-lines (gf-root gf) (gf-relative gf) first last))
               (lambda (count)
                 (message (if (zerop count) "No unstaged change there" "Staged ~d change~:p") count)
                 (git-changed (gf-root gf))))))

(defun stage-hunk (view hunk)
  (multiple-value-bind (first last) (hunk-lines hunk)
    (stage-hunk-lines view (1+ first) (1+ last))))

(define-command git-stage-change ()
  "Stage the change at the cursor (or the selected lines' changes)."
  (let ((view (current-view)))
    (multiple-value-bind (first last) (selected-lines view)
      (stage-hunk-lines view (1+ first) (1+ last)))))

;;; Live diffs

(define-major-mode git-diff-mode (:title "Diff")
  "A file's changes, as a unified diff: s stages the change at the cursor,
u unstages it, x discards it, n and p move between changes, g refreshes.")

(defun diff-hunk-at-cursor (view)
  "The hunk of VIEW's diff the cursor is in, the file header, and the hunk's line."
  (let* ((text (buffer-string (view-buffer view)))
         (lines (split-text-lines text))
         (cursor (cursor-line-column view))
         (starts (loop for line in lines for n from 0
                       when (and (> (length line) 2) (string= "@@" line :end2 2)) collect n))
         (index (position-if (lambda (s) (<= s cursor)) starts :from-end t)))
    (unless index (editor-error "Put the cursor in a change (below an @@ line)"))
    (values (nth index (diff-hunks text)) (diff-file-header text) (nth index starts))))

(defun diff-kind (buffer) (buffer-local buffer :diff-kind))

(defun refresh-diff-buffer (buffer)
  "Read BUFFER's diff again, keeping the cursor near where it was."
  (let ((root (buffer-local buffer :diff-root))
        (relative (buffer-local buffer :diff-relative))
        (kind (diff-kind buffer)))
    (git-async (lambda () (git-diff-text root relative :staged (eq kind :staged) :head (eq kind :head)))
               (lambda (text)
                 (when (member buffer (buffer-list))
                   (let* ((view (first (buffer-views *window* buffer)))
                          (line (and view (cursor-line-column view))))
                     (fill-diff-buffer buffer (if (string= text "") "No changes." text))
                     (when view
                       (goto-line-column view (min (or line 0) (1- (gtk:text-buffer-get-line-count (buffer-text buffer)))) 0))))))))

(defun diff-hunk-command (operation)
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (root (buffer-local buffer :diff-root))
         (kind (diff-kind buffer)))
    (unless root (editor-error "Not a diff of a file"))
    (when (eq kind :head)
      (editor-error "This diff is against the last commit; open the file's changes from Source Control to stage parts of them"))
    (multiple-value-bind (hunk header) (diff-hunk-at-cursor view)
      (let ((patch (hunk-patch header hunk)))
        (flet ((apply-it (&rest options)
                 (git-async (lambda () (apply #'git-apply-patch root patch options))
                            (lambda (r) (declare (ignore r))
                              (refresh-diff-buffer buffer)
                              (git-changed root)
                              (dolist (b (buffer-list)) (when (buffer-git b) (schedule-git-hunks b)))))))
          (ecase operation
            (:stage (if (eq kind :staged) (message "Already staged; u unstages it") (apply-it :cached t)))
            (:unstage (if (eq kind :staged) (apply-it :cached t :reverse t) (message "Not staged; s stages it")))
            (:discard (if (eq kind :staged)
                          (message "Unstage it first (u), then discard it")
                          (ask-yes "Discard this change?" "The file goes back to how it was before this change."
                                   "_Discard" (lambda () (apply-it :reverse t)))))))))))

(define-command diff-stage-hunk () "Stage the change at the cursor." (:modes git-diff-mode) (diff-hunk-command :stage))
(define-command diff-unstage-hunk () "Unstage the change at the cursor." (:modes git-diff-mode) (diff-hunk-command :unstage))
(define-command diff-discard-hunk () "Discard the change at the cursor (asking first)." (:modes git-diff-mode) (diff-hunk-command :discard))

(defun diff-move (direction)
  (let* ((view (current-view))
         (cursor (cursor-line-column view))
         (starts (loop for line in (split-text-lines (buffer-string (view-buffer view))) for n from 0
                       when (and (> (length line) 2) (string= "@@" line :end2 2)) collect n))
         (target (if (plusp direction)
                     (find-if (lambda (s) (> s cursor)) starts)
                     (find-if (lambda (s) (< s cursor)) starts :from-end t))))
    (if target (goto-line-column view target 0) (message "No ~:[earlier~;more~] changes" (plusp direction)))))

(define-command diff-next-hunk () "Go to the next change." (:modes git-diff-mode) (diff-move 1))
(define-command diff-previous-hunk () "Go to the previous change." (:modes git-diff-mode) (diff-move -1))
(define-command diff-refresh () "Read the diff again." (:modes git-diff-mode) (refresh-diff-buffer (current-buffer)))

;;; Conflicts in a buffer

(defun conflict-tags (gtk-buffer)
  (list (ensure-face-tag gtk-buffer "cadre-conflict-ours" :conflict-ours)
        (ensure-face-tag gtk-buffer "cadre-conflict-theirs" :conflict-theirs)
        (ensure-face-tag gtk-buffer "cadre-conflict-base" :conflict-base)
        (ensure-face-tag gtk-buffer "cadre-conflict-marker" :conflict-marker)))

(defun line-span (gtk-buffer first last)
  "Iters from line FIRST's start to the start of the line after LAST."
  (values (line-iter gtk-buffer first)
          (let ((it (line-iter gtk-buffer last)))
            (if (gtk:text-iter-forward-line it) it (gtk:text-buffer-get-end-iter gtk-buffer)))))

(defun update-conflicts (buffer)
  "Color BUFFER's conflicts and put their buttons above them."
  (let* ((gtk-buffer (buffer-text buffer))
         (text (buffer-string buffer))
         (conflicts (and (search "<<<<<<<" text) (find-conflicts text))))
    (when (or conflicts (buffer-local buffer :conflicts))
      (destructuring-bind (ours theirs base marker) (conflict-tags gtk-buffer)
        (dolist (tag (list ours theirs base marker))
          (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                      (gtk:text-buffer-get-end-iter gtk-buffer)))
        (dolist (c conflicts)
          (destructuring-bind (&key start base middle end) c
            (flet ((tag (tag first last)
                     (when (<= first last)
                       (multiple-value-bind (s e) (line-span gtk-buffer first last)
                         (gtk:text-buffer-apply-tag gtk-buffer tag s e)))))
              (tag ours (1+ start) (1- (or base middle)))
              (when base (tag base (1+ base) (1- middle)))
              (tag theirs (1+ middle) (1- end))
              (dolist (l (remove nil (list start base middle end))) (tag marker l l)))))))
    (setf (buffer-local buffer :conflicts) conflicts)
    (when *window*
      (dolist (view (buffer-views *window* buffer)) (place-conflict-buttons view)))))

(defvar *conflict-buttons* (make-hash-table :test 'eq :weakness :key) "View → its conflict button boxes.")

(defun place-conflict-buttons (view)
  "Put Accept buttons at the end of each conflict's first line in VIEW."
  (let ((text-view (view-text-view view))
        (gtk-buffer (view-gtk-buffer view)))
    (dolist (w (gethash view *conflict-buttons*))
      (gtk:widget-set-visible w nil)
      (ignore-errors (gtk:text-view-remove text-view w)))
    (setf (gethash view *conflict-buttons*)
          (loop for c in (buffer-local (view-buffer view) :conflicts)
                for i from 0
                collect (let* ((start (getf c :start))
                               (rect (gtk:text-view-get-iter-location text-view (line-end-iter gtk-buffer start)))
                               (box (make-instance 'gtk:box :spacing 2 :css-classes '("cadre-conflict-actions"))))
                          (flet ((button (label choice tooltip)
                                   (let ((b (make-instance 'gtk:button :label label :tooltip-text tooltip
                                                                       :css-classes '("flat" "caption"))))
                                     (gobject:connect b :clicked (lambda (x) (declare (ignore x))
                                                                   (resolve-merge-conflict view i choice)))
                                     (gtk:box-append box b))))
                            (button "Accept Current" :ours "Keep your side (above =======)")
                            (button "Accept Incoming" :theirs "Keep their side (below =======)")
                            (button "Accept Both" :both "Keep yours, then theirs"))
                          ;; On the <<<<<<< line, after its text (the buttons are a little taller than a line).
                          (gtk:text-view-add-overlay text-view box (+ (gdk:rectangle-x rect) 24)
                                                     (max 0 (- (gdk:rectangle-y rect) 3)))
                          box)))))

(defun resolve-merge-conflict (view index choice)
  "Replace conflict INDEX in VIEW's buffer with CHOICE's lines (:ours, :theirs, :both)."
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (conflict (or (nth index (buffer-local buffer :conflicts)) (editor-error "That conflict is gone")))
         (lines (coerce (split-text-lines (buffer-string buffer)) 'vector))
         (keep (conflict-resolution lines conflict choice)))
    (multiple-value-bind (s e) (line-span gtk-buffer (getf conflict :start) (getf conflict :end))
      (let ((start (gtk:text-iter-get-offset s))
            (end (gtk:text-iter-get-offset e))
            (at-end (gtk:text-iter-is-end e)))
        (with-user-action (gtk-buffer)
          (replace-text-between gtk-buffer start end
                                (format nil (if at-end "~{~a~^~%~}" "~{~a~%~}") keep)))
        (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer start))))
    (update-conflicts buffer)
    (if (buffer-local buffer :conflicts)
        (message "~d conflict~:p left" (length (buffer-local buffer :conflicts)))
        (message "No conflicts left in ~a: save it, then stage it to mark it resolved" (buffer-name buffer)))))

(defun conflict-at-cursor (view)
  (let ((line (cursor-line-column view))
        (conflicts (buffer-local (view-buffer view) :conflicts)))
    (or (position-if (lambda (c) (<= (getf c :start) line (getf c :end))) conflicts)
        (position-if (lambda (c) (> (getf c :start) line)) conflicts)
        (and conflicts 0)
        (editor-error "No conflicts here"))))

(define-command accept-current-change ()
  "Resolve the conflict at the cursor by keeping your side."
  (let ((view (current-view))) (resolve-merge-conflict view (conflict-at-cursor view) :ours)))

(define-command accept-incoming-change ()
  "Resolve the conflict at the cursor by keeping their side."
  (let ((view (current-view))) (resolve-merge-conflict view (conflict-at-cursor view) :theirs)))

(define-command accept-both-changes ()
  "Resolve the conflict at the cursor by keeping both sides, yours first."
  (let ((view (current-view))) (resolve-merge-conflict view (conflict-at-cursor view) :both)))

(defun conflict-move (direction)
  (let* ((view (current-view))
         (line (cursor-line-column view))
         (starts (mapcar (lambda (c) (getf c :start)) (buffer-local (view-buffer view) :conflicts)))
         (target (if (plusp direction)
                     (or (find-if (lambda (s) (> s line)) starts) (first starts))
                     (or (find-if (lambda (s) (< s line)) starts :from-end t) (car (last starts))))))
    (if target (goto-line-column view target 0) (message "No conflicts in this file"))))

(define-command next-conflict () "Go to the next merge conflict in this file." (conflict-move 1))
(define-command previous-conflict () "Go to the previous merge conflict in this file." (conflict-move -1))

(defun schedule-conflicts (buffer)
  (let ((timer (buffer-local buffer :conflict-timer)))
    (when timer (glib:source-remove timer))
    (setf (buffer-local buffer :conflict-timer)
          (glib:timeout-add glib:+priority-default+ 300
                            (lambda ()
                              (setf (buffer-local buffer :conflict-timer) nil)
                              (when (member buffer (buffer-list)) (update-conflicts buffer))
                              nil)))))

(defun watch-conflicts (buffer)
  (when (and (buffer-file buffer) (typep (buffer-text buffer) 'gtk:text-buffer))
    (gobject:connect (buffer-text buffer) :changed
                     (lambda (b) (declare (ignore b)) (schedule-conflicts buffer)))
    (schedule-conflicts buffer)))

(add-hook '*buffer-created-hook* 'watch-conflicts)

;;; Merging a branch, and what's under way

(define-command merge-branch ()
  "Merge another branch into this one."
  (let ((root (git-root-or-error)))
    (git-async (lambda () (git-branches root))
               (lambda (branches)
                 (let ((others (remove-if (lambda (b) (getf b :current)) branches)))
                   (unless others (editor-error "No other branches"))
                   (open-picker (window-picker *window*)
                                :items others :label (lambda (b) (getf b :name))
                                :detail (lambda (b) (if (getf b :remote) "remote" ""))
                                :placeholder (format nil "Merge which branch into ~a?" *git-branch*)
                                :on-choose (lambda (b) (merge-into-current root (getf b :name)))))))))

(defun merge-into-current (root branch)
  (git-async (lambda () (git-merge root branch))
             (lambda (output)
               (declare (ignore output))
               (message "Merged ~a" branch)
               (git-changed root))
             (lambda (error)
               (git-changed root)
               (if (search "CONFLICT" error)
                   (progn (show-sidebar-page *window* "git" :toggle nil)
                          (message "Merging ~a: resolve the conflicts, stage the files, and commit" branch))
                   (message "Git: ~a" error)))))

(defun operation-name (operation)
  (ecase operation (:merge "Merging") (:rebase "Rebasing") (:cherry-pick "Cherry-picking") (:revert "Reverting")))

(defun operation-banner (root operation)
  "The banner for OPERATION under way in ROOT: Abort and Continue."
  (gtk:build
    (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 4 :css-classes '("cadre-operation-banner")
      (gtk:label :label (operation-name operation) :xalign 0.0 :hexpand t :css-classes '("heading"))
      (gtk:button :label "Abort" :css-classes '("destructive-action" "caption")
                  :on-clicked (lambda (b) (declare (ignore b))
                                (ask-yes (format nil "Abort ~(~a~)?" (operation-name operation))
                                         "Everything goes back to how it was before it started."
                                         "_Abort"
                                         (lambda () (git-run root (lambda () (git-abort root operation))
                                                             (lambda (r) (declare (ignore r)) (message "Aborted")))))))
      (gtk:button :label (if (eq operation :merge) "Commit" "Continue") :css-classes '("suggested-action" "caption")
                  :on-clicked (lambda (b) (declare (ignore b))
                                (if (find :conflict *git-entries* :key (lambda (e) (git-status-kind (first e) (second e))))
                                    (message "Resolve and stage every conflicted file first")
                                    (git-run root (lambda () (git-continue root operation))
                                             (lambda (r) (declare (ignore r)) (message "Done")))))))))
