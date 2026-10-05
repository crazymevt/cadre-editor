;;;; editing.lisp — Emacs editing: the kill ring, the mark, and the commands
;;;; an Emacs user's fingers expect
;;;;
;;;; The kill ring (core editing.lisp) and the system clipboard stay in step:
;;;; each kill is copied to the clipboard, and text copied anywhere else (in
;;;; another application, or with Ctrl+C) joins the ring, so C-y and M-y
;;;; reach it.
;;;;
;;;; The mark is a GtkTextMark, "cadre-mark", in each buffer. While the mark
;;;; is active (after C-SPC) movement extends the selection, as in Emacs's
;;;; transient-mark-mode. Commands that act on "the region" use the selection
;;;; if there is one, else the text between the mark and the cursor.

(in-package #:cadre-ui)

;;; Changing text, respecting read-only text (the REPL's output)

(defun insert-text-at (gtk-buffer offset string)
  "Insert STRING at OFFSET unless the text there is read-only. Returns t if inserted."
  (gtk:text-buffer-insert-interactive gtk-buffer (iter-at gtk-buffer offset) string -1 t))

(defun delete-text-between (gtk-buffer start end)
  "Delete from START to END (offsets) unless some of it is read-only. Returns t if deleted."
  (gtk:text-buffer-delete-interactive gtk-buffer (iter-at gtk-buffer (min start end))
                                      (iter-at gtk-buffer (max start end)) t))

(defun replace-text-between (gtk-buffer start end string)
  (with-user-action (gtk-buffer)
    (when (delete-text-between gtk-buffer start end)
      (insert-text-at gtk-buffer (min start end) string))))

(defun point-offset (view) (text-point (view-gtk-buffer view)))

(defun set-point (view offset)
  "Move VIEW's cursor to OFFSET (extending the selection while the mark is active)."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (if (buffer-local (view-buffer view) :mark-active)
        (gtk:text-buffer-move-mark gtk-buffer (gtk:text-buffer-get-insert gtk-buffer) (iter-at gtk-buffer offset))
        (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer offset)))
    (scroll-to-cursor view)))

;;; The clipboard

(defvar *clipboard-handler* nil)

(defun clipboard ()
  (gdk:display-get-clipboard (gdk:display-get-default)))

(defun copy-to-clipboard (string)
  (gdk:clipboard-set-text (clipboard) string))

(defun setup-clipboard ()
  "Keep the kill ring and the system clipboard in step."
  (add-hook '*kill-hook* 'copy-to-clipboard)
  (unless *clipboard-handler*
    (setf *clipboard-handler*
          (gobject:connect (clipboard) :changed
                           (lambda (cb &rest more)
                             (declare (ignore more))
                             (gio:async (gdk:clipboard-read-text-async cb)
                                        (lambda (text)
                                          (when (and (stringp text) (plusp (length text)))
                                            (kill-new text)))
                                        :error (lambda (e) (declare (ignore e)))))))))

;;; The mark

(defun buffer-mark (buffer)
  "BUFFER's mark, a GtkTextMark, or nil if it was never set."
  (gtk:text-buffer-get-mark (buffer-text buffer) "cadre-mark"))

(defun mark-offset (buffer)
  (let ((mark (buffer-mark buffer)))
    (and mark (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark (buffer-text buffer) mark)))))

(define-option *mark-ring-max* 16 (integer 1)
  "How many earlier marks each buffer remembers, for C-u C-SPC."
  :category "Editing")

(defun push-mark (buffer offset &key activate)
  "Set BUFFER's mark at OFFSET, remembering where it was before."
  (let* ((gtk-buffer (buffer-text buffer))
         (old (mark-offset buffer))
         (mark (buffer-mark buffer)))
    (when old
      (let ((ring (cons (gtk:text-buffer-create-mark gtk-buffer nil (iter-at gtk-buffer old) t)
                        (buffer-local buffer :mark-ring))))
        (when (> (length ring) *mark-ring-max*)
          (gtk:text-buffer-delete-mark gtk-buffer (car (last ring)))
          (setf ring (butlast ring)))
        (setf (buffer-local buffer :mark-ring) ring)))
    (if mark
        (gtk:text-buffer-move-mark gtk-buffer mark (iter-at gtk-buffer offset))
        (gtk:text-buffer-create-mark gtk-buffer "cadre-mark" (iter-at gtk-buffer offset) t))
    (when activate
      (setf (buffer-local buffer :mark-active) t)
      (gtk:text-buffer-move-mark gtk-buffer (gtk:text-buffer-get-selection-bound gtk-buffer)
                                 (iter-at gtk-buffer offset)))))

(defun deactivate-mark (view)
  (setf (buffer-local (view-buffer view) :mark-active) nil)
  (let ((gtk-buffer (view-gtk-buffer view)))
    (gtk:text-buffer-place-cursor gtk-buffer (cursor-iter gtk-buffer))))

(defun region-bounds (view &key (error t))
  "The region's start and end offsets: the selection if there is one, else
between the mark and the cursor."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (cond (has (values (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end)))
            ((mark-offset (view-buffer view))
             (let ((mark (mark-offset (view-buffer view)))
                   (point (point-offset view)))
               (values (min mark point) (max mark point))))
            (error (editor-error "The mark is not set"))
            (t nil)))))

(define-command set-mark ()
  "Set the mark here and start selecting: movement extends the selection
until C-g. With C-u, jump back to the previous mark instead."
  (let* ((view (current-view))
         (buffer (view-buffer view)))
    (if (consp *prefix-arg*)
        (pop-mark view)
        (progn (push-mark buffer (point-offset view) :activate t)
               (message "Mark set")))))

(defun pop-mark (view)
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (mark (mark-offset buffer))
         (ring (buffer-local buffer :mark-ring)))
    (unless mark (editor-error "No mark set in this buffer"))
    (setf (buffer-local buffer :mark-active) nil)
    (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer mark))
    (scroll-to-cursor view)
    ;; The mark ring turns: the mark becomes the oldest entry.
    (when ring
      (let ((next (first ring)))
        (gtk:text-buffer-move-mark gtk-buffer (buffer-mark buffer)
                                   (gtk:text-buffer-get-iter-at-mark gtk-buffer next))
        (gtk:text-buffer-move-mark gtk-buffer next (iter-at gtk-buffer mark))
        (setf (buffer-local buffer :mark-ring) (append (rest ring) (list next)))))))

(define-command exchange-point-and-mark ()
  "Put the cursor where the mark is and the mark where the cursor was, selecting the region."
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (mark (or (mark-offset buffer) (editor-error "No mark set in this buffer")))
         (point (point-offset view)))
    (gtk:text-buffer-move-mark gtk-buffer (buffer-mark buffer) (iter-at gtk-buffer point))
    (setf (buffer-local buffer :mark-active) t)
    (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer mark) (iter-at gtk-buffer point))
    (scroll-to-cursor view)))

(define-command mark-whole-buffer ()
  "Select the whole buffer, with the cursor at the start."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (push-mark (view-buffer view) (text-length gtk-buffer))
    (setf (buffer-local (view-buffer view) :mark-active) t)
    (gtk:text-buffer-select-range gtk-buffer (gtk:text-buffer-get-start-iter gtk-buffer)
                                  (gtk:text-buffer-get-end-iter gtk-buffer))))

;;; Killing and yanking

(defun kill-between (view start end &key (direction :forward))
  "Kill the text from START to END in VIEW: onto the kill ring, out of the buffer."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (start (min start end)) (end (max start end))
         (string (text-string gtk-buffer start end)))
    (when (< start end)
      (with-user-action (gtk-buffer)
        (if (delete-text-between gtk-buffer start end)
            (kill-text string :direction direction)
            (progn (kill-text string :direction direction)
                   (message "Text is read-only; copied it to the kill ring")))))))

(define-command kill-region ()
  "Kill (cut) the region: the selection, or the text between the mark and the cursor."
  (let ((view (current-view)))
    (multiple-value-bind (start end) (region-bounds view)
      (kill-between view start end)
      (setf (buffer-local (view-buffer view) :mark-active) nil))))

(define-command copy-region-as-kill ()
  "Copy the region to the kill ring (and the clipboard) without deleting it."
  (let ((view (current-view)))
    (multiple-value-bind (start end) (region-bounds view)
      (kill-new (text-string (view-gtk-buffer view) start end))
      (run-hook '*kill-hook* (first *kill-ring*))
      (deactivate-mark view)
      (message "Copied ~d character~:p" (- end start)))))

(defun line-kill-end (view)
  "Where C-k kills to from the cursor in VIEW."
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view))
         (syntax (buffer-syntax buffer)))
    (cond ((integerp *prefix-arg*)
           ;; C-u N C-k: N whole lines.
           (multiple-value-bind (line) (text-position-line gtk-buffer point)
             (let ((target (+ line *prefix-arg*)))
               (if (>= target (text-line-count gtk-buffer))
                   (text-length gtk-buffer)
                   (text-line-position gtk-buffer target)))))
          ((and syntax (minor-mode-enabled-p buffer 'paredit-mode))
           (multiple-value-bind (line column) (text-position-line gtk-buffer point)
             (paredit-kill-end syntax line column)))
          (t (let ((iter (cursor-iter gtk-buffer)))
               (if (gtk:text-iter-ends-line iter)
                   (gtk:text-iter-forward-char iter)
                   (let ((rest (gtk:text-buffer-get-text gtk-buffer iter (line-end-iter gtk-buffer (gtk:text-iter-get-line iter)) t)))
                     ;; Only spaces left: kill them and the line break.
                     (if (every (lambda (c) (member c '(#\Space #\Tab))) rest)
                         (progn (gtk:text-iter-forward-to-line-end iter) (gtk:text-iter-forward-char iter))
                         (gtk:text-iter-forward-to-line-end iter))))
               (gtk:text-iter-get-offset iter))))))

(define-command kill-line ()
  "Kill to the end of the line, or the line break at the end of a line.
Kills in a row join into one kill-ring entry. With paredit mode, stop
before the end of the list. With a numeric argument N, kill N lines."
  (let ((view (current-view)))
    (kill-between view (point-offset view) (line-kill-end view))))

(define-command kill-whole-line ()
  "Kill the whole line the cursor is on, with its line break."
  (:repeat t)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (line (gtk:text-iter-get-line (cursor-iter gtk-buffer)))
         (start (text-line-position gtk-buffer line))
         (end (if (< (1+ line) (text-line-count gtk-buffer))
                  (text-line-position gtk-buffer (1+ line))
                  (text-length gtk-buffer))))
    (kill-between view start end)))

(defun word-end-offset (view direction)
  (let ((iter (cursor-iter (view-gtk-buffer view))))
    (if (eq direction :forward)
        (gtk:text-iter-forward-word-end iter)
        (gtk:text-iter-backward-word-start iter))
    (gtk:text-iter-get-offset iter)))

(define-command kill-word ()
  "Kill to the end of the next word."
  (:repeat t)
  (let ((view (current-view)))
    (kill-between view (point-offset view) (word-end-offset view :forward))))

(define-command backward-kill-word ()
  "Kill back to the start of the previous word."
  (:repeat t)
  (let ((view (current-view)))
    (kill-between view (word-end-offset view :backward) (point-offset view) :direction :backward)))

(defun sexp-offset (view direction)
  (let* ((syntax (current-syntax))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (line column) (text-position-line gtk-buffer (point-offset view))
      (multiple-value-bind (l c) (if (eq direction :forward)
                                     (forward-sexp-position syntax line column)
                                     (backward-sexp-position syntax line column))
        (unless l (editor-error "No expression ~:[before~;after~] the cursor" (eq direction :forward)))
        (text-line-position gtk-buffer l c)))))

(define-command kill-sexp ()
  "Kill the expression after the cursor."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (:repeat t)
  (let ((view (current-view)))
    (kill-between view (point-offset view) (sexp-offset view :forward))))

(define-command backward-kill-sexp ()
  "Kill the expression before the cursor."
  (:modes lisp-mode repl-mode editor-repl-mode)
  (:repeat t)
  (let ((view (current-view)))
    (kill-between view (sexp-offset view :backward) (point-offset view) :direction :backward)))

(defun insert-yank (view string)
  (let* ((buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      ;; Yanking over a selection replaces it, as typing does.
      (when has (delete-text-between gtk-buffer (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end))))
    (let ((point (point-offset view)))
      (setf (buffer-local buffer :mark-active) nil)
      (push-mark buffer point)
      (unless (insert-text-at gtk-buffer point string)
        (editor-error "Text is read-only"))
      (setf (buffer-local buffer :last-yank) (cons point (+ point (length string)))
            *this-command-kind* :yank)
      (scroll-to-cursor view))))

(define-command yank ()
  "Insert the most recent kill (or text copied in another application)."
  (let ((view (current-view)))
    (with-user-action ((view-gtk-buffer view))
      (insert-yank view (current-kill 0)))))

(define-command yank-pop ()
  "Right after a yank, replace the text yanked with the kill before it. Otherwise,
choose an entry of the kill ring to insert."
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (last (buffer-local buffer :last-yank)))
    (if (and (eq *last-command-kind* :yank) last)
        (with-user-action (gtk-buffer)
          (delete-text-between gtk-buffer (car last) (cdr last))
          (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (car last)))
          (insert-yank view (current-kill 1)))
        (progn
          (unless *kill-ring* (editor-error "The kill ring is empty"))
          (open-picker (window-picker *window*)
                       :items (copy-list *kill-ring*)
                       :label (lambda (s) (let ((line (substitute #\⏎ #\Newline s)))
                                            (if (> (length line) 100) (format nil "~a…" (subseq line 0 100)) line)))
                       :placeholder "Insert from the kill ring"
                       :on-choose (lambda (s)
                                    (focus-view view)
                                    (with-user-action (gtk-buffer)
                                      (insert-yank view s))))))))

;;; Small Emacs editing commands

(define-command open-line ()
  "Insert a line break after the cursor, leaving the cursor where it is."
  (:repeat t)
  (let* ((view (current-view))
         (point (point-offset view)))
    (insert-text-at (view-gtk-buffer view) point (string #\Newline))
    (gtk:text-buffer-place-cursor (view-gtk-buffer view) (iter-at (view-gtk-buffer view) point))))

(define-command transpose-chars ()
  "Swap the characters around the cursor (at the end of a line, the two before it), and move forward."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view))
         (iter (cursor-iter gtk-buffer))
         (point (if (or (gtk:text-iter-ends-line iter) (= point (text-length gtk-buffer)))
                    (1- point)
                    point)))
    (when (< point 1) (editor-error "Beginning of buffer"))
    (let ((two (text-string gtk-buffer (1- point) (1+ point))))
      (replace-text-between gtk-buffer (1- point) (1+ point) (reverse two))
      (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (1+ point))))))

(defun change-word-case (function)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view))
         (text (text-string gtk-buffer point (min (text-length gtk-buffer) (+ point 400)))))
    (multiple-value-bind (start end) (word-bounds-after text 0)
      (unless start (editor-error "No word after the cursor"))
      (replace-text-between gtk-buffer (+ point start) (+ point end) (funcall function (subseq text start end)))
      (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer (+ point end))))))

(define-command upcase-word () "Make the next word upper case." (:repeat t) (change-word-case #'string-upcase))
(define-command downcase-word () "Make the next word lower case." (:repeat t) (change-word-case #'string-downcase))
(define-command capitalize-word () "Capitalize the next word." (:repeat t) (change-word-case #'capitalize-string))

(defun change-region-case (function)
  (let ((view (current-view)))
    (multiple-value-bind (start end) (region-bounds view)
      (let ((gtk-buffer (view-gtk-buffer view)))
        (replace-text-between gtk-buffer start end (funcall function (text-string gtk-buffer start end)))))))

(define-command upcase-region () "Make the region upper case." (change-region-case #'string-upcase))
(define-command downcase-region () "Make the region lower case." (change-region-case #'string-downcase))

(defun spaces-around (gtk-buffer point)
  (let ((start point) (end point) (n (text-length gtk-buffer)))
    (loop while (and (> start 0) (member (text-char gtk-buffer (1- start)) '(#\Space #\Tab))) do (decf start))
    (loop while (and (< end n) (member (text-char gtk-buffer end) '(#\Space #\Tab))) do (incf end))
    (values start end)))

(define-command delete-horizontal-space ()
  "Delete the spaces and tabs around the cursor."
  (let ((view (current-view)))
    (multiple-value-bind (start end) (spaces-around (view-gtk-buffer view) (point-offset view))
      (delete-text-between (view-gtk-buffer view) start end))))

(define-command just-one-space ()
  "Replace the spaces and tabs around the cursor with one space."
  (let ((view (current-view)))
    (multiple-value-bind (start end) (spaces-around (view-gtk-buffer view) (point-offset view))
      (replace-text-between (view-gtk-buffer view) start end " "))))

(define-command delete-indentation ()
  "Join this line to the previous one, leaving one space between them (none after an open paren)."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (line (gtk:text-iter-get-line (cursor-iter gtk-buffer))))
    (when (zerop line) (editor-error "Beginning of buffer"))
    (let* ((start (text-line-position gtk-buffer line))
           (break (1- start)))
      (multiple-value-bind (s e) (spaces-around gtk-buffer start)
        (declare (ignore s))
        (let* ((before (and (> break 0) (text-char gtk-buffer (1- break))))
               (after (and (< e (text-length gtk-buffer)) (text-char gtk-buffer e)))
               (gap (if (or (eql before #\() (eql after #\)) (null after) (eql after #\Newline)) "" " ")))
          ;; Also the spaces at the end of the previous line.
          (let ((s2 (multiple-value-bind (a) (spaces-around gtk-buffer break) a)))
            (replace-text-between gtk-buffer s2 e gap)
            (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer s2))))))))

(define-command back-to-indentation ()
  "Move to the first non-blank character on the line."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (line (gtk:text-iter-get-line (cursor-iter gtk-buffer))))
    (set-point view (text-line-position gtk-buffer line (leading-space-count (text-line-string gtk-buffer line))))))

;;; The start of a line is where its code starts: after the indentation, and
;;; on a REPL's input line, after the prompt. Pressed again there, it goes to
;;; the line's real start (in the REPL, the input's).

(defun repl-input-start (view line)
  "The offset where the input of VIEW's REPL starts, if it is on LINE."
  (let ((repl (buffer-local (view-buffer view) :repl)))
    (when repl
      (let ((iter (gtk:text-buffer-get-iter-at-mark (view-gtk-buffer view) (repl-input-mark repl))))
        (and (= line (gtk:text-iter-get-line iter)) (gtk:text-iter-get-offset iter))))))

(defun line-starts (view line)
  "LINE's start (in a REPL's input line, the input's), and where its code starts."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (start (or (repl-input-start view line) (text-line-position gtk-buffer line 0)))
         (end (gtk:text-iter-get-offset (line-end-iter gtk-buffer line))))
    (values start
            (loop for offset from start below end
                  while (member (text-char gtk-buffer offset) '(#\Space #\Tab))
                  finally (return offset)))))

(defun move-to-line-start (&key extend)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view)))
    (multiple-value-bind (start code) (line-starts view (gtk:text-iter-get-line (cursor-iter gtk-buffer)))
      (let ((target (if (= point code) start code)))
        (if extend
            (progn (gtk:text-buffer-move-mark gtk-buffer (gtk:text-buffer-get-insert gtk-buffer) (iter-at gtk-buffer target))
                   (scroll-to-cursor view))
            (set-point view target))))))

(define-command beginning-of-code-or-line ()
  "Move to where the line's code starts; there, to the line's start."
  (move-to-line-start))

(define-command select-to-beginning-of-code-or-line ()
  "Extend the selection to where the line's code starts; there, to the line's start."
  (move-to-line-start :extend t))

(define-command recenter ()
  "Scroll so the line with the cursor is in the middle of the view."
  (let ((view (current-view)))
    (gtk:text-view-scroll-to-mark (view-text-view view) (gtk:text-buffer-get-insert (view-gtk-buffer view))
                                  0d0 t 0d0 0.5d0)))

;;; Comments

(defun line-comment-p (string &optional (prefix ";;") suffix)
  "True if STRING is a comment line: for Lisp (PREFIX ;;) any number of
semicolons; otherwise PREFIX, and SUFFIX at the end if there is one."
  (let ((trimmed (string-trim '(#\Space #\Tab) string)))
    (cond ((string= prefix ";;") (and (plusp (length trimmed)) (char= (char trimmed 0) #\;)))
          (t (and (>= (length trimmed) (+ (length prefix) (length (or suffix ""))))
                  (string= prefix trimmed :end2 (length prefix))
                  (or (null suffix)
                      (string= suffix trimmed :start2 (- (length trimmed) (length suffix)))))))))

(defun buffer-comment-syntax (buffer)
  "How BUFFER's lines are commented: the prefix, and the suffix for a language
with only block comments (<!-- -->, /* */)."
  (let* ((name (tree-sitter-language-for-mode (buffer-major-mode buffer)))
         (spec (and name (tree-sitter-language-spec name))))
    (cond ((null spec) (values ";;" nil))
          ((getf spec :comment) (values (getf spec :comment) nil))
          ((getf spec :block-comment) (values-list (getf spec :block-comment)))
          (t (editor-error "This language has no comments")))))

(defun comment-lines (gtk-buffer first last &optional (prefix ";;") suffix)
  "Comment out lines FIRST to LAST with PREFIX (and SUFFIX) at their common
indentation, or uncomment them if they are all comments already."
  (let* ((lines (loop for l from first to last collect (text-line-string gtk-buffer l)))
         (nonblank (remove-if (lambda (s) (string= (string-trim " 	" s) "")) lines)))
    (with-user-action (gtk-buffer)
      (if (and nonblank (every (lambda (s) (line-comment-p s prefix suffix)) nonblank))
          (loop for l from first to last
                for s in lines
                when (line-comment-p s prefix suffix)
                  do (let* ((i (leading-space-count s))
                            (marker-end (if (string= prefix ";;")
                                            (or (position #\; s :start i :test-not #'char=) (length s))
                                            (+ i (length prefix))))
                            (end (if (and (< marker-end (length s)) (char= (char s marker-end) #\Space))
                                     (1+ marker-end)
                                     marker-end)))
                       ;; The suffix first, so the prefix's positions still hold.
                       (when suffix
                         (let* ((stop (length (string-right-trim '(#\Space #\Tab) s)))
                                (from (- stop (length suffix)))
                                (from (if (and (> from end) (char= (char s (1- from)) #\Space)) (1- from) from)))
                           (delete-text-between gtk-buffer (text-line-position gtk-buffer l from)
                                                (text-line-position gtk-buffer l stop))))
                       (delete-text-between gtk-buffer (text-line-position gtk-buffer l i)
                                            (text-line-position gtk-buffer l end))))
          (let ((indent (reduce #'min (mapcar #'leading-space-count nonblank) :initial-value 1000)))
            (loop for l from first to last
                  for s in lines
                  unless (string= (string-trim " 	" s) "")
                    do (when suffix
                         (insert-text-at gtk-buffer (text-line-position gtk-buffer l (length (string-right-trim '(#\Space #\Tab) s)))
                                         (concatenate 'string " " suffix)))
                       (insert-text-at gtk-buffer (text-line-position gtk-buffer l (min indent (length s)))
                                       (concatenate 'string prefix " "))))))))

(define-command toggle-comment ()
  "Comment out the selected lines (or this line), or uncomment them."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (if has
          (let ((last (gtk:text-iter-get-line end)))
            ;; A selection ending at the start of a line doesn't include that line.
            (when (and (gtk:text-iter-starts-line end) (> last (gtk:text-iter-get-line start)))
              (decf last))
            (multiple-value-bind (prefix suffix) (buffer-comment-syntax (view-buffer view))
              (comment-lines gtk-buffer (gtk:text-iter-get-line start) last prefix suffix)))
          (let ((line (gtk:text-iter-get-line (cursor-iter gtk-buffer))))
            (multiple-value-bind (prefix suffix) (buffer-comment-syntax (view-buffer view))
              (comment-lines gtk-buffer line line prefix suffix)))))))

(define-command comment-dwim ()
  "Comment or uncomment the region; with no region, add a comment at the
end of the line (or start one on a blank line)."
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (if (or (gtk:text-buffer-get-has-selection gtk-buffer)
            (buffer-local (view-buffer view) :mark-active))
        (progn (call-command 'toggle-comment) (deactivate-mark view))
        (let* ((line (gtk:text-iter-get-line (cursor-iter gtk-buffer)))
               (string (text-line-string gtk-buffer line))
               (semi (position #\; string)))
          (cond ((string= (string-trim " 	" string) "")
                 (let ((syntax (buffer-syntax (view-buffer view))))
                   (let ((column (or (and syntax (lisp-indentation syntax line)) (length string))))
                     (indent-line-to gtk-buffer line column)
                     (insert-text-at gtk-buffer (text-line-position gtk-buffer line column) ";; ")
                     (set-point view (text-line-position gtk-buffer line (+ column 3))))))
                (semi
                 (set-point view (text-line-position gtk-buffer line
                                                     (min (length string)
                                                          (+ 1 (or (position #\; string :start semi :test-not #'char=) semi))))))
                (t
                 (let ((end (text-line-position gtk-buffer line (length string))))
                   (insert-text-at gtk-buffer end (format nil "~vT; " (max 1 (- 40 (length string)))))
                   (set-point view (text-line-position gtk-buffer line (length (text-line-string gtk-buffer line)))))))))))

;;; Expanding words (M-/)

(define-command dabbrev-expand ()
  "Complete the word before the cursor from words in the buffer (and other
buffers), nearest first. Press again for the next candidate."
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (gtk-buffer (view-gtk-buffer view))
         (point (point-offset view))
         (state (buffer-local buffer :dabbrev)))
    (if (and (eq *last-command-kind* :dabbrev) state (= point (getf state :end)))
        ;; Again: the next candidate.
        (let ((candidates (getf state :candidates))
              (index (1+ (getf state :index))))
          (when (>= index (length candidates))
            (replace-text-between gtk-buffer (getf state :start) point (getf state :prefix))
            (setf (buffer-local buffer :dabbrev) nil)
            (editor-error "No further expansions for ~a" (getf state :prefix)))
          (replace-text-between gtk-buffer (getf state :start) point (nth index candidates))
          (setf (getf (buffer-local buffer :dabbrev) :index) index
                (getf (buffer-local buffer :dabbrev) :end) (+ (getf state :start) (length (nth index candidates)))))
        (let* ((start (loop for i downfrom point
                            while (and (> i 0) (symbol-constituent-p (text-char gtk-buffer (1- i))))
                            finally (return i)))
               (prefix (text-string gtk-buffer start point))
               (candidates (append (dabbrev-candidates prefix (text-string gtk-buffer) point)
                                   (loop for other in (buffer-list)
                                         unless (eq other buffer)
                                           append (ignore-errors (dabbrev-candidates prefix (buffer-string other) 0)))))
               (candidates (remove-duplicates candidates :test #'string= :from-end t)))
          (when (string= prefix "") (editor-error "No word before the cursor"))
          (unless candidates (editor-error "No expansion for ~a" prefix))
          (replace-text-between gtk-buffer start point (first candidates))
          (setf (buffer-local buffer :dabbrev)
                (list :start start :prefix prefix :candidates candidates :index 0
                      :end (+ start (length (first candidates)))))))
    (setf *this-command-kind* :dabbrev)))
