;;;; cursors.lisp — more than one cursor, as in VS Code
;;;;
;;;; GTK's text view has one cursor. The others are pairs of marks, the
;;;; cursor and the other end of its selection, drawn as thin overlay
;;;; widgets with their selections tagged. A key typed while there are
;;;; others is done at each cursor in turn, as one step for undo: GTK's
;;;; cursor and selection move to each one, the key's command runs (or the
;;;; key is typed, as a keyboard macro replays it), and where it ends up is
;;;; kept. Commands that aren't about the text at the cursor (saving,
;;;; opening files, the palette) run once.
;;;;
;;;; Ctrl+D selects the word at the cursor, then adds its next occurrence;
;;;; Ctrl+K Ctrl+D skips one; Ctrl+Shift+L selects them all;
;;;; Ctrl+Alt+Up/Down add a cursor on the line above or below; Alt+click
;;;; adds or removes one. Escape, or a click without Alt, goes back to one.

(in-package #:cadre-ui)

(defstruct (extra-cursor (:conc-name cursor-))
  insert bound caret)

(defvar *at-cursor* nil "True while a key is being done at a cursor other than GTK's.")

(defun mark-offset-in (gtk-buffer mark)
  (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer mark)))

(defun cursor-selection-offsets (gtk-buffer)
  "GTK's cursor and the other end of its selection, as offsets."
  (values (mark-offset-in gtk-buffer (gtk:text-buffer-get-insert gtk-buffer))
          (mark-offset-in gtk-buffer (gtk:text-buffer-get-selection-bound gtk-buffer))))

(defun select-offsets (gtk-buffer insert bound)
  (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer insert) (iter-at gtk-buffer bound)))

(defun cursor-offsets (view cursor)
  (let ((gtk-buffer (view-gtk-buffer view)))
    (values (mark-offset-in gtk-buffer (cursor-insert cursor))
            (mark-offset-in gtk-buffer (cursor-bound cursor)))))

(defun all-cursor-ranges (view)
  "Every cursor's (insert bound) in VIEW, GTK's first, then the others."
  (cons (multiple-value-list (cursor-selection-offsets (view-gtk-buffer view)))
        (mapcar (lambda (cursor) (multiple-value-list (cursor-offsets view cursor)))
                (view-cursors view))))

;;; Adding and removing cursors

(defvar *spare-carets* (make-hash-table :test 'eq :weakness :key)
  "View → the carets of cursors it no longer has, hidden, to use again:
gtk_text_view_remove leaves an overlay child in the text view.")

(defun view-caret (view)
  "A hidden caret overlay in VIEW's text view: a spare one, or a new one."
  (or (pop (gethash view *spare-carets*))
      (let ((caret (make-instance 'gtk:box :css-classes '("cadre-extra-cursor") :can-target nil :visible nil)))
        (gtk:text-view-add-overlay (view-text-view view) caret 0 0)
        caret)))

(defun add-extra-cursor (view insert &optional (bound insert))
  "Add a cursor at offset INSERT in VIEW, with a selection to BOUND."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (caret (view-caret view))
         (cursor (make-extra-cursor
                  :insert (gtk:text-buffer-create-mark gtk-buffer nil (iter-at gtk-buffer insert) nil)
                  :bound (gtk:text-buffer-create-mark gtk-buffer nil (iter-at gtk-buffer bound) nil)
                  :caret caret)))
    (setf (view-cursors view) (append (view-cursors view) (list cursor)))
    (redraw-cursors view)
    cursor))

(defun remove-extra-cursor (view cursor)
  (let ((gtk-buffer (view-gtk-buffer view))
        (caret (cursor-caret cursor)))
    (setf (view-cursors view) (remove cursor (view-cursors view)))
    (gtk:widget-set-visible caret nil)
    (push caret (gethash view *spare-carets*))
    (gtk:text-buffer-delete-mark gtk-buffer (cursor-insert cursor))
    (gtk:text-buffer-delete-mark gtk-buffer (cursor-bound cursor))))

(defun clear-extra-cursors (view)
  "Go back to GTK's one cursor in VIEW. True if there were others."
  (when (view-cursors view)
    (dolist (cursor (view-cursors view))
      (remove-extra-cursor view cursor))
    (setf (view-occurrence-whole-word view) nil)
    (redraw-cursors view)
    t))

(defun merge-cursors (view)
  "Remove cursors that have come together with another one."
  (let ((seen (list (cursor-selection-offsets (view-gtk-buffer view)))))
    (dolist (cursor (view-cursors view))
      (let ((insert (cursor-offsets view cursor)))
        (if (member insert seen)
            (remove-extra-cursor view cursor)
            (push insert seen))))))

;;; Drawing them

(defun extra-selection-tag (gtk-buffer)
  (ensure-face-tag gtk-buffer "cadre-extra-selection" :search))

(defun redraw-cursors (view)
  "Put each cursor's caret where it is now, and tag its selection. The tag
is the buffer's, so the selections of the buffer's other views (splits)
are tagged again too."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (text-view (view-text-view view))
         (tag (extra-selection-tag gtk-buffer)))
    (gtk:text-buffer-remove-tag gtk-buffer tag (gtk:text-buffer-get-start-iter gtk-buffer)
                                (gtk:text-buffer-get-end-iter gtk-buffer))
    (dolist (cursor (view-cursors view))
      (let ((rect (gtk:text-view-get-iter-location text-view (iter-at gtk-buffer (cursor-offsets view cursor))))
            (caret (cursor-caret cursor)))
        (gtk:widget-set-size-request caret 2 (max 1 (gdk:rectangle-height rect)))
        (gtk:text-view-move-overlay text-view caret (gdk:rectangle-x rect) (gdk:rectangle-y rect))
        (gtk:widget-set-visible caret t)))
    (dolist (shown (adjoin view (and *window* (buffer-views *window* (view-buffer view)))))
      (dolist (cursor (view-cursors shown))
        (multiple-value-bind (insert bound) (cursor-offsets shown cursor)
          (unless (= insert bound)
            (gtk:text-buffer-apply-tag gtk-buffer tag (iter-at gtk-buffer (min insert bound))
                                       (iter-at gtk-buffer (max insert bound)))))))))

(defvar *cursor-redraws-pending* (make-hash-table :test 'eq :weakness :key))

(defun schedule-cursor-redraw (view)
  "Redraw VIEW's cursors once GTK has laid out the text again."
  (unless (gethash view *cursor-redraws-pending*)
    (setf (gethash view *cursor-redraws-pending*) t)
    (glib:idle-add glib:+priority-default-idle+
                   (lambda ()
                     (remhash view *cursor-redraws-pending*)
                     (redraw-cursors view)
                     nil))))

;;; Doing a key at each cursor

(defun call-at-each-cursor (view thunk &key only-if-done)
  "Call THUNK at GTK's cursor, then with GTK's cursor and selection moved
to each of VIEW's other cursors, as one step for undo. With ONLY-IF-DONE,
the others only if THUNK returned true at the first. Returns what THUNK
returned at the first."
  (if (or *at-cursor* (null (view-cursors view)))
      (funcall thunk)
      (let* ((gtk-buffer (view-gtk-buffer view))
             (result nil))
        (with-user-action (gtk-buffer)
          (setf result (funcall thunk))
          (when (or result (not only-if-done))
            (multiple-value-bind (insert bound) (cursor-selection-offsets gtk-buffer)
              (let ((main-insert (gtk:text-buffer-create-mark gtk-buffer nil (iter-at gtk-buffer insert) nil))
                    (main-bound (gtk:text-buffer-create-mark gtk-buffer nil (iter-at gtk-buffer bound) nil)))
                (unwind-protect
                     (let ((*at-cursor* t))
                       (dolist (cursor (copy-list (view-cursors view)))
                         (multiple-value-bind (i b) (cursor-offsets view cursor)
                           (select-offsets gtk-buffer i b))
                         (handler-case (funcall thunk)
                           (editor-error () nil))
                         (multiple-value-bind (i b) (cursor-selection-offsets gtk-buffer)
                           (gtk:text-buffer-move-mark gtk-buffer (cursor-insert cursor) (iter-at gtk-buffer i))
                           (gtk:text-buffer-move-mark gtk-buffer (cursor-bound cursor) (iter-at gtk-buffer b)))))
                  (gtk:text-buffer-select-range gtk-buffer
                                                (gtk:text-buffer-get-iter-at-mark gtk-buffer main-insert)
                                                (gtk:text-buffer-get-iter-at-mark gtk-buffer main-bound))
                  (gtk:text-buffer-delete-mark gtk-buffer main-insert)
                  (gtk:text-buffer-delete-mark gtk-buffer main-bound))))))
        (merge-cursors view)
        (gtk:text-view-scroll-mark-onscreen (view-text-view view) (gtk:text-buffer-get-insert gtk-buffer))
        (schedule-cursor-redraw view)
        result)))

(defparameter *cursor-commands*
  '(indent-line newline-and-indent code-newline code-indent markdown-newline markdown-indent
    beginning-of-code-or-line select-to-beginning-of-code-or-line transpose-chars kill-line yank
    forward-sexp backward-sexp)
  "Commands done at each cursor, besides those that take a repeat count and paredit's.")

(defparameter *one-cursor-commands*
  '(other-group scroll-down-page scroll-up-page move-lines-up move-lines-down
    duplicate-lines-up duplicate-lines-down)
  "Commands that take a repeat count but are done once, whatever the cursors.")

(defun cursor-command-p (name)
  "True if the command NAME is done at each cursor."
  (and (not (member name *one-cursor-commands*))
       (or (member name *cursor-commands*)
           (let ((command (find-command name))) (and command (command-repeat command)))
           (uiop:string-prefix-p "PAREDIT-" (symbol-name name)))))

(defun cursor-motion (key)
  "What KEY does to the cursor, when Cadre doesn't bind it: the text
view's :move-cursor step and count, and whether it extends the selection."
  (let* ((mods (key-modifiers key))
         (base (subseq key (* 2 (length mods))))
         (shift (and (member #\S mods) t))
         (others (remove #\S mods))
         (word (if (macos-p) '(#\M) '(#\C))))
    (flet ((result (step count) (values step count shift)))
      (cond ((null others)
             (let ((motion (assoc base *cursor-keys* :test #'string=)))
               (and motion (result (second motion) (third motion)))))
            ((and (equal others word) (member base '("Left" "Right") :test #'string=))
             (result :words (if (string= base "Left") -1 1)))
            ((and (macos-p) (equal others '(#\C)) (string= base "Right"))
             (result :display-line-ends 1))
            ((and (equal others '(#\C)) (member base '("Up" "Down" "Home" "End") :test #'string=)
                  (or (macos-p) (member base '("Home" "End") :test #'string=)))
             (result :buffer-ends (if (member base '("Up" "Home") :test #'string=) -1 1)))))))

(defun typed-string (key text)
  "The text KEY types when Cadre doesn't bind it, or nil."
  (cond ((string= key "RET") (string #\Newline))
        ((string= key "TAB") (string #\Tab))
        ((string= key "SPC") " ")
        ((and text (plain-key-p key)) text)))

(defun type-at-cursor (view key text)
  "Do what KEY does at GTK's cursor in VIEW when Cadre doesn't bind it."
  (let ((text-view (view-text-view view))
        (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (step count extend) (cursor-motion key)
      (cond (step (gobject:emit text-view :move-cursor step count extend))
            ((string= key "DEL") (gobject:emit text-view :backspace))
            ((string= key "Delete") (gobject:emit text-view :delete-from-cursor :chars 1))
            (t (let ((string (typed-string key text)))
                 (when string
                   (gtk:text-buffer-delete-selection gtk-buffer t t)
                   (gtk:text-buffer-insert-interactive-at-cursor gtk-buffer string -1 t))))))))

(defun cursor-key-p (key text)
  (or (cursor-motion key) (member key '("DEL" "Delete") :test #'string=) (typed-string key text)))

;;; Copying and pasting with several cursors

(defvar *cursor-clipboard* nil
  "What several cursors last copied: (text . pieces), so pasting with as
many cursors puts each piece back at one.")

(defun cursors-in-order (view)
  "VIEW's cursors in the order CALL-AT-EACH-CURSOR visits them, each as its
index in the text's order."
  (let* ((ranges (all-cursor-ranges view))
         (sorted (sort (copy-list ranges) #'< :key (lambda (r) (min (first r) (second r))))))
    (mapcar (lambda (range) (position range sorted :test #'eq)) ranges)))

(defun copy-at-cursors (view &key cut)
  (let* ((gtk-buffer (view-gtk-buffer view))
         (ranges (sort (all-cursor-ranges view) #'< :key (lambda (r) (min (first r) (second r)))))
         (pieces (mapcar (lambda (r) (text-string gtk-buffer (min (first r) (second r)) (max (first r) (second r))))
                         ranges))
         (text (format nil "~{~a~^~%~}" pieces)))
    (when (every (lambda (piece) (zerop (length piece))) pieces)
      (editor-error "Nothing is selected"))
    (setf *cursor-clipboard* (cons text pieces))
    (copy-to-clipboard text)
    (when cut
      (call-at-each-cursor view (lambda () (gtk:text-buffer-delete-selection gtk-buffer t t))))))

(defun paste-pieces (text count)
  "How to paste TEXT at COUNT cursors: a list of COUNT strings, in the
text's order."
  (let ((lines (uiop:split-string (string-right-trim '(#\Newline) text) :separator '(#\Newline))))
    (cond ((and (equal text (car *cursor-clipboard*)) (= count (length (cdr *cursor-clipboard*))))
           (cdr *cursor-clipboard*))
          ((and (> count 1) (= count (length lines))) lines)
          (t (make-list count :initial-element text)))))

(defun paste-at-cursors (view)
  (gio:async (gdk:clipboard-read-text-async (clipboard))
             (lambda (text)
               (when (and (stringp text) (view-cursors view) (member (view-buffer view) (buffer-list)))
                 (let* ((gtk-buffer (view-gtk-buffer view))
                        (pieces (paste-pieces text (1+ (length (view-cursors view)))))
                        (queue (mapcar (lambda (index) (nth index pieces)) (cursors-in-order view))))
                   (call-at-each-cursor view (lambda ()
                                               (gtk:text-buffer-delete-selection gtk-buffer t t)
                                               (gtk:text-buffer-insert-interactive-at-cursor
                                                gtk-buffer (pop queue) -1 t))))))
             :error (lambda (e) (message "Could not paste: ~a" e))))

(defun cursors-unbound-key (win key text)
  "With several cursors in the focused editor, do KEY, which nothing binds,
at each. True if it was done."
  (let ((view (focused-view win)))
    (when (and view (view-cursors view))
      (flet ((is (&rest keys) (member key keys :test #'string=)))
        (handler-case
            (cond ((is "C-c" "s-c") (copy-at-cursors view) t)
                  ((is "C-x" "s-x") (copy-at-cursors view :cut t) t)
                  ((is "C-v" "s-v") (paste-at-cursors view) t)
                  ((cursor-key-p key text)
                   (call-at-each-cursor view (lambda () (type-at-cursor view key text)))
                   t))
          (editor-error (e) (message "~a" (editor-error-message e)) t))))))

;;; Occurrences

(defun view-word-char-p (view)
  (if (buffer-sexp-syntax (view-buffer view))
      #'symbol-constituent-p
      (lambda (c) (or (word-char-p c) (char= c #\$)))))

(defun occurrence-needle (view)
  "The selected text to look for, first selecting the word at the cursor if
nothing is. Second value: true if it was just selected that way."
  (let ((gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (insert bound) (cursor-selection-offsets gtk-buffer)
      (if (/= insert bound)
          (text-string gtk-buffer (min insert bound) (max insert bound))
          (multiple-value-bind (start end) (word-at (text-string gtk-buffer) insert (view-word-char-p view))
            (unless start (editor-error "No word at the cursor"))
            (select-offsets gtk-buffer end start)
            (let ((needle (text-string gtk-buffer start end)))
              ;; A word picked this way matches only as a whole word.
              (setf (view-occurrence-whole-word view) needle)
              (values needle t)))))))

(defun occurrence-search (view needle)
  "The text, and the starts of the occurrences already selected."
  (values (text-string (view-gtk-buffer view))
          (mapcar (lambda (r) (min (first r) (second r))) (all-cursor-ranges view))
          (equal needle (view-occurrence-whole-word view))))

(defun select-next-occurrence-in (view &key keep)
  "Select the next occurrence of the selection in VIEW, keeping the
current selection as another cursor if KEEP."
  (multiple-value-bind (needle new) (occurrence-needle view)
    (unless new
      (let ((gtk-buffer (view-gtk-buffer view)))
        (multiple-value-bind (string taken whole-word) (occurrence-search view needle)
          (multiple-value-bind (insert bound) (cursor-selection-offsets gtk-buffer)
            (let ((start (next-occurrence string needle (max insert bound)
                                          (if keep taken (remove (min insert bound) taken))
                                          :whole-word whole-word :char-p (view-word-char-p view))))
              (unless start (editor-error "No more occurrences of ~s" needle))
              (when keep (add-extra-cursor view insert bound))
              (select-offsets gtk-buffer (+ start (length needle)) start)
              (scroll-to-cursor view)
              (schedule-cursor-redraw view))))))))

(define-command select-next-occurrence ()
  "Select the word at the cursor; again, add a cursor at the selection's next occurrence."
  (select-next-occurrence-in (current-view) :keep t))

(define-command skip-occurrence ()
  "Move the latest selection on to the next occurrence, without a cursor where it was."
  (select-next-occurrence-in (current-view) :keep nil))

(define-command select-all-occurrences ()
  "Put a cursor at every occurrence of the selection (or the word at the cursor)."
  (let* ((view (current-view))
         (needle (occurrence-needle view)))
    (multiple-value-bind (string taken whole-word) (occurrence-search view needle)
      (let ((count 0))
        (dolist (start (occurrences string needle :whole-word whole-word :char-p (view-word-char-p view)))
          (unless (member start taken)
            (add-extra-cursor view (+ start (length needle)) start)
            (incf count)))
        (message "~d cursor~:p" (1+ (length (view-cursors view))))
        count))))

;;; Cursors above and below, and by clicking

(defun add-cursor-vertically (view direction)
  (let* ((gtk-buffer (view-gtk-buffer view))
         (lines (mapcar (lambda (r) (gtk:text-iter-get-line (iter-at gtk-buffer (first r))))
                        (all-cursor-ranges view)))
         (line (if (plusp direction) (1+ (reduce #'max lines)) (1- (reduce #'min lines))))
         (column (gtk:text-iter-get-line-offset (cursor-iter gtk-buffer))))
    (unless (< -1 line (gtk:text-buffer-get-line-count gtk-buffer))
      (editor-error "No line ~:[above~;below~]" (plusp direction)))
    (add-extra-cursor view (gtk:text-iter-get-offset (line-iter gtk-buffer line column)))))

(define-command add-cursor-above ()
  "Add a cursor on the line above the top one."
  (add-cursor-vertically (current-view) -1))

(define-command add-cursor-below ()
  "Add a cursor on the line below the bottom one."
  (add-cursor-vertically (current-view) 1))

(define-command single-cursor ()
  "Remove every cursor but the main one."
  (clear-extra-cursors (current-view)))

(defun toggle-cursor-at (view offset)
  "Alt+click: remove the cursor at OFFSET, or add one there."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (existing (find offset (view-cursors view) :key (lambda (c) (cursor-offsets view c)))))
    (cond (existing (remove-extra-cursor view existing))
          ((= offset (cursor-selection-offsets gtk-buffer))
           ;; The main cursor: the newest other one takes its place.
           (let ((last (car (last (view-cursors view)))))
             (when last
               (multiple-value-bind (i b) (cursor-offsets view last)
                 (remove-extra-cursor view last)
                 (select-offsets gtk-buffer i b)))))
          (t (multiple-value-bind (insert bound) (cursor-selection-offsets gtk-buffer)
               (add-extra-cursor view insert bound))
             (gtk:text-buffer-place-cursor gtk-buffer (iter-at gtk-buffer offset))))
    (redraw-cursors view)))

(defun setup-cursor-clicks (view)
  "Alt+click adds or removes a cursor; another click goes back to one."
  (let ((click (gtk:gesture-click-new)))
    (gtk:event-controller-set-propagation-phase click :capture)
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore n))
                       (let ((state (modifier-list (gtk:event-controller-get-current-event-state gesture)))
                             (text-view (view-text-view view)))
                         (cond ((member :alt-mask state)
                                (multiple-value-bind (bx by)
                                    (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
                                  (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
                                    (when ok
                                      (gtk:gesture-set-state gesture :claimed)
                                      (gtk:widget-grab-focus text-view)
                                      (toggle-cursor-at view (gtk:text-iter-get-offset iter))))))
                               ((view-cursors view) (clear-extra-cursors view))))))
    (gtk:widget-add-controller (view-text-view view) click)))
