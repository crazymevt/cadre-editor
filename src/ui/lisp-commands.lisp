;;;; lisp-commands.lisp — M1 commands: moving over s-expressions,
;;;; indentation, the command palette, quick open, find, go to line

(in-package #:cadre-ui)

(defun current-syntax ()
  "The current buffer's Lisp syntax. Signals an editor-error outside Lisp buffers."
  (or (buffer-syntax (view-buffer (current-view)))
      (editor-error "Not a Lisp buffer.")))

(defun cursor-line-column (view)
  (iter-line-column (cursor-iter (view-gtk-buffer view))))

(defun goto-line-column (view line column &key (extend (buffer-local (view-buffer view) :mark-active)))
  "Move VIEW's cursor to (LINE, COLUMN), extending the selection if EXTEND."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (iter (line-iter gtk-buffer line column)))
    (if extend
        (gtk:text-buffer-move-mark gtk-buffer (gtk:text-buffer-get-insert gtk-buffer) iter)
        (gtk:text-buffer-place-cursor gtk-buffer iter))
    (scroll-to-cursor view)))

(defmacro define-motion (name documentation function failure)
  `(define-command ,name ()
     ,documentation
     (:modes lisp-mode)
     (let ((view (current-view))
           (syntax (current-syntax)))
       (multiple-value-bind (line column) (cursor-line-column view)
         (multiple-value-bind (l c) (,function syntax line column)
           (if l
               (goto-line-column view l c)
               (editor-error ,failure)))))))

(define-motion forward-sexp "Move over the next s-expression."
  forward-sexp-position "No more expressions in this list")
(define-motion backward-sexp "Move back over the previous s-expression."
  backward-sexp-position "No more expressions in this list")
(define-motion backward-up-list "Move to the start of the list around the cursor."
  up-list-position "Not inside a list")
(define-motion down-list "Move into the next list."
  down-list-position "No list ahead")
(define-motion beginning-of-defun "Move to the start of the top-level form."
  beginning-of-defun-position "No top-level form before the cursor")
(define-motion end-of-defun "Move to the end of the top-level form."
  end-of-defun-position "No top-level form after the cursor")

(define-command mark-sexp ()
  "Select the next s-expression."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (l c) (forward-sexp-position (current-syntax) line column)
        (unless l (editor-error "No expression to select"))
        (let ((gtk-buffer (view-gtk-buffer view)))
          (gtk:text-buffer-select-range gtk-buffer (line-iter gtk-buffer l c) (line-iter gtk-buffer line column)))))))

;;; Indentation

(defun leading-space-count (string)
  (or (position-if-not (lambda (c) (member c '(#\Space #\Tab))) string) (length string)))

(defun indent-line-to (gtk-buffer line column)
  "Make LINE's indentation COLUMN spaces."
  (let* ((string (text-line-string gtk-buffer line))
         (current (leading-space-count string)))
    (unless (and (= current column) (not (find #\Tab string :end current)))
      (gtk:text-buffer-delete gtk-buffer (line-iter gtk-buffer line) (line-iter gtk-buffer line current))
      (gtk:text-buffer-insert gtk-buffer (line-iter gtk-buffer line)
                              (make-string column :initial-element #\Space) -1))))

(defun indent-lines (view first last)
  (let* ((gtk-buffer (view-gtk-buffer view))
         (syntax (buffer-syntax (view-buffer view))))
    (with-user-action (gtk-buffer)
      (loop for line from first to last
            for column = (lisp-indentation syntax line)
            when (and column (plusp (length (string-trim " 	" (text-line-string gtk-buffer line)))))
              do (indent-line-to gtk-buffer line column)))))

(define-command indent-line ()
  "Indent the current line, or the selected lines, as Lisp."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view))
         (syntax (current-syntax)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (if (and has (/= (gtk:text-iter-get-line start) (gtk:text-iter-get-line end)))
          (indent-lines view (gtk:text-iter-get-line start)
                        (if (zerop (gtk:text-iter-get-line-offset end))
                            (1- (gtk:text-iter-get-line end))
                            (gtk:text-iter-get-line end)))
          (multiple-value-bind (line column) (cursor-line-column view)
            (let ((target (lisp-indentation syntax line)))
              (when target
                (let ((before (leading-space-count (text-line-string gtk-buffer line))))
                  (with-user-action (gtk-buffer)
                    (indent-line-to gtk-buffer line target))
                  ;; In the indentation, go to its end; otherwise stay put in the text.
                  (if (<= column before)
                      (goto-line-column view line target :extend nil))))))))))

(define-command newline-and-indent ()
  "Start a new line, indented as Lisp."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete-selection gtk-buffer t t)
      (gtk:text-buffer-insert-at-cursor gtk-buffer (string #\Newline) -1)
      (multiple-value-bind (line) (cursor-line-column view)
        (let ((target (lisp-indentation (current-syntax) line)))
          (when target
            (indent-line-to gtk-buffer line target)
            (goto-line-column view line target :extend nil)))))
    (scroll-to-cursor view)))

(define-command indent-region ()
  "Indent the selected lines as Lisp."
  (:modes lisp-mode)
  (let* ((view (current-view))
         (gtk-buffer (view-gtk-buffer view)))
    (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
      (unless has (editor-error "Nothing is selected"))
      (indent-lines view (gtk:text-iter-get-line start) (gtk:text-iter-get-line end))
      (message "Indented"))))

(define-command indent-defun ()
  "Indent the whole top-level form around the cursor."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (line column) (cursor-line-column view)
      (multiple-value-bind (sl sc el) (toplevel-form-bounds (current-syntax) line column)
        (declare (ignore sc))
        (unless sl (editor-error "Not in a top-level form"))
        (indent-lines view sl el)))))

;;; Pickers

(defun window-picker (win)
  (or (window-picker-object win)
      (setf (window-picker-object win) (make-picker (window-title win)))))

(define-command execute-command ()
  "Choose a command by name and run it."
  (:title "Show all commands")
  (let ((keymaps (active-keymaps *window*)))
    (open-picker (window-picker *window*)
                 :items (list-commands)
                 :label #'command-title
                 :detail (lambda (command) (first (where-is (command-name command) keymaps)))
                 :placeholder "Run a command"
                 :on-choose (lambda (command) (call-command (command-name command))))))

(defparameter *quick-open-limit* 20000)

(defun quick-open-files (directory)
  "Files under DIRECTORY, as paths relative to it, skipping hidden names and types."
  (let ((files '()) (count 0))
    (labels ((hidden-p (name) (member name *explorer-hidden-names* :test #'string=))
             (walk (dir)
               (dolist (file (uiop:directory-files dir))
                 (when (>= count *quick-open-limit*) (return-from quick-open-files (nreverse files)))
                 (unless (or (hidden-p (file-namestring file))
                             (member (pathname-type file) *explorer-hidden-types* :test #'equalp))
                   (push (enough-namestring file directory) files)
                   (incf count)))
               (dolist (sub (uiop:subdirectories dir))
                 (unless (or (hidden-p (car (last (pathname-directory sub))))
                             ;; Do not follow links to directories (they may loop).
                             (directory-link-p sub))
                   (walk sub)))))
      (walk directory))
    (sort (nreverse files) #'< :key #'length)))

(define-command quick-open ()
  "Open a file in the project by typing part of its name."
  (let ((project (or (window-project *window*) (editor-error "Open a folder first."))))
    (open-picker (window-picker *window*)
                 :items (quick-open-files project)
                 :placeholder "Open a file by name"
                 :on-choose (lambda (path) (open-file-path (merge-pathnames path project))))))

(define-command switch-to-buffer ()
  "Choose an open buffer to show."
  (open-picker (window-picker *window*)
               :items (buffer-list)
               :label #'buffer-name
               :detail (lambda (b) (if (buffer-file b) (uiop:native-namestring (buffer-file b)) ""))
               :placeholder "Switch to buffer"
               :on-choose (lambda (buffer) (show-buffer *window* buffer))))

(define-command go-to-line ()
  "Go to a line by number."
  (let ((view (current-view)))
    (open-picker (window-picker *window*)
                 :placeholder (format nil "Line number (1–~d)"
                                      (gtk:text-buffer-get-line-count (view-gtk-buffer view)))
                 :on-choose (lambda (text)
                              (let ((n (ignore-errors (parse-integer text))))
                                (unless n (editor-error "Not a line number: ~a" text))
                                (goto-line-column view (max 0 (1- n)) 0 :extend nil)
                                (focus-view view))))))

;;; Find

(define-command find-text ()
  "Find text in the current buffer. Again (with the bar open): the next match."
  (current-view)
  (let ((fb (window-find-bar *window*)))
    (if (and (find-bar-open-p fb) (gtk:widget-has-focus (find-bar-entry fb)))
        (find-step fb 1)
        (find-open fb))))

(define-command find-next ()
  "Go to the next match."
  (let ((fb (window-find-bar *window*)))
    (if (find-bar-open-p fb) (find-step fb 1) (find-open fb))))

(define-command find-previous ()
  "Go to the previous match."
  (let ((fb (window-find-bar *window*)))
    (if (find-bar-open-p fb) (find-step fb -1) (find-open fb))))
