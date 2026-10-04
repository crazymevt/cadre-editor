;;;; refactor.lisp — refactoring commands, the editor's right-click menu, and
;;;; wrapping the selection in brackets or quotes as you type them

(in-package #:cadre-ui)

;;; Extracting a function or a variable (the edits come from core)

(defun selection-offsets (view)
  (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds (view-gtk-buffer view))
    (unless has (editor-error "Select the code first"))
    (values (gtk:text-iter-get-offset start) (gtk:text-iter-get-offset end))))

(defun apply-refactoring (view edits)
  "Make EDITS (from core) in VIEW as one undo step, re-indenting what they touched."
  (let* ((gtk-buffer (view-gtk-buffer view))
         (first (reduce #'min edits :key #'first))
         (last (map-offset (reduce #'max edits :key #'second) edits :after)))
    (with-user-action (gtk-buffer)
      (apply-edits gtk-buffer edits first)
      (let ((first-line (text-position-line gtk-buffer first))
            (last-line (text-position-line gtk-buffer (min last (text-length gtk-buffer)))))
        (indent-lines view first-line last-line)))
    (setf (buffer-local (view-buffer view) :mark-active) nil)
    (scroll-to-cursor view)))

(defun ask-name (prompt then &key (text ""))
  (open-picker (window-picker *window*)
               :placeholder prompt :text text
               :on-choose (lambda (name)
                            (let ((name (string-trim " " name)))
                              (when (string= name "") (editor-error "No name given"))
                              (funcall then name)))))

(define-command extract-function ()
  "Move the selected code into a new function, defined before this top-level
form, and call it here. The variables it uses from around it become its parameters."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (start end) (selection-offsets view)
      (ask-name "Name of the new function:"
                (lambda (name)
                  (multiple-value-bind (edits params)
                      (extract-function-edits (text-string (view-gtk-buffer view)) start end name)
                    (apply-refactoring view edits)
                    (message "Extracted ~a~@[ with parameters ~{~a~^ ~}~]" name params)))))))

(define-command extract-variable ()
  "Bind the selected expression to a new variable with LET around the form
containing it, and use the variable in its place."
  (:modes lisp-mode)
  (let ((view (current-view)))
    (multiple-value-bind (start end) (selection-offsets view)
      (ask-name "Name of the new variable:"
                (lambda (name)
                  (apply-refactoring view (extract-variable-edits (text-string (view-gtk-buffer view)) start end name))
                  (message "Extracted ~a" name))))))

;;; Wrapping the selection

(define-option *wrap-selection* t boolean
  "Typing ( [ { \" ' or ` (and in Markdown * _ ~) with text selected wraps the selection in that pair."
  :category "Editing")

(defparameter *wrap-pairs*
  '(("(" . ")") ("[" . "]") ("{" . "}") ("\"" . "\"") ("'" . "'") ("`" . "`")))

(defun wrap-selection-key (win key text)
  "If KEY types an opening bracket or quote while text is selected in an
editor, wrap the selection in the pair and keep it selected. Returns t if so."
  (let* ((view (focused-view win))
         (pairs (if (and view (markdown-buffer-p (view-buffer view)))
                    (append *wrap-pairs* *markdown-wrap-pairs*)
                    *wrap-pairs*))
         (pair (and *wrap-selection* text (plain-key-p key) (assoc text pairs :test #'string=))))
    (when (and pair view)
      (let ((gtk-buffer (view-gtk-buffer view)))
        (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
          (when (and has (gtk:text-iter-editable start t))
            (let ((s (gtk:text-iter-get-offset start))
                  (e (gtk:text-iter-get-offset end)))
              (with-user-action (gtk-buffer)
                (insert-text-at gtk-buffer e (cdr pair))
                (insert-text-at gtk-buffer s (car pair)))
              (setf (buffer-local (view-buffer view) :mark-active) nil)
              (gtk:text-buffer-select-range gtk-buffer (iter-at gtk-buffer (1+ s)) (iter-at gtk-buffer (1+ e)))
              t)))))))

;;; The right-click menu

(defun command-item (menu label command)
  (gio:menu-append menu label (format nil "app.command('~(~a~)')" command)))

(defun editor-context-menu (buffer)
  "The commands added to the right-click menu of a text view showing BUFFER."
  (let ((menu (gio:menu-new))
        (lisp (member (buffer-major-mode buffer) '(lisp-mode repl-mode editor-repl-mode))))
    (let ((navigate (gio:menu-new)))
      (when lisp
        (command-item navigate "Go to Definition" 'edit-definition)
        (command-item navigate "Find References" 'find-references)
        (command-item navigate "Find Symbol in Project" 'find-symbol-in-project))
      (command-item navigate "Find in Project" 'find-in-project)
      (gio:menu-append-section menu nil navigate))
    (when (eq (buffer-major-mode buffer) 'lisp-mode)
      (let ((refactor (gio:menu-new)))
        (command-item refactor "Rename Symbol…" 'rename-symbol)
        (command-item refactor "Extract Function…" 'extract-function)
        (command-item refactor "Extract Variable…" 'extract-variable)
        (gio:menu-append-submenu menu "Refactor" refactor)))
    (when lisp
      (let ((lisp-section (gio:menu-new)))
        (command-item lisp-section "Describe Symbol" 'describe-symbol)
        (command-item lisp-section "Macroexpand" 'expand-macro-once)
        (when (eq (buffer-major-mode buffer) 'lisp-mode)
          (command-item lisp-section "Evaluate or Compile Form" 'compile-or-eval-defun))
        (gio:menu-append-section menu nil lisp-section)))
    (when (eq (buffer-major-mode buffer) 'markdown-mode)
      (markdown-context-menu menu))
    (let ((claude (gio:menu-new)))
      (command-item claude "Ask Claude to Change…" 'claude-edit)
      (gio:menu-append-section menu nil claude))
    (when (buffer-file buffer)
      (let ((git (gio:menu-new)))
        (command-item git "Blame" 'toggle-blame)
        (command-item git "File History" 'show-file-history)
        (command-item git "Changes Since the Last Commit" 'git-diff-file)
        (command-item git "Revert This Change" 'git-revert-change)
        (gio:menu-append-submenu menu "Git" git)))
    (unless (eq (buffer-major-mode buffer) 'markdown-mode)
      (let ((edit (gio:menu-new)))
        (command-item edit "Toggle Comment" 'toggle-comment)
        (gio:menu-append-section menu nil edit)))
    menu))

(defun setup-context-menu (view)
  "Give VIEW's text view Cadre's right-click menu, and make a right click
move the cursor there first (unless it is inside the selection), so the
commands act on what was clicked."
  (let ((text-view (view-text-view view))
        (click (gtk:gesture-click-new)))
    (gtk:text-view-set-extra-menu text-view (editor-context-menu (view-buffer view)))
    (gtk:gesture-single-set-button click 3)
    (gtk:event-controller-set-propagation-phase click :capture)
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore gesture n))
                       (multiple-value-bind (bx by)
                           (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
                         (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
                           (when ok
                             (let ((gtk-buffer (view-gtk-buffer view)))
                               (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
                                 (unless (and has (gtk:text-iter-in-range iter start end))
                                   (gtk:text-buffer-place-cursor gtk-buffer iter)))))))
                       (focus-view view)))
    (gtk:widget-add-controller text-view click)))
