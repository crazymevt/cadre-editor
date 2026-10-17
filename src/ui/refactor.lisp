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
  "The right-click menu of a text view showing BUFFER. It is Cadre's own,
not GTK's text menu with these added: that one is taller than a laptop's
window, and a menu macOS has to move to fit on the screen takes its clicks
where it was meant to be, not where it is drawn. So it is kept short: the
everyday commands, and the rest in submenus."
  (let* ((menu (gio:menu-new))
         (mode (buffer-major-mode buffer))
         (lisp (member mode '(lisp-mode repl-mode editor-repl-mode)))
         (other-lisp (member mode '(clojure-mode clojure-repl-mode racket-mode racket-repl-mode)))
         (lisp-file (eq mode 'lisp-mode))
         (lisp-family (lisp-family-mode-p mode))
         (markdown (eq mode 'markdown-mode)))
    (let ((clipboard (gio:menu-new)))
      ;; The text view's own actions.
      (gio:menu-append clipboard "Cut" "clipboard.cut")
      (gio:menu-append clipboard "Copy" "clipboard.copy")
      (gio:menu-append clipboard "Paste" "clipboard.paste")
      (gio:menu-append-section menu nil clipboard))
    (when lisp
      (let ((symbol (gio:menu-new)))
        (command-item symbol "Go to Definition" 'edit-definition)
        (command-item symbol "Look Up in HyperSpec" 'hyperspec-look-up)
        (command-item symbol "Describe Symbol" 'describe-symbol)
        (command-item symbol "Browse Class" 'browse-class)
        (when lisp-file
          (command-item symbol "Evaluate or Compile Form" 'compile-or-eval-defun))
        (gio:menu-append-section menu nil symbol)))
    (when other-lisp
      (let ((symbol (gio:menu-new)))
        (command-item symbol "Go to Definition" 'edit-definition)
        (command-item symbol "Describe Symbol" 'describe-symbol)
        (command-item symbol "Look Up in Docs" 'look-up-documentation)
        (when (member mode '(clojure-mode racket-mode))
          (command-item symbol "Evaluate Form" 'compile-or-eval-defun))
        (gio:menu-append-section menu nil symbol)))
    (when markdown
      (markdown-context-menu menu))
    (let ((more (gio:menu-new)))
      (let ((find (gio:menu-new)))
        (when (or lisp other-lisp)
          (command-item find "Find References" 'find-references)
          (command-item find "Find Symbol in Project" 'find-symbol-in-project))
        (command-item find "Find in Project" 'find-in-project)
        (gio:menu-append-submenu more "Find" find))
      (when lisp-file
        (let ((refactor (gio:menu-new)))
          (command-item refactor "Rename Symbol…" 'rename-symbol)
          (command-item refactor "Extract Function…" 'extract-function)
          (command-item refactor "Extract Variable…" 'extract-variable)
          (gio:menu-append-submenu more "Refactor" refactor)))
      (when lisp
        (let ((lisp-menu (gio:menu-new))
              (debug (gio:menu-new)))
          (command-item lisp-menu "Macroexpand" 'expand-macro-once)
          (command-item lisp-menu "Expand in Place" 'macrostep-expand-in-place)
          (when (macrosteps buffer)
            (command-item lisp-menu "Collapse Expansion" 'macrostep-collapse)
            (command-item lisp-menu "Collapse All Expansions" 'macrostep-collapse-all-expansions))
          (command-item debug "Trace / Untrace Function" 'toggle-trace)
          (command-item debug "Show Traces" 'show-traces)
          (when lisp-file
            (command-item debug "Profile Form" 'profile-form)
            (command-item debug "Run This Test" 'run-test-at-cursor)
            (command-item debug "Step Through Form" 'step-expression)
            (command-item debug "Compile for Debugging" 'compile-defun-for-debugging))
          (gio:menu-append-section lisp-menu nil debug)
          (gio:menu-append-submenu more "Lisp" lisp-menu)))
      (when (eq mode 'clojure-mode)
        (let ((clojure-menu (gio:menu-new)))
          (command-item clojure-menu "Macroexpand" 'expand-macro-once)
          (command-item clojure-menu "Expand in Place" 'macrostep-expand-in-place)
          (when (macrosteps buffer)
            (command-item clojure-menu "Collapse Expansion" 'macrostep-collapse)
            (command-item clojure-menu "Collapse All Expansions" 'macrostep-collapse-all-expansions))
          (command-item clojure-menu "Switch the REPL to This Namespace" 'clojure-set-namespace)
          (command-item clojure-menu "Load File" 'compile-and-load-file)
          (command-item clojure-menu "Run Tests in Namespace" 'clojure-run-tests)
          (command-item clojure-menu "Inspect…" 'inspect-value)
          (gio:menu-append-submenu more "Clojure" clojure-menu)))
      (when (eq mode 'racket-mode)
        (let ((racket-menu (gio:menu-new)))
          (command-item racket-menu "Run File" 'racket-run)
          (command-item racket-menu "Rename Here…" 'racket-rename-symbol)
          (command-item racket-menu "Macroexpand" 'expand-macro-once)
          (command-item racket-menu "Expand in Place" 'macrostep-expand-in-place)
          (when (macrosteps buffer)
            (command-item racket-menu "Collapse Expansion" 'macrostep-collapse)
            (command-item racket-menu "Collapse All Expansions" 'macrostep-collapse-all-expansions))
          (command-item racket-menu "Check Syntax" 'racket-check-syntax)
          (command-item racket-menu "raco test" 'raco-test)
          (gio:menu-append-submenu more "Racket" racket-menu)))
      (when (buffer-file buffer)
        (let ((git (gio:menu-new)))
          (command-item git "Blame" 'toggle-blame)
          (command-item git "File History" 'show-file-history)
          (command-item git "Changes Since the Last Commit" 'git-diff-file)
          (command-item git "Stage This Change" 'git-stage-change)
          (command-item git "Revert This Change" 'git-revert-change)
          (let ((conflict (gio:menu-new)))
            (command-item conflict "Accept Current" 'accept-current-change)
            (command-item conflict "Accept Incoming" 'accept-incoming-change)
            (command-item conflict "Accept Both" 'accept-both-changes)
            (command-item conflict "Next Conflict" 'next-conflict)
            (gio:menu-append-section git "Merge Conflicts" conflict))
          (gio:menu-append-submenu more "Git" git)))
      (let ((code (gio:menu-new)))
        (unless markdown
          (command-item code "Toggle Comment" 'toggle-comment)
          (command-item code "Format Document" 'format-document))
        (when (or lisp-family markdown (tree-sitter-buffer-p buffer))
          (let ((fold (gio:menu-new)))
            (command-item fold "Toggle Fold" 'toggle-fold)
            (command-item fold "Fold All" 'fold-all)
            (command-item fold "Unfold All" 'unfold-all)
            (gio:menu-append-section code nil fold)))
        (let ((claude (gio:menu-new)))
          (command-item claude "Ask Assistant to Change…" 'assistant-edit)
          (gio:menu-append-section code nil claude))
        (gio:menu-append-submenu more "Code" code))
      (gio:menu-append-section menu nil more))
    menu))

(defvar *context-menu-view* nil
  "The view last right-clicked: while its menu is open, the menu has the
focus, so commands chosen from it ask this which view they're for.")

(defun context-view ()
  "The view a command acts on: the one with the focus, else the one whose
right-click menu it came from, else the selected tab's."
  (or (focused-view *window*)
      (and *context-menu-view* (member (view-buffer *context-menu-view*) (buffer-list))
           *context-menu-view*)
      (current-view)))

(defun find-descendant (widget test)
  "The first widget under WIDGET, depth first, that passes TEST."
  (loop for child = (gtk:widget-get-first-child widget) then (gtk:widget-get-next-sibling child)
        while child
        do (let ((found (if (funcall test child) child (find-descendant child test))))
             (when found (return found)))))

(defun show-context-menu (view x y)
  "Pop up VIEW's right-click menu at (X, Y) in its text view."
  (let* ((text-view (view-text-view view))
         (popover (gtk:popover-menu-new-from-model (editor-context-menu (view-buffer view)))))
    (gtk:widget-set-parent popover text-view)
    (gtk:popover-set-pointing-to popover (gdk:make-rectangle :x (round x) :y (round y) :width 1 :height 1))
    (gtk:popover-set-has-arrow popover nil)
    (gtk:widget-add-css-class popover "cadre-context-menu")
    ;; Every page the height of the tallest. A menu that opened above the
    ;; pointer and then shrank for a submenu is moved by GTK to keep its
    ;; bottom there, but macOS leaves it drawn where it was, so clicks (on
    ;; the Back arrow, say) land beside what they were aimed at.
    (let ((stack (find-descendant popover (lambda (w) (typep w 'gtk:stack)))))
      (when stack (gtk:stack-set-vhomogeneous stack t)))
    (gobject:connect popover :closed (lambda (p)
                                       (glib:idle-add glib:+priority-default-idle+
                                                      (lambda () (gtk:widget-unparent p) nil))))
    (gtk:popover-popup popover)
    popover))

(defun setup-context-menu (view)
  "Give VIEW's text view Cadre's right-click menu (in place of GTK's), and
make a right click move the cursor there first (unless it is inside the
selection), so the commands act on what was clicked."
  (let ((text-view (view-text-view view))
        (click (gtk:gesture-click-new)))
    (gtk:gesture-single-set-button click 3)
    (gtk:event-controller-set-propagation-phase click :capture)
    (gobject:connect click :pressed
                     (lambda (gesture n x y)
                       (declare (ignore n))
                       ;; Claimed, so the text view doesn't show its own menu too.
                       (gtk:gesture-set-state gesture :claimed)
                       (setf *context-menu-view* view)
                       (multiple-value-bind (bx by)
                           (gtk:text-view-window-to-buffer-coords text-view :widget (round x) (round y))
                         (multiple-value-bind (ok iter) (gtk:text-view-get-iter-at-location text-view bx by)
                           (when ok
                             (let ((gtk-buffer (view-gtk-buffer view)))
                               (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
                                 (unless (and has (gtk:text-iter-in-range iter start end))
                                   (gtk:text-buffer-place-cursor gtk-buffer iter)))))))
                       (focus-view view)))
    ;; Up on the release, not the press: a menu opened above the pointer
    ;; has its last item (Code) under it, and the release would choose it.
    (gobject:connect click :released
                     (lambda (gesture n x y)
                       (declare (ignore gesture n))
                       (show-context-menu view x y)))
    (gtk:widget-add-controller text-view click)))
