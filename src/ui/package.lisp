;;;; package.lisp — CADRE-UI: the GTK 4 / libadwaita interface

(defpackage #:cadre-ui
  (:use #:cl #:cadre)
  (:export #:main
           ;; options
           #:*keybinding-profile* #:*layout* #:*auto-vertical-min-width*
           #:*editor-font* #:*explorer-hidden-names* #:*explorer-hidden-types*
           ;; keymaps
           #:*standard-global-keymap* #:*standard-editing-keymap*
           #:*emacs-global-keymap* #:*emacs-editing-keymap*
           ;; for scripts and the init file
           #:*window* #:open-file-path #:open-project #:current-view
           #:view-buffer #:view-text-view #:call-command
           ;; commands
           #:new-file #:open-file #:open-folder #:save-buffer #:save-buffer-as #:save-all
           #:close-tab #:next-tab #:previous-tab
           #:toggle-sidebar #:toggle-panel #:toggle-layout #:use-automatic-layout #:show-output
           #:use-standard-keys #:use-emacs-keys #:keyboard-quit #:quit
           #:forward-char #:backward-char #:next-line #:previous-line
           #:forward-word #:backward-word #:beginning-of-line #:select-to-beginning-of-line #:end-of-line
           #:scroll-down-page #:scroll-up-page #:beginning-of-buffer #:end-of-buffer
           #:set-mark #:delete-char #:kill-line #:cut #:copy #:paste #:undo #:redo
           #:forward-sexp #:backward-sexp #:backward-up-list #:down-list
           #:beginning-of-defun #:end-of-defun #:mark-sexp
           #:indent-line #:newline-and-indent #:indent-region #:indent-defun
           #:execute-command #:quick-open #:switch-to-buffer #:go-to-line
           #:find-text #:find-next #:find-previous
           #:lisp #:connect #:disconnect #:restart-lisp #:interrupt-lisp
           #:*connection* #:with-connection
           #:show-repl #:clear-repl #:repl-return #:repl-previous-input #:repl-next-input #:repl-mode
           #:eval-last-expression #:eval-defun #:eval-region #:eval-expression-or-region
           #:compile-defun #:compile-or-eval-defun #:compile-and-load-file #:load-file #:load-project
           #:edit-definition #:pop-definition #:describe-symbol #:complete-symbol
           #:next-note #:previous-note #:clear-notes #:debugger-abort #:debugger-continue
           #:*highlight-from-image* #:show-explorer #:show-systems #:load-system
           #:inspect-value #:inspector-back #:inspector-forward #:inspector-refresh
           #:find-references #:who-calls #:who-references #:who-binds #:who-sets
           #:who-macroexpands #:who-specializes #:list-callers #:list-callees
           #:expand-macro-once #:expand-macro-all #:expand-compiler-macro
           #:claude #:chat-send #:claude-stop #:claude-new-chat #:claude-sign-in
           #:ask-claude-about-problems #:ask-claude-about-error #:accept-edit #:reject-edit))

(defpackage #:cadre-user
  (:use #:cl #:cadre #:cadre-ui)
  (:documentation "The package your init file is read in."))
