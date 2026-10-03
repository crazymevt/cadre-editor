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
           #:forward-word #:backward-word #:beginning-of-line #:end-of-line
           #:scroll-down-page #:scroll-up-page #:beginning-of-buffer #:end-of-buffer
           #:set-mark #:delete-char #:kill-line #:cut #:copy #:paste #:undo #:redo))

(defpackage #:cadre-user
  (:use #:cl #:cadre #:cadre-ui)
  (:documentation "The package your init file is read in."))
