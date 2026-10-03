;;;; package.lisp — the CADRE package: the editor model, independent of any GUI

(defpackage #:cadre
  (:use #:cl)
  (:export
   ;; hooks
   #:define-hook #:add-hook #:remove-hook #:run-hook
   #:*buffer-created-hook* #:*buffer-killed-hook* #:*before-save-hook* #:*after-save-hook*
   #:*after-command-hook*
   ;; options
   #:define-option #:find-option #:list-options
   #:option-name #:option-type #:option-default #:option-documentation
   ;; text protocol
   #:text-length #:text-string #:text-insert #:text-delete #:text-char
   #:text-point #:text-modified-p #:text-replace-contents
   #:string-text #:make-string-text
   ;; modes
   #:define-major-mode #:find-major-mode #:major-mode-for-file
   #:major-mode-name #:major-mode-title #:major-mode-keymap #:major-mode-extensions
   #:fundamental-mode #:lisp-mode
   ;; buffers
   #:buffer #:make-buffer #:kill-buffer #:buffer-list #:find-buffer #:find-file-buffer
   #:buffer-name #:buffer-file #:buffer-text #:buffer-major-mode #:buffer-local
   #:buffer-modified-p #:buffer-string #:buffer-length #:buffer-point
   #:buffer-insert #:buffer-delete #:buffer-display-name
   ;; editor and frontends
   #:*frontend* #:frontend-current-buffer #:frontend-message
   #:current-buffer #:message #:editor-error #:editor-error-message
   ;; commands
   #:define-command #:find-command #:list-commands #:run-command #:command-applicable-p
   #:command-name #:command-documentation #:command-modes #:command-title
   #:*this-command* #:*last-command*
   ;; keymaps
   #:keymap #:make-keymap #:keymap-name #:bind-key #:unbind-key #:keymap-lookup
   #:lookup-keys #:parse-keys #:canonical-key #:keys-string #:make-key
   #:key-dispatcher #:make-key-dispatcher #:dispatch-key #:dispatcher-pending
   #:reset-dispatcher #:where-is
   ;; text lines
   #:text-line-count #:text-line-string #:text-line-position #:text-position-line
   ;; fuzzy matching
   #:fuzzy-match #:fuzzy-filter
   ;; Lisp syntax
   #:lex-line #:terminating-char-p #:whitespace-char-p #:token #:token-type #:token-start #:token-end #:token-depth #:token-subtype
   #:make-lisp-syntax #:reset-syntax #:syntax-text #:syntax-line-count #:syntax-lines-changed
   #:ensure-lexed #:line-tokens #:line-start-state #:line-end-state
   #:line-info #:line-info-highlighted #:line-info-tokens
   #:depth-at #:context-at
   #:forward-sexp-position #:backward-sexp-position #:up-list-position #:down-list-position
   #:beginning-of-defun-position #:end-of-defun-position #:toplevel-form-bounds
   #:paren-match-at
   #:lisp-indentation #:define-indentation #:indentation-spec
   #:token-face #:*faces* #:*paren-face-count*
   ;; Swank
   #:remote-symbol #:remote-symbol-p #:remote-symbol-name #:remote-symbol= #:write-sexp #:read-sexp
   #:sexp-to-string #:swank-connection #:swank-connect #:swank-disconnect #:swank-rex
   #:swank-send #:swank-call #:swank-interrupt #:*swank-trace* #:swank-eval-sync #:swank-start-session
   #:connection-open-p #:connection-info #:connection-package #:connection-prompt
   #:connection-handler #:connection-state #:connection-process #:connection-implementation
   #:connection-host #:connection-port #:*swank-contribs*
   #:*lisp-command* #:*swank-source* #:*swank-startup-timeout*
   #:start-inferior-lisp #:kill-inferior-lisp #:inferior-alive-p #:inferior-process
   #:buffer-package-name #:symbol-at #:symbol-prefix-at #:raw-form-at #:parse-location
   #:compiler-note #:compiler-note-severity #:compiler-note-message #:compiler-note-location
   #:parse-compiler-notes #:parse-compilation-result #:severity-rank))
