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
   #:reset-dispatcher #:where-is))
