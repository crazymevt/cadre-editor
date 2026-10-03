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
   #:option-name #:option-type #:option-default #:option-documentation #:option-category
   #:option-title #:option-kind #:option-value #:valid-option-value-p #:set-option-value
   #:read-option-value #:write-option-value
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
   #:*this-command* #:*last-command* #:*this-command-kind* #:*last-command-kind*
   #:*prefix-arg* #:prefix-numeric-value #:command-repeat
   ;; minor modes
   #:define-minor-mode #:find-minor-mode #:minor-mode-enabled-p #:set-minor-mode
   #:buffer-minor-mode-keymaps #:minor-mode-name #:minor-mode-title #:minor-mode-keymap
   #:minor-mode-documentation #:*minor-modes* #:*minor-mode-hook*
   ;; the kill ring and other editing
   #:*kill-ring-max* #:*kill-ring* #:*kill-hook* #:kill-new #:kill-text #:current-kill
   #:case-fold-p #:find-all #:replacement-for #:replace-all
   #:word-char-p #:word-bounds-after #:capitalize-string #:symbol-constituent-p #:dabbrev-candidates
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
   #:line-info #:line-info-highlighted #:line-info-tokens #:forget-highlighting
   #:depth-at #:context-at
   #:forward-sexp-position #:backward-sexp-position #:up-list-position #:down-list-position
   #:beginning-of-defun-position #:end-of-defun-position #:toplevel-form-bounds
   #:paren-match-at
   ;; structural editing
   #:apply-edits #:map-offset #:offset-of #:line-column-of #:enclosing-list #:sexp-at
   #:list-bounds #:lb-open-start #:lb-open-end #:lb-close-start #:lb-close-end
   #:paredit-slurp-forward #:paredit-barf-forward #:paredit-slurp-backward #:paredit-barf-backward
   #:paredit-raise #:paredit-splice #:paredit-splice-killing-backward #:paredit-splice-killing-forward
   #:paredit-wrap #:paredit-split #:paredit-join #:paredit-open #:paredit-close #:paredit-quote
   #:paredit-delete-before #:paredit-delete-after #:paredit-kill-end
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
   #:parse-compiler-notes #:parse-compilation-result #:severity-rank
   ;; inspector, cross-references, debugger
   #:inspection #:inspection-title #:inspection-parts #:inspection-next #:inspection-more
   #:inspector-segments #:parse-inspector-range #:parse-inspection
   #:*xref-kinds* #:xref-kind-heading #:xref #:make-xref #:xref-kind #:xref-name #:xref-location
   #:parse-xrefs #:parse-xrefs-groups
   #:frame #:make-frame #:frame-number #:frame-description #:frame-restartable
   #:parse-frames #:parse-frame-locals
   ;; what the image knows
   #:*image-classes* #:classifiable-name-p #:image-classify-source #:parse-image-classes
   #:surely-called-p #:local-function-names #:evaluated-place-p #:cl-function-p
   #:symbol-base-name #:token-text
   ;; JSON
   #:jobj #:jget #:jtrue-p #:jlist #:jobject-p #:read-json #:parse-json #:write-json #:json-string
   #:json-error
   ;; the Claude Code CLI
   #:*claude-program* #:*claude-model* #:*claude-effort* #:*claude-isolated*
   #:make-uuid #:random-token #:find-claude-program #:check-claude
   #:claude-status #:claude-status-program #:claude-status-version #:claude-status-logged-in
   #:claude-status-auth-method #:claude-status-error #:claude-status-detail
   #:claude-arguments #:user-message-line #:parse-claude-event #:content-text #:tool-display-name
   #:claude-event #:event-kind #:event-text #:event-name #:event-id #:event-input #:event-error
   #:event-cost #:event-duration #:event-session-id #:event-model #:event-data #:event-subagent
   #:start-claude #:claude-alive-p #:claude-send #:claude-interrupt #:stop-claude
   #:claude-process #:cp-session-id #:cp-process #:cp-stopped
   ;; MCP
   #:define-mcp-tool #:register-mcp-tool #:*mcp-tools* #:mcp-tool-name #:tool-error #:tool-argument
   #:mcp-tool-error #:mcp-handle #:call-mcp-tool #:start-mcp-server #:stop-mcp-server
   #:mcp-server-url #:mcp-server-port #:mcp-server-token #:write-mcp-config #:http-post-json
   ;; diffs
   #:split-lines #:diff-lines #:diff-stats #:replace-unique))
