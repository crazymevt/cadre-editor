(in-package #:cadre-tests)

;;; The Terminal page's decisions (core terminal.lisp)

(define-test terminal-shell :parent cadre-tests
  (is equal '("/bin/zsh" "-l") (c:terminal-shell-command :shell nil :environment-shell "/bin/zsh" :login t))
  (is equal '("/usr/local/bin/fish") (c:terminal-shell-command :shell "/usr/local/bin/fish" :environment-shell "/bin/zsh" :login nil))
  (is equal '("/bin/sh") (c:terminal-shell-command :shell "" :environment-shell nil :login nil))
  (is string= "zsh" (c:terminal-title-for '("/bin/zsh" "-l")))
  (is string= "claude" (c:terminal-title-for '("claude" "--mcp-config" "x"))))

(define-test terminal-environment :parent cadre-tests
  (let ((env (c:terminal-environment '("PATH=/bin" "TERM=dumb" "HOME=/Users/x" "COLORTERM=")
                                     '(("TERM" . "xterm-256color") ("COLORTERM" . "truecolor")))))
    (is equal '("TERM=xterm-256color" "COLORTERM=truecolor" "PATH=/bin" "HOME=/Users/x") env))
  ;; A variable that only starts with an overridden name stays.
  (is equal '("TERM=xterm-256color" "TERMINFO=/x")
      (c:terminal-environment '("TERMINFO=/x") '(("TERM" . "xterm-256color")))))

(define-test terminal-keys :parent cadre-tests
  (let ((standard (cdr (assoc :standard c:*terminal-default-editor-keys*)))
        (emacs (cdr (assoc :emacs c:*terminal-default-editor-keys*)))
        (esc (string (code-char 27))))
    (flet ((mac (key &optional (keys standard) meta)
             (c:terminal-key-action key :macos t :editor-keys keys :option-as-meta meta))
           (linux (key &optional (keys standard))
             (c:terminal-key-action key :macos nil :editor-keys keys)))
      ;; Control keys are the shell's.
      (is eq :terminal (mac "C-c"))
      (is eq :terminal (mac "C-r"))
      (is eq :terminal (mac "ESC"))
      (is eq :terminal (mac "TAB"))
      (is eq :terminal (linux "C-p"))
      ;; ⌘ keys are Cadre's, but for copying, pasting and clearing.
      (is eq :editor (mac "s-p"))
      (is eq :editor (mac "s-S-p"))
      (is eq :copy (mac "s-c"))
      (is eq :paste (mac "s-v"))
      (is eq :clear (mac "s-k"))
      (is eq :select-all (mac "s-a"))
      (is equal (list :send (string (code-char 1))) (mac "s-Left"))
      (is equal (list :send (string (code-char 21))) (mac "s-DEL"))
      (is eq :copy (linux "C-S-c"))
      (is eq :paste (linux "C-S-v"))
      ;; The profile's keys that stay commands.
      (is eq :editor (mac "C-`"))
      (is eq :editor (linux "F1"))
      (is eq :editor (linux "C-S-p"))
      (is eq :terminal (mac "C-x"))
      (is eq :editor (mac "C-x" emacs))
      (is eq :editor (mac "M-x" emacs t))
      ;; Option as Meta.
      (is equal (list :send (concatenate 'string esc "b")) (mac "M-b" standard t))
      (is equal (list :send (concatenate 'string esc "B")) (mac "M-S-b" standard t))
      (is equal (list :send (concatenate 'string esc (string (code-char 127)))) (mac "M-DEL" standard t))
      (is equal (list :send (concatenate 'string esc "f")) (mac "M-Right" standard t))
      (is eq :terminal (mac "M-b" standard nil))
      (is eq :terminal (mac "C-M-b" standard t)))))

(define-test login-environment :parent cadre-tests
  (let ((nul (string (code-char 0))))
    (is equal (list (cons "PATH" "/opt/homebrew/bin:/usr/bin") (cons "NOTE" (format nil "two~%lines")) (cons "EMPTY" ""))
        (c:parse-environment-block
         (concatenate 'string "Welcome! (from .zshrc)" (string #\Newline)
                      c::*environment-start-marker* (string #\Newline)
                      "PATH=/opt/homebrew/bin:/usr/bin" nul
                      (format nil "NOTE=two~%lines") nul
                      "EMPTY=" nul)))
    (false (c:parse-environment-block "no marker here")))
  ;; A real login shell, when there is one.
  (when (probe-file "/bin/sh")
    (let ((env (c:login-shell-environment :shell "/bin/sh" :timeout 10)))
      (true (assoc "PATH" env :test #'string=)))))

(define-test resource-paths :parent cadre-tests
  (true (probe-file (c:resource-pathname "vendor/slime/swank-loader.lisp")))
  (let ((c:*resource-directory* #p"/Applications/Cadre.app/Contents/Resources/cadre/"))
    (is equal #p"/Applications/Cadre.app/Contents/Resources/cadre/icons/" (c:resource-pathname "icons/"))))

(define-test file-references :parent cadre-tests
  (flet ((ref (text) (multiple-value-list (c:parse-file-reference text))))
    (is equal '("src/a.lisp" 12 5) (ref "src/a.lisp:12:5"))
    (is equal '("src/a.lisp" 12 5) (ref "src/a.lisp:12:5:"))
    (is equal '("/tmp/x.c" 3 nil) (ref "/tmp/x.c:3"))
    (is equal '("README.md" nil nil) (ref "README.md"))
    (is equal '("tests/t.lisp" 40 nil) (ref "(tests/t.lisp:40)"))
    (is equal '("a.js" 1 2) (ref "\"a.js:1:2\","))
    (is equal '(nil) (ref "https://example.com/a.html"))
    (is equal '(nil) (ref "")))
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "cadre-terminal-test-~d/" (random 1000000)) (uiop:temporary-directory))))
         (file (merge-pathnames "src/a.lisp" dir)))
    (ensure-directories-exist file)
    (with-open-file (o file :direction :output :if-exists :supersede) (write-line "x" o))
    (unwind-protect
         (progn
           (is equal (truename file) (c:resolve-file-reference "src/a.lisp" (list "/nonexistent/" dir)))
           (is equal (truename file) (c:resolve-file-reference (namestring (truename file)) '()))
           (false (c:resolve-file-reference "src/missing.lisp" (list dir)))
           ;; A directory isn't a file to open.
           (false (c:resolve-file-reference "src" (list dir))))
      (uiop:delete-directory-tree dir :validate t))))
