;;;; terminal.lisp — the parts of the Terminal page that don't need GTK
;;;;
;;;; The UI (ui/terminal.lisp) puts a VTE widget in the panel; this decides
;;;; what it runs and with what environment, which keys go to the terminal
;;;; and which to Cadre, and what a clicked "file:line:column" names.

(in-package #:cadre)

(define-option *terminal-shell* nil (or null string)
  "The shell the Terminal page runs. Nil means $SHELL, or /bin/sh."
  :category "Terminal")

(defun terminal-shell-command (&key (shell *terminal-shell*) (environment-shell (uiop:getenv "SHELL"))
                                    (login (member :darwin *features*)))
  "The argument list that starts the terminal's shell: SHELL, else the
user's $SHELL, else /bin/sh. On macOS, as a login shell (as Terminal.app
starts it), so the profile files set up PATH."
  (let ((program (cond ((plusp (length shell)) shell)
                       ((plusp (length environment-shell)) environment-shell)
                       (t "/bin/sh"))))
    (if login (list program "-l") (list program))))

(defparameter *terminal-environment*
  '(("TERM" . "xterm-256color") ("COLORTERM" . "truecolor") ("TERM_PROGRAM" . "Cadre"))
  "Variables every terminal gets, replacing what Cadre's own environment has.")

(defun terminal-environment (environment &optional (overrides *terminal-environment*))
  "ENVIRONMENT (a list of \"NAME=value\" strings) with OVERRIDES (an alist)
put in place of any variables of the same names."
  (append (loop for (name . value) in overrides collect (format nil "~a=~a" name value))
          (remove-if (lambda (entry)
                       (let ((name (subseq entry 0 (or (position #\= entry) (length entry)))))
                         (assoc name overrides :test #'string=)))
                     environment)))

(defun terminal-title-for (command)
  "A short name for a terminal running COMMAND (an argument list): the program's file name."
  (let ((program (first command)))
    (subseq program (1+ (or (position #\/ program :from-end t) -1)))))

;;; Keys

(defparameter *terminal-default-editor-keys*
  '((:standard "C-`" "C-~" "C-S-p" "F1")
    (:emacs "C-`" "C-~" "C-x" "M-x"))
  "For each keybinding profile, the keys that act as commands even in a terminal.")

(defun key-parts (key)
  "KEY's modifier letters and its name: \"C-S-p\" → (#\\C #\\S) and \"p\"."
  (let ((modifiers (key-modifiers-of key)))
    (values modifiers (subseq key (* 2 (length modifiers))))))

(defun key-modifiers-of (key)
  (loop for i from 0 by 2
        while (and (< (+ i 2) (length key)) (char= (char key (1+ i)) #\-))
        collect (char key i)))

(defun meta-key-bytes (key)
  "The bytes a terminal expects for Meta with KEY's key (ESC, then the
key's own), when that key is Meta plus a character, DEL, or ← →; else nil.
For Option as Meta on macOS."
  (multiple-value-bind (modifiers name) (key-parts key)
    (when (and (member #\M modifiers) (not (intersection modifiers '(#\C #\s))))
      (let ((escape (string (code-char 27))))
        (cond ((string= name "DEL") (concatenate 'string escape (string (code-char 127))))
              ((string= name "Left") (concatenate 'string escape "b"))
              ((string= name "Right") (concatenate 'string escape "f"))
              ((string= name "SPC") (concatenate 'string escape " "))
              ((= (length name) 1)
               (concatenate 'string escape
                            (string (if (member #\S modifiers) (char-upcase (char name 0)) (char name 0))))))))))

(defun terminal-key-action (key &key macos editor-keys option-as-meta)
  "What a key press in a terminal does. KEY is the canonical key, with ⌘ as
s- (not turned into C-). One of:
  :editor     Cadre runs it as a command
  :terminal   the terminal gets it
  :copy :paste :select-all :clear
  (:send BYTES)  write BYTES to the terminal instead (⌘← and Option as Meta)."
  (multiple-value-bind (modifiers name) (key-parts key)
    (cond
      ;; ⌘ keys belong to Cadre, but for the usual terminal ones.
      ((and macos (member #\s modifiers))
       (let ((plain (remove #\s modifiers)))
         (cond ((and (null plain) (string= name "c")) :copy)
               ((and (null plain) (string= name "v")) :paste)
               ((and (null plain) (string= name "a")) :select-all)
               ((and (null plain) (string= name "k")) :clear)
               ((and (null plain) (string= name "Left")) (list :send (string (code-char 1))))
               ((and (null plain) (string= name "Right")) (list :send (string (code-char 5))))
               ((and (null plain) (string= name "DEL")) (list :send (string (code-char 21))))
               (t :editor))))
      ((and (not macos) (equal modifiers '(#\C #\S)) (string= name "c")) :copy)
      ((and (not macos) (equal modifiers '(#\C #\S)) (string= name "v")) :paste)
      ((member key editor-keys :test #'string=) :editor)
      ((and option-as-meta (meta-key-bytes key)) (list :send (meta-key-bytes key)))
      (t :terminal))))

;;; File references, as compilers and tests print them

(defun parse-file-reference (text)
  "The file, line and column (1-based; nil when absent) TEXT names, as in
\"src/a.lisp:12:5\", \"/tmp/x.c:3\" or \"README.md\". Quotes, brackets and
trailing punctuation around it are dropped. Nil if TEXT is a URL or empty."
  (let ((text (string-trim "\"'`()[]<>{},;. " text)))
    (unless (or (zerop (length text)) (search "://" text))
      (multiple-value-bind (match groups)
          (cl-ppcre:scan-to-strings "^(.*?)(?::(\\d+))?(?::(\\d+))?:?$" text)
        (when (and match (plusp (length (aref groups 0))))
          (values (aref groups 0)
                  (and (aref groups 1) (parse-integer (aref groups 1)))
                  (and (aref groups 2) (parse-integer (aref groups 2)))))))))

(defun resolve-file-reference (file directories)
  "FILE (as parse-file-reference gives it) as an existing file's pathname:
absolute, from ~, or relative to the first of DIRECTORIES that has it."
  (flet ((existing (path) (let ((p (probe-file path))) (and p (not (uiop:directory-pathname-p p)) p))))
    (cond ((uiop:string-prefix-p "~/" file)
           (existing (merge-pathnames (subseq file 2) (user-homedir-pathname))))
          ((uiop:string-prefix-p "/" file) (existing (uiop:parse-native-namestring file)))
          (t (loop for directory in directories
                   thereis (and directory
                                (existing (merge-pathnames (uiop:parse-native-namestring file)
                                                           (uiop:ensure-directory-pathname directory)))))))))
