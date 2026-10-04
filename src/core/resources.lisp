;;;; resources.lisp — Cadre's own files, and the environment of an app
;;;;
;;;; From a source checkout, Cadre's files (icons, the bundled Swank, the
;;;; tree-sitter shim's source) are found through ASDF. The macOS app
;;;; (scripts/build-app.lisp) sets *RESOURCE-DIRECTORY* to its
;;;; Contents/Resources/cadre/ instead. An app started from the Finder or
;;;; the Dock gets launchd's bare environment (PATH /usr/bin:/bin:…), so it
;;;; takes PATH and the rest from a login shell, as a terminal would have
;;;; them: that's where Homebrew's sbcl, claude and friends are found.

(in-package #:cadre)

(defvar *resource-directory* nil
  "The folder holding Cadre's own files, or nil to find them through ASDF
(in the source checkout).")

(defun resource-pathname (relative)
  "The pathname of RELATIVE (such as \"vendor/slime/\") among Cadre's own files."
  (if *resource-directory*
      (merge-pathnames relative (uiop:ensure-directory-pathname *resource-directory*))
      (asdf:system-relative-pathname :cadre relative)))

;;; The login shell's environment

(defparameter *environment-start-marker* "__CADRE_ENVIRONMENT_START__")

(defparameter *environment-skipped-names*
  '("PWD" "OLDPWD" "SHLVL" "_" "TERM" "TERM_PROGRAM" "TERM_PROGRAM_VERSION" "TERM_SESSION_ID"
    "COLUMNS" "LINES" "PS1" "PS2")
  "Variables a shell sets for itself, not to be copied into Cadre's environment.")

(defun parse-environment-block (output)
  "The variables in OUTPUT, the text a shell printed: anything (from its
startup files), then the start marker and a newline, then NAME=value
entries each ended by a NUL (env -0). An alist of (name . value)."
  (let ((start (search *environment-start-marker* output)))
    (when start
      (let ((position (1+ (or (position #\Newline output :start start) (1- (length output))))))
        (loop for end = (position (code-char 0) output :start position)
              while end
              for entry = (subseq output position end)
              for equals = (position #\= entry)
              when (and equals (plusp equals))
                collect (cons (subseq entry 0 equals) (subseq entry (1+ equals)))
              do (setf position (1+ end)))))))

(defun login-shell-environment (&key (shell (or (uiop:getenv "SHELL") "/bin/zsh")) (timeout 10))
  "The environment SHELL sets up as an interactive login shell, as an
alist; nil if it fails or takes longer than TIMEOUT seconds."
  (ignore-errors
   (let* ((process (uiop:launch-program
                    (list shell "-l" "-i" "-c"
                          (format nil "printf '%s\\n' ~a; /usr/bin/env -0" *environment-start-marker*))
                    :output :stream :error-output nil :input nil))
          (output (make-string-output-stream))
          (reader (sb-thread:make-thread
                   (lambda ()
                     (let ((stream (uiop:process-info-output process)))
                       (loop for c = (read-char stream nil) while c do (write-char c output))))
                   :name "cadre login environment")))
     (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
           while (and (uiop:process-alive-p process) (< (get-internal-real-time) deadline))
           do (sleep 0.02))
     (when (uiop:process-alive-p process)
       (uiop:terminate-process process :urgent t))
     (sb-thread:join-thread reader :default nil :timeout 2)
     (parse-environment-block (get-output-stream-string output)))))

(defun adopt-login-shell-environment (&rest arguments)
  "Copy the login shell's variables (PATH above all) into Cadre's
environment, so programs Cadre starts are found as in a terminal. Returns
the number of variables set."
  (let ((count 0))
    (loop for (name . value) in (apply #'login-shell-environment arguments)
          unless (member name *environment-skipped-names* :test #'string=)
            do (sb-posix:setenv name value 1)
               (incf count))
    count))
