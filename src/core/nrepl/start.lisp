;;;; start.lisp — starting a Clojure REPL for a project, as CIDER's jack-in does
;;;;
;;;; The project is the nearest folder above a file with deps.edn,
;;;; project.clj or bb.edn. Its REPL is started with nREPL and cider-nrepl
;;;; added (unless *cider-nrepl* is off), on a free port, in the project's
;;;; folder; Cadre reads the port from what the server prints. nREPL and
;;;; cider-nrepl come from Maven (Clojars) the first time, at the versions
;;;; below. A folder with none of those files gets a plain `clojure` REPL.

(in-package #:cadre)

(define-option *clojure-command* "clojure" string
  "The Clojure CLI (for deps.edn projects, and folders that aren't projects)."
  :category "Clojure")

(define-option *leiningen-command* "lein" string
  "Leiningen, for project.clj projects."
  :category "Clojure")

(define-option *babashka-command* "bb" string
  "Babashka, for bb.edn projects."
  :category "Clojure")

(define-option *clojure-aliases* "" string
  "Aliases to start a deps.edn project's REPL with, such as \":dev:test\"."
  :category "Clojure")

(define-option *cider-nrepl* t boolean
  "Add cider-nrepl to the REPLs Cadre starts. It gives go to definition,
cross-references and better documentation and completion."
  :category "Clojure")

(define-option *nrepl-version* "1.7.0" string
  "The nREPL version Cadre adds to the REPLs it starts."
  :category "Clojure")

(define-option *cider-nrepl-version* "0.62.2" string
  "The cider-nrepl version Cadre adds to the REPLs it starts."
  :category "Clojure")

(define-option *clojure-startup-timeout* 300 (integer 1)
  "Seconds to wait for a Clojure REPL to start. The first start downloads
nREPL and cider-nrepl, and a project's dependencies."
  :category "Clojure")

(defparameter *clojure-project-files*
  '(("deps.edn" . :deps) ("project.clj" . :lein) ("bb.edn" . :babashka))
  "Files that make a folder a Clojure project, and the kind each makes.")

(defun clojure-project (start)
  "The Clojure project START (a file or folder) is in: its folder and kind
(:deps, :lein or :babashka), or nil."
  (loop for dir = (uiop:pathname-directory-pathname start) then (uiop:pathname-parent-directory-pathname dir)
        repeat 50
        do (loop for (file . kind) in *clojure-project-files*
                 when (probe-file (merge-pathnames file dir))
                   do (return-from clojure-project (values dir kind)))
           (when (equal dir (uiop:pathname-parent-directory-pathname dir)) (return nil))))

(defun clojure-command (kind)
  "The program and arguments that start a KIND of project's REPL with an
nREPL server on a free port (nil KIND: no project)."
  (let ((deps (format nil "{:deps {nrepl/nrepl {:mvn/version ~s}~:[~; cider/cider-nrepl {:mvn/version ~s}~]}}"
                      *nrepl-version* *cider-nrepl* *cider-nrepl-version*))
        (middleware (and *cider-nrepl* (list "--middleware" "[cider.nrepl/cider-middleware]"))))
    (ecase kind
      ((:deps nil)
       (append (list *clojure-command* "-Sdeps" deps
                     (format nil "-M~a" (string-trim " " *clojure-aliases*))
                     "-m" "nrepl.cmdline")
               middleware
               (list "--port" "0")))
      (:lein
       (append (list *leiningen-command*)
               (and *cider-nrepl*
                    (list "update-in" ":plugins" "conj"
                          (format nil "[cider/cider-nrepl ~s]" *cider-nrepl-version*) "--"))
               (list "repl" ":headless" ":port" "0")))
      (:babashka (list *babashka-command* "nrepl-server" "0")))))

(defun nrepl-port-in (line)
  "The port in a line where an nREPL server says it started, or nil:
\"nREPL server started on port 54321 …\" (nREPL, Leiningen) or
\"Started nREPL server at 127.0.0.1:1667\" (Babashka)."
  (flet ((number-after (marker)
           (let ((at (search marker line)))
             (and at (parse-integer line :start (+ at (length marker)) :junk-allowed t)))))
    (cond ((search "nREPL server started on port " line) (number-after "nREPL server started on port "))
          ((search "Started nREPL server at " line)
           (let ((colon (position #\: line :from-end t)))
             (and colon (parse-integer line :start (1+ colon) :junk-allowed t)))))))

(defstruct (clojure-process (:conc-name clojure-process-)) process directory kind)

(defun start-clojure (directory kind &key (deliver #'funcall) on-output on-port on-exit)
  "Start the REPL of the KIND of project in DIRECTORY. Calls, through
DELIVER: ON-OUTPUT with each line it prints, ON-PORT with its nREPL port, and
ON-EXIT with a message if it exits first or takes too long."
  (let* ((command (clojure-command kind))
         (process (handler-case
                      (sb-ext:run-program (first command) (rest command)
                                          :search t :wait nil :directory (uiop:native-namestring directory)
                                          :input :stream :output :stream :error :output
                                          :external-format :utf-8)
                    (error (e) (editor-error "Could not start ~a: ~a" (first command) e))))
         (started (make-clojure-process :process process :directory directory :kind kind))
         (port-found nil))
    (sb-thread:make-thread
     (lambda ()
       (let ((stream (sb-ext:process-output process)))
         (loop for line = (ignore-errors (read-line stream nil))
               while line
               do (let ((line line) (port (and (not port-found) (nrepl-port-in line))))
                    (funcall deliver (lambda () (when on-output (funcall on-output line))))
                    (when port
                      (setf port-found t)
                      (funcall deliver (lambda () (when on-port (funcall on-port port)))))))
         (unless port-found
           (setf port-found t)
           (funcall deliver (lambda ()
                              (when on-exit
                                (funcall on-exit (format nil "~a exited before its nREPL server started."
                                                         (first command)))))))))
     :name "clojure output")
    (sb-thread:make-thread
     (lambda ()
       (sleep *clojure-startup-timeout*)
       (unless port-found
         (setf port-found t)
         (funcall deliver (lambda () (when on-exit (funcall on-exit "The Clojure REPL did not start in time."))))))
     :name "clojure startup timeout")
    started))

(defun stop-clojure (started)
  "Stop a REPL START-CLOJURE started."
  (let ((process (clojure-process-process started)))
    (when (sb-ext:process-alive-p process)
      (ignore-errors (close (sb-ext:process-input process)))
      (ignore-errors (sb-ext:process-kill process 15))
      (loop repeat 40 while (sb-ext:process-alive-p process) do (sleep 0.05))
      (when (sb-ext:process-alive-p process) (ignore-errors (sb-ext:process-kill process 9))))))

(defun nrepl-port-file (directory)
  "The port in DIRECTORY's .nrepl-port (written by a running nREPL), or nil."
  (read-port-file (merge-pathnames ".nrepl-port" directory)))

;;; clj-kondo, when it is installed

(define-option *clj-kondo-command* "clj-kondo" string
  "clj-kondo, the Clojure linter. When it is installed, saving a Clojure
file lints it and its findings go to the Problems page."
  :category "Clojure")

(defun clj-kondo-arguments (file)
  "The arguments that lint a Clojure file's text, given on standard input as FILE."
  (list "--lint" "-" "--filename" file))

(defun clj-kondo-notes (output file)
  "Compiler notes for the findings in clj-kondo's OUTPUT, lines such as
\"src/a.clj:3:5: warning: unused binding x\", about FILE."
  (loop for line in (uiop:split-string output :separator '(#\Newline))
        for match = (nth-value 1 (ppcre:scan-to-strings "^(.*):(\\d+):(\\d+): (error|warning|info): (.*)$" line))
        when match
          collect (make-compiler-note (cdr (assoc (aref match 3) '(("error" . :error) ("warning" . :warning)
                                                                   ("info" . :note))
                                                  :test #'string=))
                                      (aref match 4)
                                      (list :file file
                                            :line (max 0 (1- (parse-integer (aref match 1))))
                                            :column (max 0 (1- (parse-integer (aref match 2))))))))
