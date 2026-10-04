;;;; inferior.lisp — starting a Lisp with Cadre's bundled Swank
;;;;
;;;; The Lisp is started with a bootstrap form (--eval) that loads
;;;; vendor/slime/swank-loader.lisp, compiles Swank into Cadre's cache on
;;;; first use, and starts a server on a free port, writing the port to a
;;;; file. Cadre waits for the file, then connects. The Lisp's own output
;;;; (and its REPL on standard input) is the *inferior-lisp* output.

(in-package #:cadre)

(define-option *lisp-command* '("sbcl" "--noinform") list
  "The program and arguments that start a Lisp for M-x lisp. Swank's
bootstrap is added with --eval, so the Lisp must accept that (SBCL, CCL and
ECL do)."
  :category "Lisp")

(define-option *swank-source* :bundled (member :bundled :quicklisp)
  "Where the started Lisp loads Swank from: Cadre's bundled copy, or Quicklisp."
  :category "Lisp")

(define-option *swank-startup-timeout* 120 (integer 1)
  "Seconds to wait for a started Lisp's Swank server. The first start
compiles Swank, which takes a while."
  :category "Lisp")

(defun swank-directory ()
  (resource-pathname "vendor/slime/"))

(defun cache-directory ()
  (let ((xdg (uiop:getenv "XDG_CACHE_HOME")))
    (merge-pathnames "cadre/" (if (and xdg (plusp (length xdg)))
                                  (uiop:ensure-directory-pathname xdg)
                                  (merge-pathnames ".cache/" (user-homedir-pathname))))))

(defun lisp-string (string)
  "STRING as a Lisp string literal."
  (with-output-to-string (s) (write-sexp string s)))

(defun bootstrap-form (port-file)
  "Lisp source that starts Swank and writes its port to PORT-FILE. ASDF is
loaded first, on the main thread: requests arrive on several threads at once,
and two of them loading ASDF together leave it half loaded."
  (ecase *swank-source*
    (:bundled
     (format nil "(progn (ignore-errors (require \"ASDF\")) (load ~a) (setf (symbol-value (find-symbol \"*FASL-DIRECTORY*\" \"SWANK-LOADER\")) ~a) (funcall (find-symbol \"INIT\" \"SWANK-LOADER\")) (funcall (find-symbol \"START-SERVER\" \"SWANK\") ~a :dont-close t))"
             (lisp-string (namestring (merge-pathnames "swank-loader.lisp" (swank-directory))))
             (lisp-string (namestring (merge-pathnames "swank-fasl/" (cache-directory))))
             (lisp-string (namestring port-file))))
    (:quicklisp
     (format nil "(progn (ignore-errors (require \"ASDF\")) (funcall (find-symbol \"QUICKLOAD\" \"QL\") :swank) (funcall (find-symbol \"START-SERVER\" \"SWANK\") ~a :dont-close t))"
             (lisp-string (namestring port-file))))))

(defstruct (inferior-lisp (:conc-name inferior-))
  process port-file output-thread)

(defun read-port-file (file)
  (ignore-errors
   (with-open-file (in file :if-does-not-exist nil)
     (and in (let ((line (read-line in nil)))
               (and line (parse-integer line :junk-allowed t)))))))

(defun start-inferior-lisp (&key (command *lisp-command*) (deliver #'funcall)
                                 on-output on-port on-exit)
  "Start a Lisp with Swank. Calls, through DELIVER: ON-OUTPUT with each line
the Lisp prints, ON-PORT with its Swank port once it is listening, and
ON-EXIT with a message if it exits first or takes too long."
  (let* ((port-file (merge-pathnames (format nil "swank-port-~d-~d" (sb-posix:getpid) (random 1000000))
                                     (uiop:temporary-directory)))
         (process (progn
                    (ignore-errors (delete-file port-file))
                    (sb-ext:run-program (first command)
                                        (append (rest command) (list "--eval" (bootstrap-form port-file)))
                                        :search t :wait nil
                                        :input :stream :output :stream :error :output
                                        :external-format :utf-8)))
         (inferior (make-inferior-lisp :process process :port-file port-file)))
    (setf (inferior-output-thread inferior)
          (sb-thread:make-thread
           (lambda ()
             (let ((stream (sb-ext:process-output process)))
               (loop for line = (ignore-errors (read-line stream nil))
                     while line
                     do (let ((line line)) (funcall deliver (lambda () (when on-output (funcall on-output line))))))))
           :name "inferior lisp output"))
    (sb-thread:make-thread
     (lambda ()
       (loop with deadline = (+ (get-universal-time) *swank-startup-timeout*)
             for port = (read-port-file port-file)
             do (cond (port
                       (ignore-errors (delete-file port-file))
                       (funcall deliver (lambda () (when on-port (funcall on-port port))))
                       (return))
                      ((not (sb-ext:process-alive-p process))
                       (funcall deliver (lambda () (when on-exit
                                                     (funcall on-exit (format nil "The Lisp exited (status ~a) before Swank started."
                                                                              (sb-ext:process-exit-code process))))))
                       (return))
                      ((> (get-universal-time) deadline)
                       (funcall deliver (lambda () (when on-exit (funcall on-exit "Swank did not start in time."))))
                       (return)))
                (sleep 0.1)))
     :name "swank port watcher")
    inferior))

(defun inferior-alive-p (inferior)
  (and inferior (sb-ext:process-alive-p (inferior-process inferior))))

(defun kill-inferior-lisp (inferior)
  "Stop the inferior Lisp."
  (when (inferior-alive-p inferior)
    (ignore-errors (close (sb-ext:process-input (inferior-process inferior))))
    (sb-ext:process-kill (inferior-process inferior) 15)
    (sb-ext:process-wait (inferior-process inferior) t)))
