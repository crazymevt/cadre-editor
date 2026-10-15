;;;; clojure.lisp — Clojure REPLs, through nREPL (core/nrepl/)
;;;;
;;;; A Clojure buffer's commands (evaluate, compile, describe, go to
;;;; definition…) use the Clojure connection, which this file starts when
;;;; the first one needs it: it connects to the project's running nREPL if
;;;; it has a .nrepl-port, or starts the project's REPL with nREPL and
;;;; cider-nrepl, as CIDER's jack-in does. The process's output goes to the
;;;; Output page. The REPL is a tab, *clojure-repl* (language-repl.lisp).

(in-package #:cadre-ui)

(define-major-mode clojure-repl-mode (:title "Clojure REPL")
  "The REPL of the connected Clojure.")

(pushnew '(clojure-repl-mode . :clojure) cadre::*mode-dialects* :test #'equal)

;; Keys not bound here come from the REPL's keymap, then Lisp mode's.
(setf (cadre::keymap-parent (major-mode-keymap (find-major-mode 'clojure-repl-mode)))
      (major-mode-keymap (find-major-mode 'repl-mode)))

(defvar *clojure-process* nil "The Clojure REPL Cadre started, or nil.")
(defvar *clojure-waiting* '() "Functions waiting for the Clojure connection to be ready.")

(defun clojure-connection () (language-connection :clojure))

;;; The REPL tab (language-repl.lisp)

(define-language-repl :clojure :mode 'clojure-repl-mode :buffer-name "*clojure-repl*" :title "Clojure"
                               :prompt-format "~a=> ")

(defun clojure-repl () (language-repl :clojure))

(defun show-clojure-repl (&key (focus t)) (show-language-repl :clojure :focus focus))

(defun handle-clojure-event (connection event)
  (handle-language-event :clojure connection event))

;;; Starting and connecting

(defun clojure-start-directory ()
  "Where the current file's Clojure project is, and its kind; else the open folder."
  (let* ((view (and *window* (current-view-or-nil *window*)))   ; none when no file is open
         (file (and view (buffer-file (view-buffer view))))
         (start (or file (window-project *window*) (user-homedir-pathname))))
    (multiple-value-bind (dir kind) (clojure-project start)
      (if dir
          (values dir kind)
          (values (uiop:pathname-directory-pathname start) nil)))))

(defun clojure-ready (connection)
  (setf (gethash :clojure *connections*) connection
        *clojure-starting* nil)
  (language-repl-connected :clojure connection)
  (update-connection-status)
  (refresh-project-languages)
  (message "Connected to ~a" (connection-implementation connection))
  (let ((waiting (reverse *clojure-waiting*)))
    (setf *clojure-waiting* '())
    (dolist (f waiting)
      (handler-case (funcall f connection)
        (editor-error (e) (message "~a" (editor-error-message e)))))))

(defun clojure-failed (why)
  (setf *clojure-starting* nil *clojure-waiting* '())
  (update-connection-status)
  (refresh-project-languages)
  (message "~a" why))

(defun connect-clojure (host port)
  (setf *clojure-starting* t)
  (update-connection-status)
  (handler-case
      (nrepl-connect host port :deliver #'deliver-to-gui :handler 'handle-clojure-event
                               :on-ready 'clojure-ready
                               :on-failure (lambda (reason) (clojure-failed (format nil "nREPL failed: ~a" reason))))
    (editor-error (e) (clojure-failed (editor-error-message e)))))

(defvar *clojure-project-aliases* (make-hash-table :test 'equal)
  "Project folder (FOLDER-KEY) → the aliases ticked on its Project page,
which its REPL starts with (instead of *clojure-aliases*).")

(defun folder-key (directory)
  "DIRECTORY's real path, as a string: the same however it was reached."
  (namestring (or (probe-file directory) directory)))

(defvar *clojure-repl-project* nil
  "The project the Clojure REPL runs: (folder kind deps-signature), the
signature being its dependencies when the REPL started (or last loaded them).")

(defun start-clojure-connection (then &key directory kind)
  "Start or connect to the Clojure REPL of the current file's project (or
the project in DIRECTORY, of KIND), then call THEN with the connection."
  (when then (push then *clojure-waiting*))
  (unless *clojure-starting*
    (multiple-value-bind (dir kind) (if directory (values directory kind) (clojure-start-directory))
      (setf *clojure-repl-project*
            (and kind (list dir kind (ignore-errors (clojure-deps-signature (read-clojure-project-file dir kind))))))
      (let ((running (nrepl-port-file dir))
            (*clojure-aliases* (multiple-value-bind (aliases set) (gethash (folder-key dir) *clojure-project-aliases*)
                                 (if set (format nil "~{:~a~}" aliases) *clojure-aliases*))))
        (if (and running (ignore-errors (sb-bsd-sockets:socket-close
                                         (let ((s (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
                                           (sb-bsd-sockets:socket-connect s #(127 0 0 1) running)
                                           s))
                                        t))
            (progn (message "Connecting to the nREPL running in ~a" (uiop:native-namestring dir))
                   (connect-clojure "localhost" running))
            (let ((command (clojure-command kind)))
              (setf *clojure-starting* t)
              (update-connection-status)
              (refresh-project-languages)
              (message "Starting ~a in ~a…" (first command) (uiop:native-namestring dir))
              (panel-log (window-panel *window*) (format nil "Starting ~{~a~^ ~} in ~a" command (uiop:native-namestring dir)))
              (handler-case
                  (setf *clojure-process*
                        (start-clojure dir kind
                                       :deliver #'deliver-to-gui
                                       :on-output (lambda (line) (panel-log-raw (window-panel *window*) line))
                                       :on-port (lambda (port) (connect-clojure "localhost" port))
                                       :on-exit #'clojure-failed))
                (editor-error (e) (clojure-failed (editor-error-message e))))))))))

(setf (gethash :clojure *connection-starters*) 'start-clojure-connection)

(defun stop-clojure-connection ()
  (let ((connection (clojure-connection)))
    (when connection (backend-disconnect connection)))
  (when *clojure-process*
    (stop-clojure *clojure-process*)
    (setf *clojure-process* nil)))

;;; Evaluating code from a file needs its namespace to exist, which it
;;; doesn't until the file's (ns …) form has been evaluated once.

(defvar *loaded-namespaces* (make-hash-table :test 'equal)
  "Namespaces the Clojure connection has: (connection . name) → t.")

(defun ns-form-text (view)
  "The text of VIEW's buffer's (ns …) form, or nil."
  (let ((syntax (buffer-sexp-syntax (view-buffer view))))
    (when syntax
      (loop for line below (syntax-line-count syntax)
            for tokens = (line-tokens syntax line)
            do (loop for tk across tokens
                     when (and (eq (token-type tk) :open) (zerop (token-depth tk)))
                       do (let ((head (symbol-at syntax line (token-end tk))))
                            (when (and head (string= (cadre::base-symbol-name head #\/) "ns"))
                              (multiple-value-bind (sl sc el ec) (toplevel-form-bounds syntax line (token-start tk))
                                (return-from ns-form-text (view-region-text view sl sc el ec))))))))))

(defun call-in-file-namespace (view connection function)
  "Call FUNCTION once the namespace of the Clojure code in VIEW exists in
CONNECTION, evaluating the file's ns form first if it doesn't. Other
Lisps' code: call it at once."
  (let ((ns (and view (eq (connection-language connection) :clojure) (view-package view))))
    (if (or (null ns) (gethash (cons connection ns) *loaded-namespaces*))
        (funcall function)
        (flet ((loaded () (setf (gethash (cons connection ns) *loaded-namespaces*) t) (funcall function)))
          (request #'backend-eval connection (format nil "(some? (find-ns '~a))" ns) :package "user"
                   :on-ok (lambda (value)
                            (let ((form (ns-form-text view)))
                              (if (or (search "true" value) (null form))
                                  (loaded)
                                  (request #'backend-eval connection form :package "user"
                                           :on-ok (lambda (v) (declare (ignore v)) (loaded)))))))))))

;;; Commands

(define-command clojure-jack-in ()
  "Start the current project's Clojure REPL (with nREPL and cider-nrepl) and connect to it."
  (:title "Clojure: Jack In")
  (cond ((clojure-connection) (message "Already connected to ~a" (connection-implementation (clojure-connection))))
        (*clojure-starting* (message "Clojure is starting…"))
        (t (start-clojure-connection (lambda (c) (declare (ignore c)) (show-clojure-repl :focus nil))))))

(define-command clojure-connect ()
  "Connect to a running nREPL server, given as host:port."
  (:title "Clojure: Connect")
  (multiple-value-bind (dir) (clojure-start-directory)
    (let ((port (nrepl-port-file dir)))
      (open-picker (window-picker *window*)
                   :placeholder "host:port of an nREPL server, such as localhost:7888"
                   :text (if port (format nil "localhost:~d" port) "localhost:")
                   :on-choose (lambda (text)
                                (let* ((colon (position #\: text :from-end t))
                                       (host (if colon (subseq text 0 colon) "localhost"))
                                       (port (ignore-errors (parse-integer text :start (if colon (1+ colon) 0)))))
                                  (unless port (editor-error "Not host:port: ~a" text))
                                  (when (clojure-connection) (backend-disconnect (clojure-connection)))
                                  (connect-clojure host port)))))))

(define-command clojure-repl-tab ()
  "Show the Clojure REPL, starting Clojure if it isn't running."
  (:title "Clojure: REPL")
  (show-clojure-repl)
  (unless (or (clojure-connection) *clojure-starting*)
    (start-clojure-connection nil)))

(define-command clojure-disconnect ()
  "Disconnect from Clojure (a REPL Cadre started is stopped)."
  (:title "Clojure: Disconnect")
  (if (or (clojure-connection) *clojure-process*)
      (stop-clojure-connection)
      (message "Clojure isn't connected")))

(define-command clojure-restart ()
  "Stop the Clojure REPL Cadre started and start a fresh one."
  (:title "Clojure: Restart")
  (stop-clojure-connection)
  (start-clojure-connection nil))

(define-command clojure-set-namespace ()
  "Switch the Clojure REPL to the current file's namespace."
  (:modes clojure-mode)
  (:title "Clojure: Switch the REPL to This Namespace")
  (let ((ns (view-package (current-view))))
    (with-connection (connection :language :clojure)
      (request #'backend-set-package connection ns
               :on-ok (lambda (reply)
                        (declare (ignore reply))
                        (language-repl-note :clojure (format nil "; Namespace ~a~%" ns))
                        (update-connection-status)
                        (message "Clojure REPL in ~a" ns))))))

;;; clj-kondo on save, when it is installed

(defun clj-kondo-available-p ()
  (and (plusp (length *clj-kondo-command*))
       (ignore-errors (uiop:run-program (list *clj-kondo-command* "--version") :output nil) t)))

(defvar *clj-kondo-found* :unknown "Whether clj-kondo is installed, once looked for.")

(defun lint-clojure-buffer (buffer)
  "Lint BUFFER's text with clj-kondo, in the background, and show its
findings on the Problems page."
  (when (eq *clj-kondo-found* :unknown)
    (setf *clj-kondo-found* (clj-kondo-available-p)))
  (when (and *clj-kondo-found* (buffer-file buffer))
    (let ((file (uiop:native-namestring (buffer-file buffer)))
          (text (text-string (buffer-text buffer)))
          (directory (uiop:pathname-directory-pathname (buffer-file buffer))))
      (sb-thread:make-thread
       (lambda ()
         (let ((output (ignore-errors
                        (with-input-from-string (in text)
                          (uiop:run-program (cons *clj-kondo-command* (clj-kondo-arguments file))
                                            :input in :output :string :ignore-error-status t
                                            :directory directory)))))
           (when output
             (deliver-to-gui (lambda ()
                               (when (member buffer (buffer-list))
                                 (show-notes (clj-kondo-notes output file) :buffer buffer :replace-lines :all)))))))
       :name "clj-kondo"))))

(add-hook '*after-save-hook* (lambda (buffer)
                               (when (eq (buffer-major-mode buffer) 'clojure-mode)
                                 (lint-clojure-buffer buffer))))

;;; The project's own indentation (.cljfmt.edn), when a Clojure file opens

(add-hook '*buffer-created-hook* (lambda (buffer)
                                   (when (and (eq (buffer-major-mode buffer) 'clojure-mode) (buffer-file buffer))
                                     (ignore-errors (load-cljfmt-indentation (buffer-file buffer))))))

;;; Tests: the file's namespace's, through cider-nrepl; failures go to Problems

(define-command clojure-run-tests ()
  "Load the file and run its namespace's tests; failures and errors go to the
Problems page, at their lines."
  (:modes clojure-mode)
  (:title "Clojure: Run Tests in Namespace")
  (let* ((view (current-view))
         (buffer (view-buffer view))
         (ns (view-package view))
         (file (buffer-file buffer)))
    (unless file (editor-error "Save the buffer to a file first"))
    (flet ((run (connection)
             (unless (or (nrepl-op-p connection "test-var-query") (nrepl-op-p connection "test"))
               (editor-error "Running tests needs cider-nrepl"))
             (request #'backend-load-file connection (uiop:native-namestring file)
                      :on-ok (lambda (v)
                               (declare (ignore v))
                               (nrepl-run-tests connection ns
                                        :on-abort (lambda (reason) (message "The tests didn't run: ~a" reason))
                                        :on-done
                                        (lambda (summary failures)
                                          (show-notes
                                           (loop for f in failures
                                                 collect (make-compiler-note
                                                          (if (equal (getf f :type) "error") :error :warning)
                                                          (format nil "~a ~a~@[: ~a~]~@[~%expected: ~a~]~@[~%actual: ~a~]"
                                                                  (getf f :var) (getf f :type)
                                                                  (let ((m (getf f :message))) (and (plusp (length m)) m))
                                                                  (and (getf f :expected) (string-trim '(#\Newline) (getf f :expected)))
                                                                  (and (getf f :actual) (string-trim '(#\Newline) (getf f :actual))))
                                                          (list :file (uiop:native-namestring file)
                                                                :line (max 0 (1- (or (getf f :line) 1))) :column 0)))
                                           :buffer buffer :replace-lines :all)
                                          (message "~a: ~d test~:p, ~d passed, ~d failed, ~d error~:p"
                                                   ns (getf summary :test) (getf summary :pass)
                                                   (getf summary :fail) (getf summary :error))))))))
      (if (buffer-modified-p buffer)
          (write-buffer buffer file (lambda (ok) (when ok (with-connection (c :language :clojure) (run c)))))
          (with-connection (c :language :clojure) (run c))))))
