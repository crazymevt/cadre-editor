;;;; systems.lisp — the Project view in the sidebar: ASDF systems (and,
;;;; from projects.lisp, a Racket package and a Clojure project), and
;;;; loading the project into the Lisp
;;;;
;;;; The view lists the systems the open folder's .asd files define (read
;;;; from the files, so it works before any Lisp runs). With a Lisp, each
;;;; says whether it is loaded, and opening one lists its files and offers
;;;; Reload (recompile everything, notes to Problems) and Test.

(in-package #:cadre-ui)

;;; The project's systems, from its .asd files

(defun asd-system-names (file)
  "The names of the systems an .asd file defines, in lower case."
  (let ((names '()) (state '(:code 0)) (expect nil))
    (with-open-file (in file :if-does-not-exist nil)
      (when in
        (loop for line = (read-line in nil) while line
              do (multiple-value-bind (tokens end) (lex-line line state)
                   (setf state end)
                   (dolist (tk tokens)
                     (let ((text (subseq line (token-start tk) (token-end tk))))
                       (cond (expect
                              (when (member (token-type tk) '(:string :symbol :keyword))
                                (push (string-downcase (cadre::package-designator-name text)) names))
                              (setf expect nil))
                             ((and (eq (token-type tk) :symbol)
                                   (string-equal "defsystem" (symbol-base-name text)))
                              (setf expect t)))))))))
    (nreverse names)))

(defun project-system-entries (directory)
  "(name asd-file) for each system defined by .asd files in DIRECTORY (not
its subdirectories), the main system of each file first."
  (loop for file in (uiop:directory-files directory "*.asd")
        for base = (string-downcase (pathname-name file))
        for names = (asd-system-names file)
        append (mapcar (lambda (name) (list name file))
                       (cons base (remove base names :test #'string=)))))

(defun project-systems (directory)
  (mapcar #'first (project-system-entries directory)))

;;; Lisp source for ASDF. Its symbols are looked up by name: neither ASDF nor
;;; Quicklisp need be loaded when the source is read.

(defun asdf-form (body &optional directory)
  "Source that loads ASDF, lets it find DIRECTORY's systems, then runs BODY."
  (format nil "(progn (require \"ASDF\") ~@[(pushnew ~a (symbol-value (find-symbol \"*CENTRAL-REGISTRY*\" \"ASDF\")) :test (function equal)) ~]~a)"
          (and directory (cadre::lisp-string (uiop:native-namestring directory))) body))

(defun load-system-form (directory system)
  "Source that loads SYSTEM (found in DIRECTORY, if given), with Quicklisp
(fetching dependencies) if the Lisp has it."
  (asdf-form (format nil "(if (find-package \"QL\") (funcall (find-symbol \"QUICKLOAD\" \"QL\") ~a) (funcall (find-symbol \"LOAD-SYSTEM\" \"ASDF\") ~a))"
                     (cadre::lisp-string system) (cadre::lisp-string system))
             directory))

(defun test-system-form (directory system)
  (asdf-form (format nil "(funcall (find-symbol \"TEST-SYSTEM\" \"ASDF\") ~a)" (cadre::lisp-string system))
             directory))

(defun run-in-repl (note form &key on-done)
  "Show NOTE in the REPL and evaluate FORM (source) there, as if typed; then
call ON-DONE."
  (show-repl-page)
  (with-connection (connection)
    (declare (ignore connection))
    (cond ((repl-busy *repl*) (message "The REPL is busy"))
          (t (repl-fresh-line)
             (repl-insert (format nil "; ~a~%" note) "cadre-repl-note")
             (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*)
                                        (gtk:text-buffer-get-end-iter (repl-gtk-buffer)))
             (repl-eval form :on-done on-done)))))

(defun load-system-in-repl (directory system)
  "Load SYSTEM, compiling it first so its notes reach the Problems page, then
in the REPL, which also fetches what Quicklisp must."
  (setf (setting :last-system) system)
  (compile-system-then
   system
   (lambda ()
     (run-in-repl (format nil "Loading system ~a~@[ from ~a~]" system (and directory (uiop:native-namestring directory)))
                  (load-system-form directory system))
     (add-tests-tab (or directory (current-project-root))))))

(define-command load-project ()
  "Load the open folder's ASDF system into the Lisp (choosing one if there are several)."
  (let* ((directory (or (current-project-root) (editor-error "Open a folder first.")))
         (systems (or (project-systems directory)
                      (editor-error "No .asd file in ~a" (uiop:native-namestring directory))))
         (last (setting :last-system)))
    (if (null (rest systems))
        (load-system-in-repl directory (first systems))
        (open-picker (window-picker *window*)
                     :items (if (member last systems :test #'string=)
                                (cons last (remove last systems :test #'string=))
                                systems)
                     :placeholder "Load which system?"
                     :on-choose (lambda (system) (load-system-in-repl directory system))))))

;;; swank-asdf, loaded when first needed

(defvar *asdf-connection* nil "The connection swank-asdf has been loaded into.")

(defun call-with-swank-asdf (function)
  "Call FUNCTION with the connection once swank-asdf is loaded there and
ASDF can find the project's systems."
  (with-connection (connection)
    (flet ((ready ()
             (rex connection (swank-call "swank:eval-and-grab-output"
                                         (asdf-form "nil" (current-project-root)))
                  :on-ok (lambda (v) (declare (ignore v)) (funcall function connection)))))
      (if (eq *asdf-connection* connection)
          (ready)
          (rex connection (swank-call "swank:swank-require" '(:swank-asdf))
               :on-ok (lambda (v)
                        (declare (ignore v))
                        (setf *asdf-connection* connection)
                        (ready)))))))

(defmacro with-swank-asdf ((var) &body body)
  `(call-with-swank-asdf (lambda (,var) ,@body)))

;;; Compiling first, for the Problems page

;;; Loading a system in the REPL (Load, Run App) recompiles what changed,
;;; but its compiler notes are only printed there. So the system is loaded
;;; through swank first: the files recompiled get their notes on the
;;; Problems page in place of their old ones, and the REPL's load (or the
;;; app) goes on only if they compiled.

(defun files-to-compile-form (system)
  "Source for the files of SYSTEM that loading it will compile, or :UNKNOWN
if ASDF can't plan it (a dependency not installed: Quicklisp's load fetches it)."
  (format nil "(handler-case
  (let ((s (asdf:find-system ~a)))
    (loop for a in (asdf::plan-actions (asdf:make-plan nil 'asdf:load-op s))
          for o = (asdf::action-operation a)
          for c = (asdf::action-component a)
          when (and (typep o 'asdf:compile-op) (typep c 'asdf:cl-source-file)
                    (eq (asdf:component-system c) s))
            collect (namestring (asdf:component-pathname c))))
  (error () :unknown))"
          (cadre::lisp-string system)))

(defun compile-system-then (system run)
  "Load SYSTEM through swank, showing its compiler notes, then call RUN if
it compiled. With nothing to compile, or no plan, just call RUN."
  (with-swank-asdf (connection)
    (rex connection (swank-call "swank:eval-and-grab-output" (files-to-compile-form system))
         :on-ok (lambda (reply)
                  (let ((files (ignore-errors (read-sexp (second reply)))))
                    (if (or (null files) (not (listp files)) (notevery #'stringp files))
                        (funcall run)
                        (progn
                          (message "Compiling ~a…" system)
                          (rex connection (swank-call "swank:operate-on-system-for-emacs" system "LOAD-OP")
                               :on-ok (lambda (result)
                                        (multiple-value-bind (notes successp duration)
                                            (parse-compilation-result result)
                                          (show-notes notes :replace-files files)
                                          (compilation-message notes successp duration
                                                               (format nil "Compiled ~a" system))
                                          (if successp
                                              (funcall run)
                                              (panel-show (window-panel *window*) "problems"))))
                               :on-abort (lambda (reason)
                                           (message "Compiling ~a failed~@[: ~a~]" system reason)))))))
         :on-abort (lambda (reason) (declare (ignore reason)) (funcall run)))))

;;; The view

(defvar *systems-list* nil)
(defvar *systems-status* nil)
(defvar *system-rows* '() "(name . expander-row) for the systems shown.")
(defvar *language-sections* nil "The box below them holding the Racket and Clojure sections (projects.lisp).")

(defun make-systems-widget ()
  (setf *systems-list* (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")
                                                    :margin-start 8 :margin-end 8 :margin-top 4 :valign :start)
        *systems-status* (make-instance 'gtk:label :wrap t :xalign 0.0 :margin-start 12 :margin-end 12
                                                   :margin-top 8 :css-classes '("dim-label")))
  (gtk:build
    (gtk:box :orientation :vertical
      (gtk:box :margin-start 12 :margin-end 6 :margin-top 4
        (gtk:label :label "PROJECT" :xalign 0.0 :hexpand t :css-classes '("caption-heading" "dim-label"))
        (gtk:button :icon-name "view-refresh-symbolic" :tooltip-text "Refresh" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (refresh-systems)))
        (gtk:button :icon-name "cadre-play-symbolic" :tooltip-text "Load another system…" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'load-system)))
        (gtk:button :icon-name "list-add-symbolic" :tooltip-text "Libraries: dependencies, and adding one from Quicklisp"
                    :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'manage-libraries))))
      (gtk:scrolled-window :vexpand t :hscrollbar-policy :never
        (gtk:box :orientation :vertical
          *systems-status*
          *systems-list*
          (setf *language-sections* (make-instance 'gtk:box :orientation :vertical :margin-bottom 8)))))))

(defun refresh-project-languages ()
  "Redraw the page's Racket and Clojure sections (projects.lisp), as when a
REPL starts or stops: not the ASDF systems, which ask the Lisp."
  (when (and *language-sections* *window*)
    (refresh-language-sections (current-project-root))))

(defun systems-status (format &rest args)
  (gtk:label-set-text *systems-status* (if format (apply #'format nil format args) ""))
  (gtk:widget-set-visible *systems-status* (and format t)))

(defvar *project-page-root* nil "The project the Project page shows (a namestring).")

(defun follow-project-root ()
  "When the current file is in another project than the page shows, show
that one, and its ▶."
  (let ((root (current-project-root)))
    (unless (equal (and root (namestring root)) *project-page-root*)
      (refresh-systems)
      (update-run-button))))

(defun refresh-systems ()
  "List the project's systems again, and ask the Lisp which are loaded."
  (setf *project-page-root* (let ((root (current-project-root))) (and root (namestring root))))
  (when *systems-list*
    (gtk:list-box-remove-all *systems-list*)
    (setf *system-rows* '())
    (let* ((directory (current-project-root))
           (entries (and directory (project-system-entries directory)))
           (others (refresh-language-sections directory)))
      (gtk:widget-set-visible *systems-list* (and entries t))
      (cond ((null directory) (systems-status "Open a folder to see its ASDF systems, Racket package or Clojure project."))
            ((null entries) (systems-status (if others nil "No .asd, info.rkt, deps.edn, project.clj or bb.edn in this folder.")))
            (t (systems-status nil)
               (dolist (entry entries)
                 (let ((row (system-row directory (first entry) (second entry))))
                   (push (cons (first entry) row) *system-rows*)
                   (gtk:list-box-append *systems-list* row)))
               (setf *system-rows* (nreverse *system-rows*))
               (when (connected-p) (update-systems-loaded)))))))

(defun update-systems-loaded ()
  (with-swank-asdf (connection)
    (dolist (entry *system-rows*)
      (destructuring-bind (name . row) entry
        (rex connection (swank-call "swank:asdf-system-loaded-p" name)
             :on-ok (lambda (loaded)
                      (adw:expander-row-set-subtitle row (if loaded "Loaded" "Not loaded")))
             :on-abort (lambda (reason)
                         (declare (ignore reason))
                         (adw:expander-row-set-subtitle row "Not found by ASDF")))))))

(defun system-row (directory name asd-file)
  (let ((row (make-instance 'adw:expander-row :title name
                                              :subtitle (if (connected-p) "" (file-namestring asd-file))))
        (load (make-instance 'gtk:button :icon-name "cadre-play-symbolic" :valign :center
                                         :tooltip-text (format nil "Load ~a into the Lisp" name)
                                         :css-classes '("flat"))))
    (gobject:connect load :clicked (lambda (b) (declare (ignore b)) (load-system-in-repl directory name)))
    (adw:expander-row-add-suffix row load)
    (let ((filled nil))
      (gobject:connect row "notify::expanded"
                       (lambda (r pspec)
                         (declare (ignore pspec))
                         (when (and (adw:expander-row-get-expanded r) (not filled))
                           (setf filled t)
                           (fill-system-row row directory name asd-file)))))
    row))

(defun file-row (file &optional subtitle)
  (let ((row (make-instance 'adw:action-row :title (file-namestring file) :activatable t
                                            :subtitle (or subtitle ""))))
    (gobject:connect row :activated (lambda (r) (declare (ignore r)) (open-file-path (pathname file))))
    row))

(defun fill-system-row (row directory name asd-file)
  (adw:expander-row-add-row
   row
   (gtk:build
     (gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 6
       (gtk:button :label "Load" :tooltip-text "Load the system (with Quicklisp if there is one)"
                   :on-clicked (lambda (b) (declare (ignore b)) (load-system-in-repl directory name)))
       (gtk:button :label "Reload" :tooltip-text "Compile every file again and load it; notes go to Problems"
                   :on-clicked (lambda (b) (declare (ignore b)) (reload-system name)))
       (gtk:button :label "Test" :tooltip-text "Run the project's tests on the Tests page (or, if they aren't parachute or FiveAM tests, asdf:test-system in the REPL)"
                   :on-clicked (lambda (b) (declare (ignore b))
                                 (test-project directory
                                               (lambda ()
                                                 (run-in-repl (format nil "Testing system ~a" name)
                                                              (test-system-form directory name))))))
       (gtk:button :label "Libraries" :tooltip-text "What the system depends on, and adding a library from Quicklisp"
                   :on-clicked (lambda (b) (declare (ignore b)) (show-libraries directory name))))))
  (if (connected-p)
      (with-swank-asdf (connection)
        (rex connection (swank-call "swank:eval-and-grab-output" (system-files-form name))
             :on-ok (lambda (reply)
                      (dolist (file (let ((files (ignore-errors (read-sexp (second reply)))))
                                      (if (listp files) (remove-if-not #'stringp files) '())))
                        (adw:expander-row-add-row row (file-row file (relative-directory file directory)))))
             :on-abort (lambda (reason)
                         (declare (ignore reason))
                         (adw:expander-row-add-row row (file-row asd-file)))))
      (adw:expander-row-add-row row (file-row asd-file "Start a Lisp to list the system's files"))))

(defun system-files-form (name)
  "Source whose value is the system NAME's .asd file and source files, as
namestrings. (swank:asdf-system-files warns about a deprecated ASDF function.)"
  (format nil "(let ((s (asdf:find-system ~a)) (files '()))
  (labels ((walk (c)
             (typecase c
               (asdf:source-file (push (namestring (asdf:component-pathname c)) files))
               (asdf:module (mapc #'walk (asdf:component-children c))))))
    (walk s))
  (cons (namestring (asdf:system-source-file s)) (nreverse files)))"
          (cadre::lisp-string name)))

(defun relative-directory (file directory)
  "FILE's directory relative to DIRECTORY, as text, or \"\"."
  (let ((dir (directory-namestring (pathname file)))
        (base (uiop:native-namestring directory)))
    (if (and (> (length dir) (length base)) (string= base dir :end2 (length base)))
        (string-right-trim "/" (subseq dir (length base)))
        "")))

(defun reload-system (name)
  (with-swank-asdf (connection)
    (message "Reloading ~a…" name)
    (rex connection (swank-call "swank:reload-system" name)
         :on-ok (lambda (result)
                  (multiple-value-bind (notes successp duration) (parse-compilation-result result)
                    (flet ((show (files)
                             ;; Every file was compiled again: its old notes go.
                             (show-notes notes :replace-files files)
                             (compilation-message notes successp duration (format nil "Reloaded ~a" name))
                             (image-changed)
                             (update-systems-loaded)))
                      (rex connection (swank-call "swank:eval-and-grab-output" (system-files-form name))
                           :on-ok (lambda (reply)
                                    (let ((files (ignore-errors (read-sexp (second reply)))))
                                      (show (if (listp files) (remove-if-not #'stringp files) '()))))
                           :on-abort (lambda (reason) (declare (ignore reason)) (show '())))))))))

(define-command load-system ()
  "Load any system ASDF can find, choosing from a list."
  (with-swank-asdf (connection)
    (rex connection (swank-call "swank:list-asdf-systems")
         :on-ok (lambda (systems)
                  (open-picker (window-picker *window*)
                               :items systems :placeholder "Load which system?"
                               :on-choose (lambda (system) (load-system-in-repl nil system)))))))

(define-command show-systems ()
  "Show the project's ASDF systems in the sidebar."
  (show-sidebar-page *window* "systems" :toggle nil))
