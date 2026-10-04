;;;; systems.lisp — ASDF systems: the Systems view in the sidebar, and
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

(defun run-in-repl (note form)
  "Show NOTE in the REPL and evaluate FORM (source) there, as if typed."
  (show-repl-page)
  (with-connection (connection)
    (declare (ignore connection))
    (cond ((repl-busy *repl*) (message "The REPL is busy"))
          (t (repl-fresh-line)
             (repl-insert (format nil "; ~a~%" note) "cadre-repl-note")
             (gtk:text-buffer-move-mark (repl-gtk-buffer) (repl-output-mark *repl*)
                                        (gtk:text-buffer-get-end-iter (repl-gtk-buffer)))
             (repl-eval form)))))

(defun load-system-in-repl (directory system)
  (setf (setting :last-system) system)
  (run-in-repl (format nil "Loading system ~a~@[ from ~a~]" system (and directory (uiop:native-namestring directory)))
               (load-system-form directory system)))

(define-command load-project ()
  "Load the open folder's ASDF system into the Lisp (choosing one if there are several)."
  (let* ((directory (or (window-project *window*) (editor-error "Open a folder first.")))
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
                                         (asdf-form "nil" (window-project *window*)))
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

;;; The view

(defvar *systems-list* nil)
(defvar *systems-status* nil)
(defvar *system-rows* '() "(name . expander-row) for the systems shown.")

(defun make-systems-widget ()
  (setf *systems-list* (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")
                                                    :margin-start 8 :margin-end 8 :margin-top 4 :valign :start)
        *systems-status* (make-instance 'gtk:label :wrap t :xalign 0.0 :margin-start 12 :margin-end 12
                                                   :margin-top 8 :css-classes '("dim-label")))
  (gtk:build
    (gtk:box :orientation :vertical
      (gtk:box :margin-start 12 :margin-end 6 :margin-top 4
        (gtk:label :label "SYSTEMS" :xalign 0.0 :hexpand t :css-classes '("caption-heading" "dim-label"))
        (gtk:button :icon-name "view-refresh-symbolic" :tooltip-text "Refresh" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (refresh-systems)))
        (gtk:button :icon-name "cadre-play-symbolic" :tooltip-text "Load another system…" :css-classes '("flat")
                    :on-clicked (lambda (b) (declare (ignore b)) (call-command 'load-system))))
      (gtk:scrolled-window :vexpand t :hscrollbar-policy :never
        (gtk:box :orientation :vertical
          *systems-status*
          *systems-list*)))))

(defun systems-status (format &rest args)
  (gtk:label-set-text *systems-status* (if format (apply #'format nil format args) ""))
  (gtk:widget-set-visible *systems-status* (and format t)))

(defun refresh-systems ()
  "List the project's systems again, and ask the Lisp which are loaded."
  (when *systems-list*
    (gtk:list-box-remove-all *systems-list*)
    (setf *system-rows* '())
    (let* ((directory (window-project *window*))
           (entries (and directory (project-system-entries directory))))
      (cond ((null directory) (systems-status "Open a folder to see its ASDF systems."))
            ((null entries) (systems-status "No .asd file in this folder."))
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
       (gtk:button :label "Test" :tooltip-text "Run the system's tests (asdf:test-system) in the REPL"
                   :on-clicked (lambda (b) (declare (ignore b))
                                 (run-in-repl (format nil "Testing system ~a" name)
                                              (test-system-form directory name)))))))
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
