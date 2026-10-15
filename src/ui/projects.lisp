;;;; projects.lisp — Racket packages and Clojure projects on the Project page
;;;;
;;;; Beside the ASDF systems (systems.lisp), the Project page shows the open
;;;; folder's Racket package (info.rkt) and Clojure project (deps.edn,
;;;; project.clj or bb.edn), read from their files (core
;;;; language-projects.lisp):
;;;;   Racket   its dependencies, and which aren't installed, with Install;
;;;;            its modules, each with Run and Test; raco make and raco
;;;;            test; installing the package for this machine (raco pkg
;;;;            install --link)
;;;;   Clojure  the REPL (Jack In, Restart), aliases to start it with, its
;;;;            dependencies, with Load New Dependencies when they changed
;;;;            since the REPL started (sync-deps in a deps.edn REPL, else
;;;;            downloading them and restarting); its namespaces, each with
;;;;            Load and Run Tests; Run All Tests; Build Docs
;;;; Saving info.rkt or a Clojure project file refreshes the page, and says
;;;; when there are dependencies to install.

(in-package #:cadre-ui)

(defun section-heading (text)
  (make-instance 'gtk:label :label text :xalign 0.0 :margin-start 12 :margin-top 12 :margin-bottom 2
                            :css-classes '("caption-heading" "dim-label")))

(defun boxed-list ()
  (make-instance 'gtk:list-box :selection-mode :none :css-classes '("boxed-list")
                               :margin-start 8 :margin-end 8 :margin-top 4 :valign :start))

(defun one-line-row (&rest initargs)
  "An action row whose title and subtitle keep to one line each, cut off at the right."
  (apply #'make-instance 'adw:action-row :title-lines 1 :subtitle-lines 1 initargs))

(defun flat-button (label tooltip action)
  (let ((button (make-instance 'gtk:button :label label :tooltip-text tooltip :valign :center)))
    (gobject:connect button :clicked (lambda (b) (declare (ignore b))
                                       (handler-case (funcall action)
                                         (editor-error (e) (message "~a" (editor-error-message e))))))
    button))

(defun button-bar (&rest buttons)
  (let ((box (make-instance 'gtk:box :spacing 6 :margin-start 8 :margin-end 8 :margin-top 6 :margin-bottom 6)))
    (dolist (b buttons) (when b (gtk:box-append box b)))
    (make-instance 'gtk:list-box-row :activatable nil :selectable nil :child box)))

(defun file-in-project-name (file directory)
  (let ((base (uiop:native-namestring directory)) (name (uiop:native-namestring file)))
    (if (uiop:string-prefix-p base name) (subseq name (length base)) name)))

(defun open-then (file function)
  "Open FILE in a tab, then call FUNCTION (in it)."
  (open-file-path (pathname file) :then (lambda (view) (declare (ignore view)) (funcall function))))

;;; Racket

(defun racket-section (directory)
  (let* ((info (read-racket-info directory))
         (list (boxed-list))
         (all-deps (append (getf info :deps) (getf info :build-deps)))
         (deps-row (one-line-row :title "Dependencies"
                                                  :subtitle (if all-deps "Checking…" "None")))
         (install (flat-button "Install" "Install the missing dependencies (raco pkg install)"
                               (lambda () (install-racket-dependencies directory))))
         (modules (racket-package-modules directory))
         (modules-row (make-instance 'adw:expander-row :title-lines 1 :subtitle-lines 1 :title "Modules"
                                                       :subtitle (format nil "~d file~:p" (length modules)))))
    (gtk:widget-set-visible install nil)
    (adw:action-row-add-suffix deps-row install)
    (when all-deps (gtk:widget-set-tooltip-text deps-row (format nil "~{~a~^, ~}" all-deps)))
    (gtk:list-box-append list deps-row)
    (dolist (file modules)
      (let ((file file)
            (row (one-line-row :title (file-in-project-name file directory) :activatable t)))
        (gobject:connect row :activated (lambda (r) (declare (ignore r)) (open-file-path file)))
        (adw:action-row-add-suffix row (flat-button "Run" "Run the module and put the REPL in it"
                                                    (lambda () (open-then file (lambda () (call-command 'racket-run))))))
        (adw:action-row-add-suffix row (flat-button "Test" "raco test this file"
                                                    (lambda () (run-raco (uiop:pathname-directory-pathname file)
                                                                         (list "test" (uiop:native-namestring file))
                                                                         "raco test"))))
        (adw:expander-row-add-row modules-row row)))
    (gtk:list-box-append list modules-row)
    (gtk:list-box-append
     list
     (button-bar
      (flat-button "Test" "raco test the package"
                   (lambda () (run-raco directory (list "test" (uiop:native-namestring directory)) "raco test")))
      (flat-button "Make" "raco make every module"
                   (lambda () (run-raco directory (cons "make" (mapcar #'uiop:native-namestring modules)) "raco make")))
      (flat-button "Install for This Machine" "raco pkg install --link: other code can require it"
                   (lambda () (run-raco directory (list "pkg" "install" "--auto" "--batch" "--link"
                                                        (uiop:native-namestring directory))
                                        "raco pkg install")))
      (flat-button "Build Docs" "Racket: Build Docs" (lambda () (call-command 'racket-build-docs)))))
    (when all-deps (check-racket-dependencies directory all-deps deps-row install))
    (values (section-heading (format nil "RACKET PACKAGE~@[ · ~a~]~@[ ~a~]"
                                     (getf info :collection) (getf info :version)))
            list)))

(defun check-racket-dependencies (directory deps row install)
  "Find which of DEPS aren't installed, in the background, and show it in ROW."
  (declare (ignore directory))
  (sb-thread:make-thread
   (lambda ()
     (let ((missing (ignore-errors (racket-missing-dependencies deps))))
       (deliver-to-gui
        (lambda ()
          (adw:action-row-set-subtitle row (if missing
                                               (format nil "Missing: ~{~a~^, ~}" missing)
                                               (format nil "~d, all installed" (length deps))))
          (gtk:widget-set-visible install (and missing t))))))
   :name "racket deps"))

(defun install-racket-dependencies (directory)
  (let* ((info (or (read-racket-info directory) (editor-error "No info.rkt in ~a" (uiop:native-namestring directory))))
         (missing (racket-missing-dependencies (append (getf info :deps) (getf info :build-deps)))))
    (if (null missing)
        (message "Every dependency is installed")
        (run-raco directory (racket-install-arguments missing) "Installing dependencies"))))

;;; Clojure

(defun clojure-project-kind-here (directory)
  (loop for (file . kind) in cadre::*clojure-project-files*
        when (probe-file (merge-pathnames file directory)) return kind))

(defun same-folder-p (a b)
  "True if A and B are the same folder, however they were reached (macOS's
/var is /private/var)."
  (equal (folder-key a) (folder-key b)))

(defun clojure-repl-runs-project-p (directory)
  (and (clojure-connection) *clojure-repl-project*
       (same-folder-p (first *clojure-repl-project*) directory)))

(defun clojure-deps-changed-p (directory kind)
  "True if the project's dependencies changed since its REPL started (or last loaded them)."
  (and (clojure-repl-runs-project-p directory)
       (not (equal (third *clojure-repl-project*)
                   (ignore-errors (clojure-deps-signature (read-clojure-project-file directory kind)))))))

(defun clojure-section (directory kind)
  (let* ((project (read-clojure-project-file directory kind))
         (list (boxed-list))
         (running (clojure-repl-runs-project-p directory))
         (repl-row (one-line-row :title "REPL"
                                                  :subtitle (cond (running (connection-implementation (clojure-connection)))
                                                                  (*clojure-starting* "Starting…")
                                                                  ((clojure-connection) "Running another project")
                                                                  (t "Not running")))))
    (adw:action-row-add-suffix repl-row
                               (if running
                                   (flat-button "Restart" "Stop the REPL and start it again (with the aliases ticked)"
                                                (lambda () (stop-clojure-connection)
                                                  (start-clojure-connection nil :directory directory :kind kind)))
                                   (flat-button "Jack In" "Start the project's REPL (with the aliases ticked)"
                                                (lambda () (start-clojure-connection
                                                            (lambda (c) (declare (ignore c)) (show-clojure-repl :focus nil))
                                                            :directory directory :kind kind)))))
    (gtk:list-box-append list repl-row)
    (when (getf project :aliases)
      (let ((row (make-instance 'adw:expander-row :title-lines 1 :subtitle-lines 1 :title (if (eq kind :lein) "Profiles" "Aliases")
                                                  :subtitle "Used the next time the REPL starts")))
        (dolist (alias (getf project :aliases))
          (let* ((alias alias)
                 (check (make-instance 'gtk:check-button :valign :center
                                                         :active (member alias (gethash (folder-key directory) *clojure-project-aliases*)
                                                                         :test #'string=)))
                 (alias-row (one-line-row :title (format nil ":~a" alias) :activatable-widget check)))
            (adw:action-row-add-prefix alias-row check)
            (gobject:connect check :toggled
                             (lambda (c)
                               (let ((key (folder-key directory)))
                                 (setf (gethash key *clojure-project-aliases*)
                                       (if (gtk:check-button-get-active c)
                                           (adjoin alias (gethash key *clojure-project-aliases*) :test #'string=)
                                           (remove alias (gethash key *clojure-project-aliases*) :test #'string=))))))
            (adw:expander-row-add-row row alias-row)))
        (gtk:list-box-append list row)))
    (let* ((changed (clojure-deps-changed-p directory kind))
           (deps (getf project :deps))
           (row (make-instance 'adw:expander-row :title-lines 1 :subtitle-lines 1 :title "Dependencies"
                                                 :subtitle (if changed
                                                               "Changed since the REPL started"
                                                               (format nil "~d librar~:@p" (length deps))))))
      ;; The button is the list's first row: beside the title it squeezes it.
      (adw:expander-row-add-row row (button-bar (flat-button (if changed "Load New Dependencies" "Download")
                                                             (if changed
                                                                 "Add the new libraries to the running REPL (or download them and restart)"
                                                                 "Download the project's dependencies")
                                                             (lambda () (load-clojure-dependencies directory kind)))))
      (when changed (adw:expander-row-set-expanded row t))
      (dolist (dep deps)
        (adw:expander-row-add-row row (one-line-row :title (car dep) :subtitle (cdr dep))))
      (gtk:list-box-append list row))
    (let* ((namespaces (clojure-project-namespaces directory))
           (row (make-instance 'adw:expander-row :title-lines 1 :subtitle-lines 1 :title "Namespaces"
                                                 :subtitle (format nil "~d" (length namespaces)))))
      (loop for (ns . file) in namespaces
            do (let ((file file)
                     (ns-row (one-line-row :title ns :subtitle (file-in-project-name file directory)
                                                            :activatable t)))
                 (gobject:connect ns-row :activated (lambda (r) (declare (ignore r)) (open-file-path file)))
                 (adw:action-row-add-suffix ns-row (flat-button "Load" "Load the file into the REPL"
                                                                (lambda () (open-then file (lambda () (call-command 'load-file))))))
                 (adw:action-row-add-suffix ns-row (flat-button "Test" "Run the namespace's tests"
                                                                (lambda () (open-then file (lambda () (call-command 'clojure-run-tests))))))
                 (adw:expander-row-add-row row ns-row)))
      (gtk:list-box-append list row))
    (gtk:list-box-append list
                         (button-bar
                          (flat-button "Run All Tests" "Load every namespace and run the project's tests; failures go to Problems"
                                       (lambda () (run-clojure-project-tests directory)))
                          (flat-button "Build Docs" "Clojure: Build Docs" (lambda () (call-command 'clojure-build-docs)))))
    (values (section-heading (format nil "CLOJURE PROJECT · ~a~@[ ~a~] · ~a"
                                     (getf project :name) (getf project :version)
                                     (ecase kind (:deps "deps.edn") (:lein "project.clj") (:babashka "bb.edn"))))
            list)))

(defun load-clojure-dependencies (directory kind)
  "Get the project's dependencies: in a deps.edn project's running REPL,
sync-deps adds the new ones without a restart; otherwise download them (and
offer to restart a running REPL, so it sees them)."
  (let ((aliases (gethash (folder-key directory) *clojure-project-aliases*)))
    (if (and (eq kind :deps) (clojure-repl-runs-project-p directory))
        (progn
          (message "Loading the new dependencies into the REPL…")
          (request #'backend-eval (clojure-connection) *clojure-sync-deps-code* :package "user"
                   :on-ok (lambda (value)
                            (if (search ":synced" value)
                                (progn
                                  (setf (third *clojure-repl-project*)
                                        (ignore-errors (clojure-deps-signature (read-clojure-project-file directory kind))))
                                  (refresh-systems)
                                  (message "The new dependencies are loaded in the REPL"))
                                (fetch-clojure-dependencies directory kind aliases t)))))
        (fetch-clojure-dependencies directory kind aliases (clojure-repl-runs-project-p directory)))))

(defun fetch-clojure-dependencies (directory kind aliases restart)
  (when *build* (editor-error "~a is already running (Stop Build ends it)" (getf *build* :what)))
  (let ((command (clojure-fetch-command kind aliases)))
    (run-project-process directory (list :program (first command) :arguments (rest command)
                                         :description (format nil "~{~a~^ ~}" command))
                         :what "Downloading dependencies"
                         :on-success (lambda (seconds)
                                       (if restart
                                           (message "Downloaded in ~,1f s: restart the REPL (Project page) to use them" seconds)
                                           (message "Dependencies downloaded in ~,1f s" seconds))))))

(defun run-clojure-project-tests (directory)
  (with-connection (connection :language :clojure)
    (unless (nrepl-op-p connection "test-var-query") (editor-error "Running tests needs cider-nrepl"))
    (message "Running the project's tests…")
    (let ((files (clojure-project-namespaces directory)))
      (nrepl-run-tests
       connection :project
       :on-abort (lambda (reason) (message "The tests didn't run: ~a" reason))
       :on-done (lambda (summary failures)
                  (show-notes
                   (loop for f in failures
                         for file = (cdr (assoc (getf f :ns) files :test #'equal))
                         collect (make-compiler-note
                                  (if (equal (getf f :type) "error") :error :warning)
                                  (format nil "~a/~a ~a~@[: ~a~]~@[~%expected: ~a~]~@[~%actual: ~a~]"
                                          (getf f :ns) (getf f :var) (getf f :type)
                                          (let ((m (getf f :message))) (and (plusp (length m)) m))
                                          (and (getf f :expected) (string-trim '(#\Newline) (getf f :expected)))
                                          (and (getf f :actual) (string-trim '(#\Newline) (getf f :actual))))
                                  (if file
                                      (list :file (uiop:native-namestring file)
                                            :line (max 0 (1- (or (getf f :line) 1))) :column 0)
                                      (list :error (format nil "In ~a" (getf f :ns))))))
                   :replace-files (mapcar (lambda (e) (uiop:native-namestring (cdr e))) files))
                  (message "Project tests: ~d test~:p, ~d passed, ~d failed, ~d error~:p"
                           (getf summary :test) (getf summary :pass) (getf summary :fail) (getf summary :error)))))))

;;; The page's sections, and noticing changed dependencies

(defun refresh-language-sections (directory)
  "Show DIRECTORY's Racket package and Clojure project, if it has them.
Returns true if it has either."
  (when *language-sections*
    (loop for child = (gtk:widget-get-first-child *language-sections*)
          while child do (gtk:box-remove *language-sections* child))
    (let ((any nil))
      (when directory
        (when (probe-file (merge-pathnames "info.rkt" directory))
          (multiple-value-bind (heading list) (racket-section directory)
            (gtk:box-append *language-sections* heading)
            (gtk:box-append *language-sections* list)
            (setf any t)))
        (let ((kind (clojure-project-kind-here directory)))
          (when kind
            (multiple-value-bind (heading list) (clojure-section directory kind)
              (gtk:box-append *language-sections* heading)
              (gtk:box-append *language-sections* list)
              (setf any t)))))
      any)))

(defparameter *project-file-names* '("info.rkt" "deps.edn" "project.clj" "bb.edn"))

(defun message-after-save (format &rest args)
  "Say something about a file just saved, after the \"Saved …\" message (which would hide it)."
  (let ((text (apply #'format nil format args)))
    (glib:timeout-add glib:+priority-default+ 150 (lambda () (message "~a" text) nil))))

(add-hook '*after-save-hook*
          (lambda (buffer)
            (let ((file (buffer-file buffer)))
              (when (and file (member (file-namestring file) *project-file-names* :test #'string=))
                (let ((directory (uiop:pathname-directory-pathname file)))
                  (refresh-systems)
                  (cond ((string= (file-namestring file) "info.rkt")
                         (let ((info (read-racket-info directory)))
                           (sb-thread:make-thread
                            (lambda ()
                              (let ((missing (ignore-errors (racket-missing-dependencies
                                                             (append (getf info :deps) (getf info :build-deps))))))
                                (when missing
                                  (deliver-to-gui
                                   (lambda ()
                                     (message-after-save "info.rkt needs ~{~a~^, ~}: Install on the Project page (or Install Project Dependencies)"
                                                         missing))))))
                            :name "racket deps")))
                        ((clojure-deps-changed-p directory (clojure-project-kind-here directory))
                         (message-after-save "The dependencies changed: Load New Dependencies on the Project page (or Install Project Dependencies)"))))))))

(define-command install-project-dependencies ()
  "Install what the open folder's Racket package or Clojure project depends on:
Racket's missing packages, or Clojure's new libraries (into the running REPL
when it can)."
  (let* ((directory (or (window-project *window*) (editor-error "Open a project first")))
         (kind (clojure-project-kind-here directory)))
    (cond ((probe-file (merge-pathnames "info.rkt" directory)) (install-racket-dependencies directory))
          (kind (load-clojure-dependencies directory kind))
          (t (editor-error "The open folder has no info.rkt, deps.edn, project.clj or bb.edn")))))
