;;;; build-app.lisp — save Cadre's image for the macOS app and the Linux package:
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load scripts/build-app.lisp
;;;; (make app runs this, then scripts/make-app.sh puts it in Cadre.app.)
;;;;
;;;; It writes build/app/cadre.core, a core without the runtime, and
;;;; build/app/lisp-systems.tsv, the Lisp libraries in it with their
;;;; versions and licenses (for THIRD-PARTY-NOTICES). The packages run the
;;;; core with a copy of this SBCL's runtime, which a launcher starts with
;;;; the heap size and --end-runtime-options, so the command-line arguments
;;;; (a folder, files) all reach Cadre. A core is kept apart from the
;;;; runtime because macOS can't code-sign an executable with the image
;;;; appended to it (codesign: "main executable failed strict validation"),
;;;; and notarization needs a valid signature.
(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings and the docset library: checkouts next to Cadre if
;; there are any, otherwise wherever Quicklisp finds them
;; (~/quicklisp/local-projects/).
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre :silent t)

(let ((others (remove-if (lambda (thread) (search "finalizer" (sb-thread:thread-name thread)))
                         (rest (sb-thread:list-all-threads)))))
  (when others
    (error "Can't save: other threads are running (~{~a~^, ~})" (mapcar #'sb-thread:thread-name others))))

(defun primary-system-name (name)
  (asdf:primary-system-name name))

;; One line per library (its primary system): name, version, license, and
;; the folder it was loaded from.
(let ((path (merge-pathnames "build/app/lisp-systems.tsv" (truename "."))))
  (ensure-directories-exist path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (let ((seen (make-hash-table :test 'equal)))
      (dolist (name (sort (asdf:already-loaded-systems) #'string<))
        (let* ((primary (primary-system-name name))
               (system (asdf:find-system primary nil)))
          (when (and system (not (gethash primary seen)) (asdf:system-source-directory system))
            (setf (gethash primary seen) t)
            (format out "~a	~a	~a	~a~%" primary
                    (or (asdf:component-version system) "")
                    (or (asdf:system-license system) "")
                    (namestring (asdf:system-source-directory system)))))))))

(let ((path (merge-pathnames "build/app/cadre.core" (truename "."))))
  (format t "~&Saving ~a~%" path)
  (sb-ext:save-lisp-and-die path
                            :toplevel (lambda ()
                                        (let ((status (gtk4.runtime:with-gtk-float-traps (cadre-ui::app-main))))
                                          (sb-ext:exit :code (if (integerp status) status 0) :abort nil)))))
