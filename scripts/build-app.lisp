;;;; build-app.lisp — save Cadre as an executable for the macOS app:
;;;;   sbcl --dynamic-space-size 4096 --non-interactive --load scripts/build-app.lisp
;;;; (make app runs this, then scripts/make-app.sh puts it in Cadre.app.)
;;;;
;;;; The image keeps this Lisp's runtime options, so the app gets the same
;;;; 4 GB heap as make run, and its command-line arguments (a folder, files)
;;;; all reach Cadre instead of being read as SBCL's options.
(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings: a checkout next to Cadre if there is one, otherwise
;; wherever Quicklisp finds them (~/quicklisp/local-projects/).
(let ((gtk4 (probe-file "../gtk4/gtk4.asd")))
  (when gtk4 (push (uiop:pathname-directory-pathname gtk4) asdf:*central-registry*)))
(ql:quickload :cadre :silent t)

(let ((others (remove-if (lambda (thread) (search "finalizer" (sb-thread:thread-name thread)))
                         (rest (sb-thread:list-all-threads)))))
  (when others
    (error "Can't save: other threads are running (~{~a~^, ~})" (mapcar #'sb-thread:thread-name others))))

(let ((path (merge-pathnames "build/app/Cadre" (truename "."))))
  (ensure-directories-exist path)
  (format t "~&Saving ~a~%" path)
  (sb-ext:save-lisp-and-die path
                            :executable t
                            :save-runtime-options t
                            :toplevel (lambda ()
                                        (let ((status (gtk4.runtime:with-gtk-float-traps (cadre-ui::app-main))))
                                          (sb-ext:exit :code (if (integerp status) status 0) :abort nil)))))
