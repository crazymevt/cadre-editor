;;;; run.lisp — start Cadre from a source checkout:
;;;;   sbcl --load scripts/run.lisp [folder]
;;;; GTK must run on the first thread on macOS, so Cadre is started from a
;;;; script rather than from an editor's REPL.
(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings and the docset library: checkouts next to Cadre if
;; there are any, otherwise wherever Quicklisp finds them
;; (~/quicklisp/local-projects/).
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre :silent t)
(let ((args (rest (member "--end-toplevel-options" sb-ext:*posix-argv* :test #'string=))))
  (cadre-ui:main :project (first args)
                 :quit-after (let ((q (uiop:getenv "CADRE_QUIT_AFTER"))) (and q (parse-integer q)))))
(uiop:quit 0)
