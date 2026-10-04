;;;; run.lisp — start Cadre from a source checkout:
;;;;   sbcl --load scripts/run.lisp [folder]
;;;; GTK must run on the first thread on macOS, so Cadre is started from a
;;;; script rather than from an editor's REPL.
(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings: a checkout next to Cadre if there is one, otherwise
;; wherever Quicklisp finds them (~/quicklisp/local-projects/).
(let ((gtk4 (probe-file "../gtk4/gtk4.asd")))
  (when gtk4 (push (uiop:pathname-directory-pathname gtk4) asdf:*central-registry*)))
(ql:quickload :cadre :silent t)
(let ((args (rest (member "--end-toplevel-options" sb-ext:*posix-argv* :test #'string=))))
  (cadre-ui:main :project (first args)
                 :quit-after (let ((q (uiop:getenv "CADRE_QUIT_AFTER"))) (and q (parse-integer q)))))
(uiop:quit 0)
