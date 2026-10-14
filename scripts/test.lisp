;;;; test.lisp — run the headless test suite: sbcl --script-ish via make test
(push (truename ".") asdf:*central-registry*)
;; The docset library: a checkout next to Cadre if there is one, otherwise
;; wherever Quicklisp finds it (~/quicklisp/local-projects/).
(let ((found (probe-file "../docset/docset.asd")))
  (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*)))
(handler-bind ((warning (lambda (w) (format *error-output* "~&WARNING: ~a~%" w) (muffle-warning w))))
  (ql:quickload :cadre/tests :silent t))
(uiop:quit (if (parachute:status (parachute:test :cadre-tests :report 'parachute:plain)) 0 1))
