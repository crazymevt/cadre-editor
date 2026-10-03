;;;; test.lisp — run the headless test suite: sbcl --script-ish via make test
(push (truename ".") asdf:*central-registry*)
(handler-bind ((warning (lambda (w) (format *error-output* "~&WARNING: ~a~%" w) (muffle-warning w))))
  (ql:quickload :cadre/tests :silent t))
(uiop:quit (if (parachute:status (parachute:test :cadre-tests :report 'parachute:plain)) 0 1))
