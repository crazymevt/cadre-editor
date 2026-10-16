;;;; Save and close regressions: make test-ui (no window or display required).
(push (truename ".") asdf:*central-registry*)
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre/ui-tests :silent t)
(uiop:quit (if (eq :passed (parachute:status
                           (parachute:test :cadre-ui-tests :report 'parachute:plain)))
               0 1))
