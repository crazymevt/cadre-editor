;;;; UI regressions: make test-ui (no window or display required).
;; Buffer hooks can write recent files and preferences even without a window.
;; Isolate both stores before loading the UI or constructing any test buffers.
(require :sb-posix)
(let ((directory (merge-pathnames (format nil "cadre-ui-tests-~d-~d/" (get-universal-time) (sb-posix:getpid))
                                 (uiop:temporary-directory))))
  (ensure-directories-exist directory)
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring (merge-pathnames "config/" directory)) 1)
  (sb-posix:setenv "XDG_STATE_HOME" (namestring (merge-pathnames "state/" directory)) 1)
  (push (lambda () (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))
        sb-ext:*exit-hooks*))
(push (truename ".") asdf:*central-registry*)
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre/ui-tests :silent t)
(uiop:quit (if (eq :passed (parachute:status
                           (parachute:test :cadre-ui-tests :report 'parachute:plain)))
               0 1))
