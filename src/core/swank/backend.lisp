;;;; backend.lisp — Swank's answers to the editor's requests (../backend.lisp)
;;;;
;;;; Each method sends the Swank call the editor has always made for that
;;;; request, so a Common Lisp connection behaves as it did before there was
;;;; a protocol.

(in-package #:cadre)

(defun swank-request (connection form package on-ok on-abort &key (thread t))
  (swank-rex connection form
             :package (or package (connection-package connection)) :thread thread
             :on-ok (or on-ok #'identity) :on-abort (or on-abort #'default-abort)))

(defmethod backend-supports-p ((connection swank-connection) feature)
  (declare (ignore feature))
  t)

(defmethod backend-eval ((connection swank-connection) string &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:interactive-eval" string 3 120) package on-ok on-abort))

(defmethod backend-eval-region ((connection swank-connection) string &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:interactive-eval-region" string 3 120) package on-ok on-abort))

(defmethod backend-compile-form ((connection swank-connection) string
                                 &key buffer-name position line column file policy package on-ok on-abort)
  (swank-request connection
                 (swank-call "swank:compile-string-for-emacs" string buffer-name
                             (list (list :position position) (list :line line column))
                             file policy)
                 package on-ok on-abort))

(defmethod backend-compile-file ((connection swank-connection) path &key package on-ok on-abort on-loaded)
  (swank-request connection (swank-call "swank:compile-file-for-emacs" path t) package
                 (lambda (result)
                   (when on-ok (funcall on-ok result))
                   ;; Compiling only compiles; load the fasl if it worked.
                   (let ((successp (third result)) (loadp (fifth result)) (fasl (sixth result)))
                     (when (and successp loadp fasl)
                       (swank-request connection (swank-call "swank:load-file" fasl) package
                                      (lambda (v) (declare (ignore v)) (when on-loaded (funcall on-loaded)))
                                      nil))))
                 on-abort))

(defmethod backend-load-file ((connection swank-connection) path &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:load-file" path) package on-ok on-abort))

(defmethod backend-describe ((connection swank-connection) name &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:describe-symbol" name) package on-ok on-abort))

(defmethod backend-arglist ((connection swank-connection) name &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:operator-arglist" name (or package (connection-package connection)))
                 package on-ok on-abort))

(defmethod backend-autodoc ((connection swank-connection) form &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:autodoc" form :print-right-margin 200) package on-ok on-abort))

(defmethod backend-completions ((connection swank-connection) prefix &key package on-ok on-abort)
  (swank-request connection
                 (swank-call "swank:fuzzy-completions" prefix (or package (connection-package connection))
                             :limit 100 :time-limit-in-msec 500)
                 package on-ok on-abort))

(defmethod backend-find-definitions ((connection swank-connection) name &key package on-ok on-abort)
  (swank-request connection (swank-call "swank:find-definitions-for-emacs" name) package on-ok on-abort))

(defmethod backend-xref ((connection swank-connection) kinds name &key package on-ok on-abort)
  (swank-request connection
                 (if (rest kinds)
                     (swank-call "swank:xrefs" kinds name)
                     (swank-call "swank:xref" (first kinds) name))
                 package on-ok on-abort))

(defmethod backend-macroexpand ((connection swank-connection) string &key all compiler package on-ok on-abort)
  (swank-request connection
                 (swank-call (cond (compiler "swank:swank-compiler-macroexpand-1")
                                   (all "swank:swank-macroexpand-all")
                                   (t "swank:swank-macroexpand-1"))
                             string)
                 package on-ok on-abort))

(defmethod backend-repl-eval ((connection swank-connection) string &key package on-ok on-abort)
  (swank-request connection (swank-call "swank-repl:listener-eval" string) package on-ok on-abort
                 :thread :repl-thread))

(defmethod backend-set-package ((connection swank-connection) name &key on-ok on-abort)
  (swank-request connection (swank-call "swank:set-package" name) nil on-ok on-abort :thread :repl-thread))

(defmethod backend-send-input ((connection framed-connection) thread tag string)
  ;; Swank's answer to :read-string, which Cadre's Racket back end takes too.
  (swank-send connection (list :emacs-return-string thread tag string)))

(defmethod backend-interrupt ((connection swank-connection))
  (swank-interrupt connection))

(defmethod backend-disconnect ((connection swank-connection))
  (swank-disconnect connection))
