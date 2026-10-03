(in-package #:cadre-tests)

(defvar *ran* nil)

(c:define-command test-command-anywhere ()
  "A test command."
  (push :anywhere *ran*))

(c:define-command test-command-lisp ()
  "A test command only for Lisp buffers."
  (:modes c:lisp-mode)
  (:title "Lisp only")
  (push :lisp *ran*))

(define-test commands :parent cadre-tests
  (with-clean-buffers
    (let ((*ran* '())
          (c:*after-command-hook* '())
          (after '()))
      (c:add-hook 'c:*after-command-hook* (lambda (name) (push name after)))
      (is string= "Test command anywhere" (c:command-title (c:find-command 'test-command-anywhere)))
      (is string= "Lisp only" (c:command-title (c:find-command 'test-command-lisp)))
      (is string= "A test command." (c:command-documentation (c:find-command 'test-command-anywhere)))
      (setf c::*current-buffer* (c:make-buffer :name "notes"))
      (c:run-command 'test-command-anywhere)
      (fail (c:run-command 'test-command-lisp) 'c:editor-error)
      (false (find 'test-command-lisp (c:list-commands) :key #'c:command-name))
      (setf c::*current-buffer* (c:make-buffer :file "/tmp/a.lisp"))
      (c:run-command 'test-command-lisp)
      (true (find 'test-command-lisp (c:list-commands) :key #'c:command-name))
      (is equal '(:lisp :anywhere) *ran*)
      (is equal '(test-command-lisp test-command-anywhere) after)
      (fail (c:run-command 'no-such-command) 'c:editor-error))))
