(in-package #:cadre-tests)

(defmacro with-clean-buffers (&body body)
  `(let ((c::*buffers* '())
         (c::*current-buffer* nil))
     ,@body))

(define-test buffers :parent cadre-tests
  (with-clean-buffers
    (let ((a (c:make-buffer :file "/tmp/src/foo.lisp"))
          (b (c:make-buffer :file "/tmp/test/foo.lisp"))
          (s (c:make-buffer :name "*scratch*")))
      (is string= "foo.lisp" (c:buffer-name a))
      (is string= "foo.lisp<2>" (c:buffer-name b))
      (is eq 'c:lisp-mode (c:buffer-major-mode a))
      (is eq 'c:fundamental-mode (c:buffer-major-mode s))
      (is eq s (c:find-buffer "*scratch*"))
      (is eq b (c:find-file-buffer "/tmp/test/foo.lisp"))
      (c:kill-buffer a)
      (is equal (list s b) (c:buffer-list))
      (is string= "foo.lisp" (c:buffer-name (c:make-buffer :file "/tmp/x/foo.lisp"))))))

(define-test buffer-hooks :parent cadre-tests
  (with-clean-buffers
    (let ((seen '())
          (c:*buffer-created-hook* '())
          (c:*buffer-killed-hook* '()))
      (c:add-hook 'c:*buffer-created-hook* (lambda (b) (push (list :new (c:buffer-name b)) seen)))
      (c:add-hook 'c:*buffer-killed-hook* (lambda (b) (push (list :dead (c:buffer-name b)) seen)))
      (c:kill-buffer (c:make-buffer :name "x"))
      (is equal '((:dead "x") (:new "x")) seen))))

(define-test buffer-text :parent cadre-tests
  (with-clean-buffers
    (let ((b (c:make-buffer :name "t" :text (c:make-string-text "abc"))))
      (c:buffer-insert b "123" 3)
      (is string= "abc123" (c:buffer-string b))
      (true (c:buffer-modified-p b))
      (setf (c:buffer-modified-p b) nil)
      (false (c:buffer-modified-p b)))))
