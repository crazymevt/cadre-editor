(in-package #:cadre-tests)

(define-test string-text :parent cadre-tests
  (let ((text (c:make-string-text "hello world")))
    (is = 11 (c:text-length text))
    (is string= "world" (c:text-string text 6))
    (is char= #\o (c:text-char text 4))
    (false (c:text-modified-p text))
    (c:text-insert text 5 ",")
    (is string= "hello, world" (c:text-string text))
    (true (c:text-modified-p text))
    (c:text-delete text 0 7)
    (is string= "world" (c:text-string text))
    (fail (c:text-delete text 3 10))
    (c:text-replace-contents text "new")
    (is string= "new" (c:text-string text))
    (false (c:text-modified-p text))
    (is = 0 (c:text-point text))))

(define-test string-text-point :parent cadre-tests
  (let ((text (c:make-string-text "abcdef")))
    (setf (c:text-point text) 3)
    (c:text-insert text 1 "XY")           ; before the point: point moves
    (is = 5 (c:text-point text))
    (c:text-insert text 5 "!")            ; at the point: point moves past
    (is = 6 (c:text-point text))
    (c:text-delete text 0 2)              ; before the point
    (is = 4 (c:text-point text))
    (c:text-delete text 3 6)              ; around the point: point goes to start
    (is = 3 (c:text-point text))))
