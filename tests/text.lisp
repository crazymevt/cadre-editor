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

(define-test string-text-lines :parent cadre-tests
  ;; Its lines come from a cache of where they start: check them against a
  ;; fresh scan after each kind of change.
  (let ((text (c:make-string-text (format nil "ab~%~%cde~%"))))
    (flet ((same-lines ()
             (let* ((string (c:text-string text))
                    (lines (uiop:split-string string :separator '(#\Newline))))
               (is = (length lines) (c:text-line-count text))
               (loop for line in lines
                     for i from 0
                     do (is string= line (c:text-line-string text i)))
               (loop for position from 0 to (length string)
                     do (let ((line (count #\Newline string :end position)))
                          (is equal (list line (- position (c:text-line-position text line)))
                              (multiple-value-list (c:text-position-line text position))))))))
      (same-lines)
      (c:text-insert text 1 (format nil "x~%y"))
      (same-lines)
      (c:text-delete text 0 4)
      (same-lines)
      (c:text-replace-contents text "one line")
      (same-lines)
      (fail (c:text-line-string text 1)))))

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
