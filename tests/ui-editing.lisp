;;;; Editing regressions that exercise GtkTextBuffer without opening a window.
(in-package #:cadre-ui-tests)

(define-test editing)

(defun buffer-with-text (text)
  (c:make-buffer :text (ui::make-gtk-text text)))

(define-test trim-trailing-whitespace-lines :parent editing
  (let ((buffer (buffer-with-text (format nil "alpha   ~%beta~c~c~%   ~%omega" #\Tab #\Space))))
    (is = 8 (ui::trim-trailing-whitespace-in-buffer buffer))
    (is string= (format nil "alpha~%beta~%~%omega") (c:buffer-string buffer))
    ;; A second call is safe and reports that there was nothing left to remove.
    (is = 0 (ui::trim-trailing-whitespace-in-buffer buffer))))

(define-test trim-keeps-meaningful-whitespace :parent editing
  ;; Markdown: two or more spaces after text are a line break.
  (let ((buffer (buffer-with-text (format nil "line break  ~%three   ~%one ~%tab~c~%  ~%end" #\Tab))))
    (setf (c:buffer-major-mode buffer) 'c::markdown-mode)
    (ui::trim-trailing-whitespace-in-buffer buffer)
    (is string= (format nil "line break  ~%three   ~%one~%tab~%~%end") (c:buffer-string buffer)))
  ;; Lisp: a line ending inside a string keeps its spaces; code lines don't.
  (let ((buffer (buffer-with-text (format nil "(defun f ()   ~%  \"doc   ~%  more\"  ~%  1)"))))
    (setf (c:buffer-major-mode buffer) 'c::lisp-mode)
    ;; attach-syntax needs a display; trimming only reads the syntax.
    (setf (c:buffer-local buffer :syntax) (c::make-lisp-syntax (c:buffer-text buffer)))
    (ui::trim-trailing-whitespace-in-buffer buffer)
    (is string= (format nil "(defun f ()~%  \"doc   ~%  more\"~%  1)") (c:buffer-string buffer))))

(define-test trim-trailing-whitespace-save-setting :parent editing
  (let* ((buffer (buffer-with-text (format nil "one  ~%two~c~%" #\Tab)))
         (ui:*trim-trailing-whitespace-on-save* nil))
    (c:run-hook 'c:*before-save-hook* buffer)
    (is string= (format nil "one  ~%two~c~%" #\Tab) (c:buffer-string buffer))
    (setf ui:*trim-trailing-whitespace-on-save* t)
    (c:run-hook 'c:*before-save-hook* buffer)
    (is string= (format nil "one~%two~%") (c:buffer-string buffer))))

(define-test trim-trailing-whitespace-command-is-registered :parent editing
  (let ((command (c:find-command 'ui:trim-trailing-whitespace)))
    (true command)
    (is string= "Trim trailing whitespace" (c::command-title command))))

(define-test format-on-save-setting :parent editing
  (let* ((buffer (buffer-with-text "{\"a\":1,\"b\":2}"))
         (ui:*format-on-save* nil))
    (setf (c:buffer-major-mode buffer) 'c:json-mode)
    (c:run-hook 'c:*before-save-hook* buffer)
    (is string= "{\"a\":1,\"b\":2}" (c:buffer-string buffer))
    (setf ui:*format-on-save* t)
    (c:run-hook 'c:*before-save-hook* buffer)
    (is string= (format nil "{~%  \"a\": 1,~%  \"b\": 2~%}~%")
               (c:buffer-string buffer))))

(define-test setup-check :parent editing
  (let ((home (merge-pathnames (format nil "cadre-setup-test-~a/" (c:make-uuid)) (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "quicklisp/" home))
           (is eq :missing (ui::quicklisp-state :home home))
           (with-open-file (o (merge-pathnames "quicklisp/setup.lisp" home) :direction :output) (write-line ";" o))
           (is eq :not-loaded (ui::quicklisp-state :home home) "installed, but SBCL doesn't load it")
           (is eq :ready (ui::quicklisp-state :home home :sbcl nil) "another Lisp: installed is enough")
           (with-open-file (o (merge-pathnames ".sbclrc" home) :direction :output)
             (write-line "(load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))" o))
           (is eq :ready (ui::quicklisp-state :home home)))
      (uiop:delete-directory-tree home :validate t :if-does-not-exist :ignore)))
  (true (ui::program-path "sh"))
  (false (ui::program-path "cadre-no-such-program-here"))
  (false (ui::program-path "/no/such/cadre/program"))
  (true (nth-value 1 (ui::program-version (ui::program-path "sh") "-c" "echo 1")))
  (false (nth-value 1 (ui::program-version (ui::program-path "sh") "-c" "exit 1"))
         "a program that's there but fails, like macOS's git before the developer tools")
  (true (ui::install-command :sbcl))
  (let ((results '((:sbcl :found t) (:quicklisp :found nil) (:git :found t) (:claude :found nil)
                   (:racket :found nil) (:clojure :found nil) (:prettier :found nil))))
    (is equal '(:quicklisp) (ui::setup-missing results) "optional tools don't count")
    (is equal '() (ui::setup-missing results :importance '(:required)))
    (is equal '(:sbcl) (ui::setup-missing (cons '(:sbcl :found nil) (rest results)) :importance '(:required)))))
