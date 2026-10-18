;;;; smoke.lisp — drive a real Cadre window through the M0 features:
;;;;   make smoke
;;;; Uses a throwaway project and config directory, checks each step, saves
;;;; screenshots to build/smoke/, prints a report, and exits 0 if all passed.

(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings and the docset library: checkouts next to Cadre if
;; there are any, otherwise wherever Quicklisp finds them
;; (~/quicklisp/local-projects/).
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre :silent t)

(defpackage #:cadre-smoke (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-smoke)

(defvar *out* (merge-pathnames "build/smoke/" (truename ".")))
(defvar *root* (merge-pathnames (format nil "cadre-smoke-~d-~d/" (get-universal-time) (sb-posix:getpid))
                                (uiop:temporary-directory)))
(load (merge-pathnames "scripts/drive.lisp" (truename ".")))

(defun finish ()
  (format t "~&--- Output panel ---~%~a~%---~%" (cadre-ui::panel-output-string (cadre-ui::window-panel *window*)))
  (let ((failures (reverse (remove-if #'second *results*))))
    (when failures
      (format t "~&~%Failed checks:~%")
      (dolist (result failures)
        (destructuring-bind (name ok detail) result
          (declare (ignore ok))
          (format t "  FAIL ~a~@[ — ~a~]~%" name detail))))
    (format t "~&~%~d checks, ~d failed. Screenshots in ~a~%" (length *results*) (length failures) *out*)
    (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if (null failures) 0 1))))

;;; A throwaway project
(ensure-directories-exist (merge-pathnames "src/" *root*))
(with-open-file (o (merge-pathnames "src/hello.lisp" *root*) :direction :output)
  (format o "(defun hello (name)~%  (format t \"Hello, ~~a!~~%\" name))~%"))
(with-open-file (o (merge-pathnames "notes.txt" *root*) :direction :output)
  (format o "Some notes.~%"))
(with-open-file (o (merge-pathnames "src/m1.lisp" *root*) :direction :output)
  (format o "(defun area (w h)~%  (* w h))~%~%(defvar *x* 1)~%; done~%"))
(uiop:copy-file (asdf:system-relative-pathname "gtk4" "src/generated/gtk-functions-1.lisp")
                (merge-pathnames "src/big.lisp" *root*))
(with-open-file (o (merge-pathnames "src/m2.lisp" *root*) :direction :output)
  (format o "(defun twice (x) (* 2 x))~%(defun bad (y) (+ y undefined-thing))~%(twice 21)~%~%"))
(with-open-file (o (merge-pathnames "src/m3.lisp" *root*) :direction :output)
  (format o "(defmacro my-mac (x) `(list ,x ,x))~%(defvar special-thing 5)~%(defun caller () (my-mac (twice 3)))~%(defun get-thing () special-thing)~%(defun uses-undefined () (no-such-function 1))~%(defun deep (n) (if (zerop n) (error \"deep ~~a\" n) (deep (1- n))))~%(defun binder () (let ((not-a-call 1)) not-a-call))~%"))
(with-open-file (o (merge-pathnames "src/m7.lisp" *root*) :direction :output)
  (format o "(defun double-it (x) (* x 2))~%(defun step-me (x)~%  (let ((y (double-it x)))~%    (+ (double-it y) 1)))~%"))
(with-open-file (o (merge-pathnames "src/lib.lisp" *root*) :direction :output)
  (format o "(defun smoke-lib-fn () :loaded)~%"))
(with-open-file (o (merge-pathnames "smoke.asd" *root*) :direction :output)
  (format o "(defsystem \"smoke\" :components ((:file \"src/lib\")))~%"))
(setf *lisp-command* '("sbcl" "--noinform" "--no-userinit"))
(setf *claude-program* (namestring (truename "scripts/fake-claude")))
(setf *codex-program* (namestring (truename "scripts/fake-codex")))
(with-open-file (o (merge-pathnames "src/m5.lisp" *root*) :direction :output)
  (format o "(a b) c~%"))
(with-open-file (o (merge-pathnames "src/m6.lisp" *root*) :direction :output)
  (format o "(defun uses-area () (area 2 3)) ; area in a comment~%(print \"area in a string\")~%"))
(with-open-file (o (merge-pathnames "src/hints.lisp" *root*) :direction :output)
  (format o "(defun scale-shape (shape factor &key (round t))~%  \"Scale SHAPE by FACTOR.\"~%  (list shape factor round))~%"))
(with-open-file (o (merge-pathnames "guide.md" *root*) :direction :output)
  (format o "# Guide~%~%Some **bold** text and a [link](https://example.com).~%~%- one~%- two~%~%```lisp~%(defun hi () 1)~%```~%~%## Usage~%~%| a | b |~%|---|---|~%| 1 | 2 |~%"))
(with-open-file (o (merge-pathnames "cache.fasl" *root*) :direction :output)
  (format o "hidden"))

;;; The steps
(section "basics")
(then 1500
  (check "window is the frontend" (eq *frontend* *window*))
  (check "project is open" (equal (truename (cadre-ui::window-project *window*)) (truename *root*)))
  (check "no tabs at start" (null (tab-titles)))
  (check "starts in the horizontal layout"
         (eq :vertical (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (screenshot "01-start")
  (open-file-path (merge-pathnames "src/hello.lisp" *root*))
  (open-file-path (merge-pathnames "notes.txt" *root*)))

(then 1000
  (check "two tabs open" (equal '("hello.lisp" "notes.txt") (sort (copy-list (tab-titles)) #'string<))
         (tab-titles))
  ;; Check the actual rendered tabs, not just AdwTabView's page model.
  (let* ((group (cadre-ui::window-active-group *window*))
         (tabs (cadre-ui::strip-tabs (cadre-ui::group-strip group))))
    (check "ordinary files build a complete tab strip"
           (equal (mapcar #'car tabs) (cadre-ui::group-pages group)))
    (check "ordinary file tabs have working close controls"
           (and tabs (every (lambda (tab)
                              (find-widgets (cdr tab)
                                            (lambda (widget)
                                              (and (typep widget 'gtk:button)
                                                   (gtk:widget-has-css-class widget "cadre-tab-close")))))
                            tabs))))
  (check "opening an open file reuses its tab"
         (progn (open-file-path (merge-pathnames "notes.txt" *root*))
                (= 2 (length (tab-titles)))))
  ;; Files load asynchronously, so either may have opened first.
  (unless (string= "hello.lisp" (first (tab-titles)))
    (adw:tab-view-reorder-first (cadre-ui::window-tab-view *window*)
                                (cadre-ui::view-page *window* (first (cadre-ui::buffer-views *window* (find-buffer "hello.lisp"))))))
  (show-buffer-named "notes.txt")
  (press "C-S-TAB")
  (check "previous-tab selects hello.lisp"
         (string= "hello.lisp" (buffer-name (view-buffer (current-view)))))
  (check "hello.lisp is in Lisp mode" (eq 'lisp-mode (buffer-major-mode (current-buffer))))
  (let ((buffer (current-buffer)))
    (buffer-insert buffer (format nil ";;; Edited by the smoke test~%") 0)
    (check "editing marks the buffer modified" (buffer-modified-p buffer))
    (check "the tab shows unsaved changes" (string= "hello.lisp ●" (first (tab-titles))) (tab-titles)))
  )

(then 500
  (screenshot "02-tabs")                ; after GTK has drawn the step before
  (press "C-s"))                        ; save through the keymap

(then 1000
  (let ((buffer (current-buffer)))
    (check "C-s saved the buffer" (not (buffer-modified-p buffer)))
    (check "the file on disk has the edit"
           (search ";;; Edited by the smoke test"
                   (uiop:read-file-string (merge-pathnames "src/hello.lisp" *root*))))
    (check "the tab no longer shows changes" (string= "hello.lisp" (first (tab-titles))))))

(then 300
  (press "C-k C-l"))                    ; toggle-layout, a two-key sequence

(then 800
  (check "C-k C-l switched to the vertical layout"
         (eq :horizontal (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (check "the panel is beside the editor, wide"
         (> (- (gtk:widget-get-width (cadre-ui::window-main-paned *window*))
               (gtk:paned-get-position (cadre-ui::window-main-paned *window*)))
            300))
  )

(then 300
  (check "in the vertical layout the editor fits beside the panel, unclipped"
         (<= (gtk:widget-get-width (cadre-ui::window-editor-stack *window*))
             (gtk:paned-get-position (cadre-ui::window-main-paned *window*)))
         (format nil "editor ~d px, divider at ~d"
                 (gtk:widget-get-width (cadre-ui::window-editor-stack *window*))
                 (gtk:paned-get-position (cadre-ui::window-main-paned *window*))))
  (screenshot "03-vertical")
  (press "C-k C-l"))

(then 800
  (check "and back to horizontal"
         (eq :vertical (gtk:orientable-get-orientation (cadre-ui::window-main-paned *window*))))
  (press "C-b")
  (check "C-b hides the sidebar" (not (gtk:widget-get-visible (cadre-ui::window-sidebar *window*))))
  (press "C-b")
  (check "C-b shows it again" (gtk:widget-get-visible (cadre-ui::window-sidebar *window*)))
  (press "C-k C-q")
  (check "an undefined sequence is reported"
         (search "C-k C-q is undefined"
                 (gtk:label-get-text (cadre-ui::window-status-message *window*)))))

(then 300
  (press "C-w"))                        ; close hello.lisp (saved, so no question)

(then 800
  (check "C-w closed the tab" (equal '("notes.txt") (tab-titles)) (tab-titles))
  (check "closing the last view killed the buffer"
         (null (find "hello.lisp" (buffer-list) :key #'buffer-name :test #'string=)))
  (press "C-n"))

(then 800
  (check "C-n opened an untitled buffer"
         (equal '("notes.txt" "untitled") (tab-titles)) (tab-titles))
  (setf cadre-ui::*keybinding-profile* :emacs)
  (let ((buffer (current-buffer)))
    (buffer-insert buffer "abc" 0)
    (setf (buffer-point buffer) 0)
    (press "C-f C-f")
    (check "Emacs C-f moves forward" (= 2 (buffer-point buffer)) (buffer-point buffer))
    (press "C-e")
    (check "Emacs C-e moves to the end of the line" (= 3 (buffer-point buffer)))
    (setf (buffer-modified-p buffer) nil))
  (setf cadre-ui::*keybinding-profile* :standard))

(then 300
  (screenshot "04-end")
  (press "C-w")
  (open-file-path (merge-pathnames "src/m1.lisp" *root*)))

;;; M1: Lisp editing
(section "lisp-editing")
(open-files "src/m1.lisp")
(then 800
  (check "m1.lisp is open" (string= "m1.lisp" (buffer-name (current-buffer))))
  (check "defun is highlighted as a definer" (has-face-p 0 1 :definer))
  (check "the function name is highlighted" (has-face-p 0 7 :definition-name))
  (check "*x* is highlighted as a special variable" (has-face-p 3 8 :special-variable))
  (check "comments are highlighted" (has-face-p 4 0 :comment))
  (check "parens are colored by depth"
         (and (has-face-p 0 0 '(:paren 0)) (has-face-p 0 12 '(:paren 1))))
  (check "the cursor's line is highlighted" (has-face-p 0 3 :current-line))
  (set-cursor 1 9)                      ; just after (* w h)
  )

(then 300
  (check "the paren before the cursor and its match are marked"
         (and (has-face-p 1 2 :paren-match) (has-face-p 1 8 :paren-match)))
  ;; Replace what's between the marked parens, as a reload or Claude's
  ;; edit does: the new text must not take the mark.
  (let ((text (buffer-text (current-buffer))))
    (gtk:text-buffer-delete text (cadre-ui::line-iter text 1 3) (cadre-ui::line-iter text 1 8))
    (gtk:text-buffer-insert text (cadre-ui::line-iter text 1 3) "* w h" -1))
  (set-cursor 0 3))

(then 300
  (check "a paren's mark doesn't spread to text put between the parens"
         (notany (lambda (c) (has-face-p 1 c :paren-match)) '(2 3 4 5 6 7 8)))
  (set-cursor 0 0)
  (press "C-M-f")
  (check "C-M-f moves over the defun" (equal '(1 10) (cursor)) (cursor))
  (press "C-M-b")
  (check "C-M-b moves back" (equal '(0 0) (cursor)) (cursor))
  (set-cursor 0 19)                     ; end of "(defun area (w h)"
  (press "RET")
  (check "RET indents the new line as a body" (equal '(1 2) (cursor)) (cursor))
  (insert-at-cursor "(let ((a 1)")
  (press "RET")
  (check "RET aligns let bindings" (equal '(2 8) (cursor)) (cursor))
  (insert-at-cursor "(b 2))")
  (gtk:text-buffer-insert (buffer-text (current-buffer))
                          (cadre-ui::line-iter (buffer-text (current-buffer)) 3 0) "      " -1)
  (set-cursor 3 0)
  (press "TAB")
  (check "TAB fixes a line's indentation" (string= "    (* w h))" (line-text 3)) (line-text 3))
  (screenshot "05-lisp"))

(then 300
  (press "C-S-p")
  (check "C-S-p opens the command palette"
         (and (picker) (gtk:widget-get-visible (cadre-ui::picker-popover (picker)))))
  (check "one click on a row chooses it"
         (gtk:list-view-get-single-click-activate (cadre-ui::picker-list-view (picker))))
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "toggle lay"))

(then 300
  (check "the palette filters by fuzzy match"
         (string= "Toggle layout"
                  (command-title (gobject:lisp-object-value
                                  (gio:list-model-get-item (cadre-ui::picker-store (picker)) 0)))))
  (screenshot "06-palette")
  (cadre-ui::choose (picker)))

(then 500
  (check "choosing a command runs it" (eq :vertical (cadre-ui::window-layout *window*)))
  (call-command 'toggle-layout)
  (press "C-p")
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "notes"))

(then 300
  (cadre-ui::choose (picker)))

(then 800
  (check "quick open opens a file by name" (string= "notes.txt" (buffer-name (current-buffer))))
  (call-command 'previous-tab)
  (press "C-f")
  (gtk:editable-set-text (cadre-ui::find-bar-entry (cadre-ui::window-find-bar *window*)) "w"))

(then 400
  (let ((fb (cadre-ui::window-find-bar *window*)))
    (check "find counts every match"
           (= 2 (length (cadre-ui::find-bar-matches fb)))
           (gtk:label-get-text (cadre-ui::find-bar-status fb)))
    (check "every match is highlighted" (and (has-face-p 0 13 :search) (has-face-p 3 7 :search)))
    (screenshot "07-find")
    (let ((before (cadre-ui::find-bar-current fb)))
      (cadre-ui::find-step fb 1)
      (check "Enter moves to the next match"
             (= (mod (1+ before) 2) (cadre-ui::find-bar-current fb))))
    (cadre-ui::find-close fb)
    (check "closing the find bar selects the match"
           (gtk:text-buffer-get-has-selection (buffer-text (current-buffer))))
    (setf (buffer-modified-p (current-buffer)) nil)
    (call-command 'close-tab)
    (open-file-path (merge-pathnames "src/big.lisp" *root*))))

(then 2000
  (check "big.lisp is open" (string= "big.lisp" (buffer-name (current-buffer))))
  (let* ((view (current-view))
         (gtk-buffer (buffer-text (current-buffer)))
         (lines (gtk:text-buffer-get-line-count gtk-buffer))
         (end-ms (timed (gtk:text-buffer-place-cursor gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer))
                        (cadre-ui::scroll-to-cursor view)
                        (cadre-ensure-all)))
         (type-ms (timed (gtk:text-buffer-insert gtk-buffer (cadre-ui::line-iter gtk-buffer 10 0) "x" -1)
                         (cadre-ui::highlight-view view))))
    (check "lexing to the end of a big file is quick" (< end-ms 1500) (format nil "~d lines in ~d ms" lines end-ms))
    (check "an edit re-highlights quickly" (< type-ms 50) (format nil "~d ms" type-ms))
    (setf (buffer-modified-p (current-buffer)) nil)))

(then 500
  (check "continuation lines of a docstring are highlighted as string"
         (and (has-face-p 4534 1 :string) (has-face-p 4536 1 :string)))
  (screenshot "08-end")
  (call-command 'close-tab)
  (open-file-path (merge-pathnames "src/m2.lisp" *root*))
  (call-command 'lisp))

;;; M2: the connected Lisp
(section "connected-lisp")
(open-files "src/m2.lisp")
(then 50 (unless (or (cadre-ui::connected-p) cadre-ui::*connecting*) (call-command 'lisp)))
(then-when ((cadre-ui::connected-p) :timeout 120)
  (check "M-x lisp starts a Lisp and connects" (cadre-ui::connected-p))
  (check "the status bar shows the connection"
         (search "SBCL" (gtk:button-get-label (cadre-ui::window-status-connection *window*))))
  (check "the REPL shows a prompt" (search "CL-USER> " (repl-text)))
  (cadre-ui::show-repl-page :focus t)
  (cadre-ui::set-repl-input "(progn (princ \"hi\") (* 6 7))")
  (call-command 'repl-return))

(then-when ((search (format nil "42~%CL-USER> ") (repl-text)))
  (check "the REPL evaluates and prints the result" (search (format nil "42~%CL-USER> ") (repl-text))
         (substitute #\| #\Newline (repl-text)))
  (check "the REPL shows output" (search "hi" (repl-text)))
  ;; Presentations
  (let ((presentation (first (cadre-ui::repl-presentations))))
    (check "results are presentations" (and presentation (string= "42" (cadre-ui::presentation-text presentation)))
           (and presentation (cadre-ui::presentation-text presentation)))
    (cadre-ui::set-repl-input "(list :a ")
    (when presentation (cadre-ui::copy-presentation-to-input presentation))
    (gtk:text-buffer-insert-at-cursor (cadre-ui::repl-gtk-buffer) ")" -1)
    (check "a result copied to the input shows as its text" (string= "(list :a 42)" (cadre-ui::repl-input))
           (cadre-ui::repl-input))
    (check "but goes to the Lisp as the object"
           (search "#.(swank:lookup-presented-object-or-lose" (cadre-ui::repl-input-for-lisp))
           (cadre-ui::repl-input-for-lisp)))
  (call-command 'repl-return))

(then-when ((search "(:A 42)" (repl-text)))
  (check "and the Lisp gets the object" (search "(:A 42)" (repl-text)))
  (let ((presentation (first (cadre-ui::repl-presentations))))
    (cadre-ui::inspect-presentation presentation)))

(defun inspector-text ()
  (format nil "~a ~a" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))
          (cadre:text-string (cadre-ui::inspector-buffer))))

(then-when ((search "proper list" (inspector-text)) :timeout 10)
  (check "clicking a result inspects it" (search "proper list" (inspector-text)) (inspector-text))
  (show-buffer-named "m2.lisp")
  (set-cursor 1 3)
  (call-command 'compile-defun))

(then-when (cadre-ui::*notes*)
  (check "compiling a defun reports its warning"
         (find :warning cadre-ui::*notes* :key (lambda (s) (compiler-note-severity (cadre-ui::sn-note s)))))
  (check "the warning is underlined in the buffer" (has-face-p 1 23 :note-warning))
  (check "the Problems tab shows the count"
         (let ((stack (cadre-ui::panel-stack (cadre-ui::window-panel *window*))))
           (search "(1)" (gtk:stack-page-get-title
                          (gtk:stack-get-page stack (gtk:stack-get-child-by-name stack "problems"))))))
  (set-cursor 0 3)
  (call-command 'compile-defun))

(then 1000
  (set-cursor 2 4)                      ; in (twice 21): not a definition, so evaluated
  (call-command 'compile-or-eval-defun))

(then-when ((cadre-ui::buffer-local (current-buffer) :inline-result))
  (let ((shown (cadre-ui::buffer-local (current-buffer) :inline-result)))
    (check "Ctrl+Return on a call shows its value inline"
           (and shown (search "⇒ 42" (gtk:label-get-text (cdr shown))))
           (and shown (gtk:label-get-text (cdr shown)))))
  (screenshot "09-inline")
  (let ((label (cdr (cadre-ui::buffer-local (current-buffer) :inline-result))))
    (insert-at-cursor " ")
    (check "editing removes the inline value" (and (null (cadre-ui::buffer-local (current-buffer) :inline-result))
                                                   label (not (gtk:widget-get-visible label))))
    (gtk:text-buffer-undo (buffer-text (current-buffer))))
  (setf (buffer-modified-p (current-buffer)) nil)
  (set-cursor 2 10)                     ; after (twice 21)
  (call-command 'eval-last-expression))

(then-when ((search "=> 42" (gtk:label-get-text (cadre-ui::window-status-message *window*))))
  (check "evaluating an expression shows its value"
         (search "=> 42" (gtk:label-get-text (cadre-ui::window-status-message *window*))))
  (set-cursor 2 7)                      ; (twice |21)
  (cadre-ui::focus-view (current-view))
  (cadre-ui::request-autodoc (current-view)))

(then-when ((search "twice" (gtk:label-get-text (cadre-ui::window-status-arglist *window*))))
  (check "the status bar shows the arglist"
         (search "(twice x)" (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
         (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
  (set-cursor 2 3)
  (call-command 'edit-definition))

(then 1000
  (check "M-. goes to the definition" (equal '(0 0) (cursor)) (cursor))
  (call-command 'pop-definition)
  (check "M-, comes back" (equal 2 (first (cursor))) (cursor))
  (set-cursor 3 0)
  (insert-at-cursor "(mvb")
  (call-command 'complete-symbol))

(then-when ((cadre-ui::completion-open-p))
  (check "completion offers candidates" (cadre-ui::completion-open-p))
  (cadre-ui::accept-completion)
  (check "accepting a completion inserts it" (search "(multiple-value-bind" (line-text 3)) (line-text 3))
  (setf (buffer-modified-p (current-buffer)) nil)
  (cadre-ui::show-repl-page :focus t)
  (cadre-ui::set-repl-input "(error \"boom\")")
  (call-command 'repl-return))

(then-when (cadre-ui::*debug-levels*)
  (check "an error opens the debugger" cadre-ui::*debug-levels*)
  (check "the debugger shows the condition"
         (search "boom" (first (cadre-ui::dl-condition (first cadre-ui::*debug-levels*))))))

(then 300
  (screenshot "09-debugger")
  (call-command 'debugger-abort))

(then-when ((null cadre-ui::*debug-levels*))
  (check "aborting leaves the debugger" (null cadre-ui::*debug-levels*))
  (check "the Debugger tab goes away when there is no error"
         (not (gtk:stack-page-get-visible (cadre-ui::panel-page (cadre-ui::window-panel *window*) "debugger"))))
  (show-buffer-named "m2.lisp")
  (set-cursor 0 9)                      ; on "twice"
  (call-command 'describe-symbol))

(then-when ((find-buffer "*Help*"))
  (call-command 'load-project)
  (check "describe-symbol shows documentation" (search "TWICE" (buffer-string (find-buffer "*Help*")))
         (let ((b (find-buffer "*Help*"))) (if b (subseq (buffer-string b) 0 (min 200 (length (buffer-string b)))) "no *Help* buffer")))
  (call-command 'show-repl))

(then-when ((search (format nil "Loading system smoke") (repl-text)))
  (check "load-project loads the folder's system in the REPL" (search "Loading system smoke" (repl-text))))

(then-when ((and (not (cadre-ui::repl-busy cadre-ui::*repl*))
                 (search "Loading system smoke" (repl-text))))
  (cadre-ui::repl-eval "(smoke-lib-fn)"))

(then-when ((search ":LOADED" (repl-text)))
  (check "the loaded system's code runs" (search ":LOADED" (repl-text)) (subseq (repl-text) (max 0 (- (length (repl-text)) 300)))))

;;; M3: debugger and tools
(section "debugger")
(then 300
  (open-file-path (merge-pathnames "src/m3.lisp" *root*)
                  :then (lambda (view) (declare (ignore view)) (call-command 'compile-and-load-file))))

(then-when ((has-face-p 2 18 :macro) :timeout 40)
  (check "a macro from the image is colored as one" (has-face-p 2 18 :macro))
  (check "a call to an undefined function is marked" (has-face-p 4 26 :undefined-function))
  (check "a special variable without earmuffs is colored from the image" (has-face-p 3 20 :special-variable))
  (check "a LET binding is not marked as an undefined call" (not (has-face-p 6 24 :undefined-function)))
  (check "a defined function is not marked" (not (has-face-p 2 26 :undefined-function)))
  (screenshot "11-image-faces")
  (set-cursor 2 17)                     ; before (my-mac (twice 3))
  (call-command 'expand-macro-once))

(then-when ((find-buffer "*Macroexpansion*"))
  (check "macroexpand shows the expansion"
         (search "(LIST (TWICE 3) (TWICE 3))" (buffer-string (find-buffer "*Macroexpansion*")))
         (buffer-string (find-buffer "*Macroexpansion*")))
  (call-command 'close-tab)
  (show-buffer-named "m3.lisp")
  (set-cursor 2 28)                     ; on "twice"
  (call-command 'who-calls))

(then-when ((plusp (hash-table-count cadre-ui::*reference-rows*)))
  (check "who-calls finds the caller"
         (loop for x being the hash-values of cadre-ui::*reference-rows*
               thereis (search "CALLER" (string-upcase (xref-name x)))))
  (check "the References tab shows the count" (search "(" (panel-page-title "references"))
         (panel-page-title "references"))
  (screenshot "12-references")
  (cadre-ui::inspect-string "(list 1 \"two\" 3)" "COMMON-LISP-USER"))

(then-when ((search "CONS" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
  (check "inspecting shows the object" (search "CONS" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
  (check "the inspector shows its parts as links"
         (find :value (cadre-ui::ins-links cadre-ui::*inspector*) :key #'third))
  (screenshot "13-inspector")
  (let ((link (find-if (lambda (l) (and (eq (third l) :value)
                                        (search "\"two\"" (text-string (cadre-ui::inspector-buffer))
                                                :start2 (first l) :end2 (second l))))
                       (cadre-ui::ins-links cadre-ui::*inspector*))))
    (check "the string element is a link" link)
    (when link (cadre-ui::follow-inspector-link link))))

(then-when ((search "CHARACTER" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
  (check "clicking a value inspects it"
         (search "CHARACTER" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
         (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
  (call-command 'inspector-back))

(defvar *dragged-divider* nil)

(then-when ((search "CONS" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
  (check "Back returns to the previous object" t)
  ;; As if the user dragged the divider: the debugger must leave it there.
  (setf *dragged-divider* (- (gtk:paned-get-position (cadre-ui::window-main-paned *window*)) 37))
  (gtk:paned-set-position (cadre-ui::window-main-paned *window*) *dragged-divider*)
  (cadre-ui::repl-eval "(deep 3)"))

(defun deep-frame ()
  (find-if (lambda (f) (search "(DEEP 0" (frame-description f)))
           (cadre-ui::dl-frames (first cadre-ui::*debug-levels*))))

(then-when (cadre-ui::*debug-levels*)
  (check "opening the debugger keeps the dragged panel size"
         (= *dragged-divider* (gtk:paned-get-position (cadre-ui::window-main-paned *window*)))
         (gtk:paned-get-position (cadre-ui::window-main-paned *window*)))
  (check "the debugger lists frames" (deep-frame)
         (mapcar #'frame-description (subseq (cadre-ui::dl-frames (first cadre-ui::*debug-levels*)) 0 3)))
  (let* ((n (and (deep-frame) (frame-number (deep-frame))))
         (expander (find-if (lambda (e) (search (format nil "~d: (DEEP 0" n)
                                                (gtk:label-get-text (gtk:expander-get-label-widget e))))
                            (find-widgets cadre-ui::*debugger-box* (lambda (w) (typep w 'gtk:expander))))))
    (check "each frame has an expander" expander)
    (when expander (gtk:expander-set-expanded expander t))))

(then-when ((member "N" (label-texts cadre-ui::*debugger-box*) :test #'equal))
  (check "opening a frame shows its locals" (member "N" (label-texts cadre-ui::*debugger-box*) :test #'equal))
  (screenshot "14-debugger-frame")
  (let ((level (first cadre-ui::*debug-levels*)))
    (cadre-ui::frame-rex level (swank-call "swank:eval-string-in-frame" "(+ n 100)" (frame-number (deep-frame))
                                           "COMMON-LISP-USER" 1 100)
                         :on-ok (lambda (v) (setf (cadre-ui::buffer-local (current-buffer) :smoke-eval) v)))))

(then-when ((cadre-ui::buffer-local (current-buffer) :smoke-eval))
  (check "evaluating in a frame sees its locals"
         (search "100" (cadre-ui::buffer-local (current-buffer) :smoke-eval))
         (cadre-ui::buffer-local (current-buffer) :smoke-eval))
  (cadre-ui::show-frame-source (first cadre-ui::*debug-levels*) (frame-number (deep-frame))))

(then 1500
  (check "Source shows the frame's code" (and (string= (buffer-name (current-buffer)) "m3.lisp")
                                              (= 5 (first (cursor))))
         (list (buffer-name (current-buffer)) (cursor)))
  (let ((before (length (cadre-ui::dl-frames (first cadre-ui::*debug-levels*))))
        (more (find-if (lambda (b) (equal (gtk:button-get-label b) "More Frames"))
                       (find-widgets cadre-ui::*debugger-box* (lambda (w) (typep w 'gtk:button))))))
    (setf (cadre-ui::buffer-local (current-buffer) :frames-before) before)
    (check "a long backtrace offers more frames" more)
    (when more (gtk:widget-activate more))))

(then 1500
  (check "More Frames fetches more"
         (> (length (cadre-ui::dl-frames (first cadre-ui::*debug-levels*)))
            (cadre-ui::buffer-local (current-buffer) :frames-before)))
  (cadre-ui::focus-debugger)
  (cadre-ui::debugger-key (gdk:keyval-from-name "a") nil))

(then-when ((null cadre-ui::*debug-levels*))
  (check "pressing a in the debugger aborts" (null cadre-ui::*debug-levels*))
  (cadre-ui::toggle-trace-of "deep" "COMMON-LISP-USER"))

;;; Lisp tools: tracing and stepping
(section "tracing" :needs ("debugger"))

(defun trace-page () cadre-ui::*trace-page*)
(defun trace-calls () (cadre-ui::tp-tree (trace-page)))
(defun traced-call (name)
  (loop for call being the hash-values of (trace-tree-calls (trace-calls))
        when (string= name (trace-call-name call)) collect call))

(then-when ((member "deep" (cadre-ui::tp-specs (trace-page)) :test #'string=))
  (check "tracing a function lists it on the Trace page" t)
  (check "and shows the page" (equal "trace" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (cadre-ui::toggle-trace-of "get-thing" "COMMON-LISP-USER"))

(then-when ((member "get-thing" (cadre-ui::tp-specs (trace-page)) :test #'string=))
  (cadre-ui::repl-eval "(progn (get-thing) (ignore-errors (deep 1)) :traced)"))

(then-when ((and (= 3 (trace-tree-count (trace-calls)))
                 (notany (lambda (c) (eq :running (trace-call-state c))) (traced-call "deep")))
            :timeout 15)
  (check "calls of traced functions appear as they happen" t)
  (let ((get-thing (first (traced-call "get-thing")))
        (outer (find-if #'trace-call-children (traced-call "deep"))))
    (check "with their values" (equal '("5") (trace-call-results get-thing)) (trace-call-results get-thing))
    (check "calls made inside a call sit under it"
           (and outer (equal '("1") (trace-call-args outer))
                (equal '("0") (trace-call-args (first (trace-call-children outer))))))
    (check "a call left by an error says so" (and outer (eq :unwound (trace-call-state outer))))
    (check "each call is a row" (= 3 (hash-table-count cadre-ui::*trace-rows*)))
    (check "the tab counts the calls" (equal "Trace (3)" (panel-page-title "trace")) (panel-page-title "trace")))
  (check "values are links"
         (find-if (lambda (b) (member "5" (label-texts b) :test #'equal))
                  (find-widgets (cadre-ui::tp-list (trace-page)) (lambda (w) (typep w 'gtk:button))))))

(then 500
  (screenshot "35-trace")
  (let ((five (find-if (lambda (b) (member "5" (label-texts b) :test #'equal))
                       (find-widgets (cadre-ui::tp-list (trace-page)) (lambda (w) (typep w 'gtk:button))))))
    (when five (gtk:widget-activate five))))

(then-when ((equal "inspector" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (check "clicking a value inspects it"
         (search "INTEGER" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
         (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
  (call-command 'cadre-ui::show-traces))

(then 600
  (let ((row (loop for row being the hash-keys of cadre-ui::*trace-rows* using (hash-value call)
                   when (trace-call-children call) return row)))
    (check "a call with calls inside can fold" row)
    (when row (gtk:widget-activate row))))

(then 400
  (check "folding hides the calls inside" (= 2 (hash-table-count cadre-ui::*trace-rows*)))
  (call-command 'cadre-ui::untrace-all-functions))

(then-when ((null (cadre-ui::tp-specs (trace-page))))
  (check "Untrace All stops tracing" t)
  (call-command 'cadre-ui::clear-traces))

(then-when ((zerop (trace-tree-count (trace-calls))))
  (check "Clear forgets the calls" (zerop (hash-table-count cadre-ui::*trace-rows*)))
  (open-file-path (merge-pathnames "src/m7.lisp" *root*)))

(then-when ((find-buffer "m7.lisp"))
  (show-buffer-named "m7.lisp")
  (call-command 'cadre-ui::load-file))

(then-when ((search "Loaded m7.lisp" (gtk:label-get-text (cadre-ui::window-status-message *window*))))
  (set-cursor 2 3)
  (call-command 'cadre-ui::step-expression))

(then-when ((and (picker) (gtk:widget-get-visible (cadre-ui::picker-popover (picker)))) :timeout 15)
  (check "stepping a definition compiles it and asks for a call"
         (equal "(step-me " (gtk:editable-get-text (cadre-ui::picker-entry (picker))))
         (gtk:editable-get-text (cadre-ui::picker-entry (picker))))
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "(step-me 3)")
  (cadre-ui::choose (picker)))

(defun stepping-text ()
  (let ((level (first cadre-ui::*debug-levels*)))
    (and (cadre-ui::stepping-level-p level) (first (cadre-ui::dl-condition level)))))

(then-when ((stepping-text) :timeout 15)
  (check "the stepper stops at the first form" (search "STEP-ME" (stepping-text)) (stepping-text))
  (check "and the panel shows the Stepper" (equal "Stepper ●" (panel-page-title "debugger")))
  (call-command 'cadre-ui::step-into))

(then-when ((search "(DOUBLE-IT X)" (or (stepping-text) "")) :timeout 10)
  (check "Step Into goes into the function" (search "(DOUBLE-IT X)" (or (stepping-text) "")) (stepping-text)))

(then-when (cadre-ui::*stepped-form* :timeout 5)
  (check "the form being stepped is highlighted in its source"
         (and (eq (first cadre-ui::*stepped-form*) (find-buffer "m7.lisp"))
              (= 2 (first (cursor))))
         (list (buffer-name (first cadre-ui::*stepped-form*)) (cursor)))
  (screenshot "36-stepper")
  (cadre-ui::focus-debugger)
  (cadre-ui::debugger-key (gdk:keyval-from-name "x") nil))

(then-when ((search "(DOUBLE-IT Y)" (or (stepping-text) "")) :timeout 10)
  (check "x steps over to the next call" (search "(DOUBLE-IT Y)" (or (stepping-text) ""))
         (stepping-text))
  (call-command 'cadre-ui::stop-stepping))

(then-when ((and (null cadre-ui::*debug-levels*)
                 (not (gtk:stack-page-get-visible (cadre-ui::panel-page (cadre-ui::window-panel *window*) "debugger"))))
            :timeout 10)
  (check "Resume finishes the evaluation" (search "13" (gtk:label-get-text (cadre-ui::window-status-message *window*)))
         (gtk:label-get-text (cadre-ui::window-status-message *window*)))
  (check "and the highlight goes" (null cadre-ui::*stepped-form*))
  (call-command 'show-systems))

(then-when ((let ((row (cdr (assoc "smoke" cadre-ui::*system-rows* :test #'string=))))
              (and row (equal "Loaded" (adw:expander-row-get-subtitle row)))))
  (check "the Systems view lists the project's system as loaded" t)
  (let ((row (cdr (assoc "smoke" cadre-ui::*system-rows* :test #'string=))))
    (adw:expander-row-set-expanded row t)))

(then-when ((find-widgets (cdr (assoc "smoke" cadre-ui::*system-rows* :test #'string=))
                          (lambda (w) (and (typep w 'adw:action-row)
                                           (equal "lib.lisp" (adw:preferences-row-get-title w))))))
  (check "opening a system lists its files" t)
  (screenshot "15-systems")
  (call-command 'show-explorer)
  (check "the explorer comes back" (equal "explorer" (cadre-ui::sidebar-page *window*))))

;;; M4: Claude (a stand-in CLI, scripts/fake-claude, plays Claude's part)
(section "claude" :needs ("connected-lisp"))
(defun chat-texts () (label-texts (cadre-ui::chat-messages cadre-ui::*chat*)))
(defun chat-says (text) (some (lambda (s) (search text s)) (chat-texts)))
(defun chat-type (text)
  (text-replace-contents (gtk:text-view-get-buffer (cadre-ui::chat-input cadre-ui::*chat*)) text)
  (call-command 'cadre-ui::chat-send))

(defvar *signed-out* (merge-pathnames "fake-claude-signed-out" *root*))

(then-when ((and cadre-ui::*claude-status* (cadre-ui::claude-ready-p)) :timeout 20)
  (check "Cadre checked Claude Code at startup"
         (and cadre-ui::*claude-status* (cadre-ui::claude-ready-p)))
  (with-open-file (o *signed-out* :direction :output :if-exists :supersede) (write-line "out" o))
  (sb-posix:setenv "FAKE_CLAUDE_SIGNED_OUT" (namestring *signed-out*) 1)
  (setf cadre-ui::*claude-status* nil)
  ;; Opening the Claude tab by clicking it checks too.
  (cadre-ui::set-panel-visible *window* t)
  (cadre-ui::panel-show (cadre-ui::window-panel *window*) "repl")
  (cadre-ui::panel-show (cadre-ui::window-panel *window*) "claude"))

(then-when ((and cadre-ui::*claude-status* (not cadre-ui::*claude-checking*)))
  (check "a signed-out CLI shows the sign-in page"
         (equal "status" (gtk:stack-get-visible-child-name (cadre-ui::chat-stack cadre-ui::*chat*))))
  (check "the sign-in page says which claude it checked"
         (search "fake-claude" (gtk:label-get-text cadre-ui::*claude-status-detail*))
         (gtk:label-get-text cadre-ui::*claude-status-detail*))
  (call-command 'cadre-ui::claude-sign-in))

(then-when ((search "fake-sign-in" (gtk:link-button-get-uri cadre-ui::*sign-in-link*)))
  (check "Sign In shows the sign-in link" t)
  (screenshot "16-sign-in")
  (gtk:editable-set-text cadre-ui::*sign-in-code* "good-code")
  (cadre-ui::submit-sign-in-code))

(then-when ((equal "chat" (gtk:stack-get-visible-child-name (cadre-ui::chat-stack cadre-ui::*chat*))))
  (check "pasting the code signs in and opens the chat" (cadre-ui::claude-ready-p))
  (chat-type "hello"))

(then-when ((chat-says "Done."))
  (check "Claude's reply streams into the chat and is rendered" (chat-says "Hello from fake Claude"))
  (check "code blocks in replies are shown as code" (member "(defun hi () :hi)" (chat-texts) :test #'equal))
  (let* ((label (first (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*)
                                     (lambda (w) (and (typep w 'gtk:label)
                                                      (equal "(defun hi () :hi)" (gtk:label-get-text w)))))))
         (family (and label (pango:font-description-get-family
                             (pango:context-get-font-description (gtk:widget-get-pango-context label))))))
    (check "code blocks in replies are in the editor's font" (and family (search "Menlo" family)) family))
  (check "bold around code in a reply is bold, with the code inside"
         (equal "<b>It is at <tt>inner</tt>.</b> Tokens"
                (cadre-ui::inline-markup "**It is at `inner`.** Tokens")))
  (check "a ``double-backtick`` code span shows its backticks inside, not around"
         (equal "by <tt>`(...)</tt> here" (cadre-ui::inline-markup "by `` `(...) `` here")))
  (check "the turn ends" (not (cadre-ui::chat-busy cadre-ui::*chat*)))
  (check "the panel shows the conversation's cost"
         (search "$0.0042" (gtk:label-get-text (cadre-ui::chat-status cadre-ui::*chat*)))
         (gtk:label-get-text (cadre-ui::chat-status cadre-ui::*chat*)))
  (screenshot "16-claude")
  (call-command 'cadre-ui::ask-claude-about-problems))

(then-when (cadre-ui::*reviews*)
  (let ((review (first cadre-ui::*reviews*)))
    (check "Claude used get_problems to see the warning" (chat-says "an undefined variable."))
    (check "propose_edit opens an inline review" (gtk:revealer-get-reveal-child cadre-ui::*review-bar*))
    (check "the review bar names the file and counts the lines"
           (search "m2.lisp  +1 −1" (gtk:label-get-text cadre-ui::*review-label*))
           (gtk:label-get-text cadre-ui::*review-label*))
    (let ((merged (text-string (cadre-ui::rv-gtk-buffer review))))
      (check "the review shows the removed and added lines"
             (and (search "(+ y undefined-thing)" merged) (search "(+ y 1)" merged))))
    (check "the tab shows the review while it lasts"
           (eq (gtk:text-view-get-buffer (view-text-view (cadre-ui::rv-view review))) (cadre-ui::rv-gtk-buffer review)))
    (check "the buffer is untouched until accepted"
           (search "undefined-thing" (buffer-string (find-buffer "m2.lisp")))))
  (screenshot "17-review")
  (call-command 'cadre-ui::accept-edit))

(then-when ((chat-says "Result:"))
  (let ((buffer (find-buffer "m2.lisp")))
    (check "accepting applies the edit" (and (search "(+ y 1)" (buffer-string buffer))
                                             (not (search "undefined-thing" (buffer-string buffer)))))
    (check "accepting saves the file" (search "(+ y 1)" (uiop:read-file-string (merge-pathnames "src/m2.lisp" *root*))))
    (check "Claude hears that the edit was accepted" (chat-says "accepted the edit"))
    (check "tool calls appear in the chat"
           (find-if (lambda (e) (search "propose_edit" (gtk:expander-get-label e)))
                    (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*) (lambda (w) (typep w 'gtk:expander)))))
    (check "the tab shows the buffer again"
           (eq (gtk:text-view-get-buffer (view-text-view (current-view))) (buffer-text (current-buffer))))
    (gtk:text-buffer-undo (buffer-text buffer))
    (check "one undo takes the whole edit back" (search "(+ y undefined-thing)" (buffer-string buffer)))
    (gtk:text-buffer-redo (buffer-text buffer))
    (setf (buffer-modified-p buffer) nil))
  (chat-type "Try another edit"))

(then-when (cadre-ui::*reviews*)
  (call-command 'cadre-ui::reject-edit))

(then-when ((chat-says "rejected"))
  (check "rejecting leaves the buffer alone" (search "(* 2 x)" (buffer-string (find-buffer "m2.lisp"))))
  (chat-type "permission please"))

(defun chat-button (label)
  (find-if (lambda (b) (equal (gtk:button-get-label b) label))
           (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*) (lambda (w) (typep w 'gtk:button)))))

(then-when ((chat-button "Allow Once"))
  (check "Claude Code's permission prompts ask in the chat" (chat-says "Claude wants to use Bash"))
  (screenshot "18-approval")
  (let ((width (gtk:widget-measure (cadre-ui::chat-scroller cadre-ui::*chat*) :horizontal -1)))
    (check "tool rows and approval buttons don't make the chat wider than a narrow panel"
           (< width 320) width))
  (gtk:widget-activate (chat-button "Allow Once")))

(then-when ((chat-says "Permission:"))
  (check "allowing tells Claude Code to go ahead" (chat-says "Permission: allow"))
  (chat-type "what is my context"))

(then-when ((chat-says "Context:"))
  (check "current_context describes what the user is looking at" (chat-says "Buffer: m2.lisp")
         (find-if (lambda (s) (search "Context:" s)) (chat-texts)))
  (call-command 'cadre-ui::claude-new-chat)
  (check "a new conversation starts empty" (not (chat-says "Context:"))))

(then 300
  (cadre-ui::set-chat-mode :agent)
  (check "Agent mode picks the agent model"
         (string= "opus" (nth (gtk:drop-down-get-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*))
                              cadre-ui::*claude-models*)))
  (chat-type "agent check"))

(then-when ((chat-says "Mode:"))
  (check "Agent mode starts Claude Code with the agent prompt and tools"
         (chat-says "agent=yes todo=yes model=opus")
         (find-if (lambda (s) (search "Mode:" s)) (chat-texts)))
  (setf (gethash '(:claude . "compile") cadre-ui::*session-allowed*) t)
  (chat-type "make a plan"))

(then-when ((chat-says "Compiled:"))
  (check "TodoWrite shows Claude's plan as a checklist"
         (let ((box (cadre-ui::chat-plan-box cadre-ui::*chat*)))
           (and box (member "◐ Compiling area" (label-texts box) :test #'equal)
                (member "☑ Read area" (label-texts box) :test #'equal)))
         (let ((box (cadre-ui::chat-plan-box cadre-ui::*chat*))) (and box (label-texts box))))
  (check "open_file shows the file" (string= "m1.lisp" (buffer-name (view-buffer (cadre-ui::selected-view *window*)))))
  (check "compile_defun compiles a form in the Lisp" (chat-says "Compiled: Compiled.")
         (find-if (lambda (s) (search "Compiled:" s)) (chat-texts)))
  (screenshot "21-agent")
  (cadre-ui::set-chat-mode :chat)
  (check "back to chat mode" (eq :chat (cadre-ui::chat-mode cadre-ui::*chat*))))

(then 500
  (screenshot "10-repl")
  (call-command 'disconnect))

(then-when ((not (cadre-ui::connected-p)))
  (check "disconnect closes the connection" (not (cadre-ui::connected-p))))

;;; Codex CLI / app-server (scripts/fake-codex, deterministic JSON-RPC peer)
(section "codex")
(then 200
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 1)
  (check "Codex can be selected in the Assistant panel"
         (eq :codex (cadre-ui::chat-provider cadre-ui::*chat*))))

(then-when ((and (cadre-ui::codex-ready-p) cadre-ui::*codex-model-catalog*) :timeout 20)
  (check "Cadre detects the signed-in Codex CLI" (cadre-ui::codex-ready-p))
  (check "the model picker selects the app-server default"
         (eq t (jget (nth (gtk:drop-down-get-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*))
                          cadre-ui::*codex-model-catalog*) "isDefault")))
  (chat-type "hello Codex"))

(then-when ((chat-says "Fake Codex reply") :timeout 20)
  (check "Codex streams a reply through app-server" (chat-says "Fake Codex reply"))
  (check "Codex renders Markdown after streaming completes"
         (and (chat-says "Fake Codex reply") (not (chat-says "**Codex**")))
         (chat-texts))
  (check "Codex can call a registered Cadre tool"
         (find-if (lambda (row) (search "current_context" (gtk:expander-get-label row)))
                  (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*)
                                (lambda (w) (typep w 'gtk:expander))))
         (chat-texts))
  (check "the Codex turn completes" (not (cadre-ui::chat-busy cadre-ui::*chat*)))
  (cadre-ui::set-chat-mode :agent)
  (chat-type "agent check"))

(then-when ((chat-says "Fake Codex agent mode") :timeout 20)
  (check "Agent guidance applies to a turn in the existing Codex thread"
         (chat-says "Fake Codex agent mode"))
  (check "Codex keeps the selected model across turns"
         (eq t (jget (nth (gtk:drop-down-get-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*))
                          cadre-ui::*codex-model-catalog*) "isDefault"))
         (gtk:drop-down-get-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*)))
  (chat-type "trigger fake failure"))

(then-when ((chat-says "Fake Codex turn failure") :timeout 20)
  (check "a failed Codex turn clears busy state and shows the error"
         (and (not (cadre-ui::chat-busy cadre-ui::*chat*))
              (chat-says "Fake Codex turn failure")))
  (check "a failed Codex turn preserves its thread for retry"
         (equal "fake-codex-thread" (cadre-ui::chat-codex-thread-id cadre-ui::*chat*)))
  (chat-type "retry after fake failure"))

(then-when ((chat-says "Fake Codex recovered after failure") :timeout 20)
  (check "Codex accepts a retry on the preserved thread"
         (and (not (cadre-ui::chat-busy cadre-ui::*chat*))
              (equal "fake-codex-thread" (cadre-ui::chat-codex-thread-id cadre-ui::*chat*)))
         (chat-texts))
  (cadre-ui::set-chat-mode :chat)
  ;; Later smoke sections exercise Claude-specific edits and assume the
  ;; provider used by the earlier Claude setup section is still selected.
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 0)
  (check "Codex smoke restores the Claude provider for later sections"
         (eq :claude (cadre-ui::chat-provider cadre-ui::*chat*))))


;;; Assistant conversations kept across sessions: the Codex conversation
;;; above is recorded, listed, restored and continued on its thread.
(section "conversations" :needs ("claude" "codex"))
(defvar *smoke-conversation* nil)
(defvar *smoke-claude-session* nil)

(then 100
  (cadre-ui::save-current-conversations)
  (setf *smoke-conversation*
        (find-if (lambda (record)
                   (and (eq :codex (getf record :provider))
                        (find "hello Codex" (getf record :entries)
                              :key (lambda (e) (getf (rest e) :text)) :test #'equal)))
                 (cadre-ui::saved-conversations)))
  (check "a Codex conversation is recorded with its thread"
         (and *smoke-conversation* (equal "fake-codex-thread" (getf *smoke-conversation* :thread-id)))
         *smoke-conversation*)
  (check "the record keeps replies and tool rows"
         (let ((entries (getf *smoke-conversation* :entries)))
           (and (find :assistant entries :key #'first)
                (find-if (lambda (e) (and (eq :tool (first e)) (search "current_context" (getf (rest e) :title))))
                         entries)))
         (mapcar #'first (getf *smoke-conversation* :entries)))
  (call-command 'cadre-ui::assistant-conversations))

(then-when ((and (picker) (gtk:widget-get-visible (cadre-ui::picker-popover (picker)))) :timeout 10)
  (check "Conversations lists the kept conversations"
         (plusp (length (cadre-ui::picker-items (picker)))))
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "hello Codex"))

(then 300
  (cadre-ui::choose (picker)))

(then 300
  (check "restoring switches to the conversation's provider" (eq :codex (cadre-ui::chat-provider cadre-ui::*chat*)))
  (check "the restored transcript shows messages, replies and tool rows"
         (and (chat-says "hello Codex") (chat-says "Fake Codex reply")
              (find-if (lambda (row) (search "current_context" (gtk:expander-get-label row)))
                       (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*) (lambda (w) (typep w 'gtk:expander)))))
         (chat-texts))
  (check "the restored conversation will resume its thread"
         (and (equal "fake-codex-thread" (cadre-ui::chat-codex-thread-id cadre-ui::*chat*))
              (null (cadre-ui::chat-codex-process cadre-ui::*chat*))
              (chat-says "Restored the conversation")))
  (screenshot "conversation-restored")
  (chat-type "after restoring"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*))
                 (<= 2 (count-if (lambda (s) (search "Fake Codex reply" s)) (chat-texts))))
            :timeout 20)
  (let ((params (cadre::codex-request (cadre-ui::chat-codex-process cadre-ui::*chat*) "test/thread-params")))
    (check "the next message resumes the same Codex thread"
           (equal "fake-codex-thread" (jget params "threadId"))
           (json-string params)))
  (cadre-ui::save-current-conversations)
  (check "and is added to the same record"
         (let ((record (find (getf *smoke-conversation* :id) (cadre-ui::saved-conversations)
                             :key (lambda (r) (getf r :id)) :test #'equal)))
           (and record (find "after restoring" (getf record :entries)
                             :key (lambda (e) (getf (rest e) :text)) :test #'equal))))
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 0)
  (check "conversation smoke restores the Claude provider for later sections"
         (eq :claude (cadre-ui::chat-provider cadre-ui::*chat*)))
  ;; A Claude conversation, then a new one, then the first again.
  (call-command 'cadre-ui::claude-new-chat)
  (chat-type "effort probe before restoring"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*)) (chat-says "Claude effort=")) :timeout 20)
  (setf *smoke-claude-session* (cadre-ui::chat-session-id cadre-ui::*chat*))
  (call-command 'cadre-ui::claude-new-chat)
  (check "a new Claude conversation has a new session"
         (not (equal *smoke-claude-session* (cadre-ui::chat-session-id cadre-ui::*chat*))))
  (cadre-ui::restore-conversation
   (find "effort probe before restoring" (cadre-ui::saved-conversations) :key (lambda (r) (getf r :title)) :test #'equal))
  (check "a restored Claude conversation will resume its session"
         (and (equal *smoke-claude-session* (cadre-ui::chat-session-id cadre-ui::*chat*))
              (cadre-ui::chat-started cadre-ui::*chat*)
              (chat-says "effort probe before restoring")))
  (chat-type "effort probe after restoring"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*))
                 (<= 2 (count-if (lambda (s) (search "Claude effort=" s)) (chat-texts))))
            :timeout 20)
  (check "the next message runs on the restored Claude session"
         (some (lambda (s) (search (format nil "session=~a" *smoke-claude-session*) s))
               (last (chat-texts) 3))
         (last (chat-texts) 3)))

;;; Assistant controls: exercise real GTK signals and inspect what the peers receive.
(section "assistant-controls" :needs ("claude" "codex"))

(defun choose-thinking (effort)
  (let ((index (position effort (cadre-ui::chat-effort-choices cadre-ui::*chat*) :test #'equal)))
    (unless index (error "Thinking level ~s is unavailable" effort))
    (gtk:drop-down-set-selected (cadre-ui::chat-effort-dropdown cadre-ui::*chat*) index)))

(defvar *effort-process* nil)
(defvar *effort-session* nil)
(defvar *effort-codex-server* nil)
(defvar *effort-layout* nil)
(defvar *effort-panel-size* nil)
(defvar *effort-narrow-rows* nil)
(defvar *effort-claude-original* nil)
(defvar *effort-codex-original* nil)

(then 100
  (setf *effort-claude-original* *claude-effort*
        *effort-codex-original* cadre-ui::*codex-effort*
        *effort-process* (cadre-ui::chat-process cadre-ui::*chat*)
        *effort-session* (cadre-ui::chat-session-id cadre-ui::*chat*))
  (choose-thinking "high")
  (chat-type "effort probe high"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*)) (chat-says "Claude effort=high")))
  (check "Claude receives the selected thinking level" (chat-says "Claude effort=high"))
  (check "changing Claude effort replaces its process"
         (not (eq *effort-process* (cadre-ui::chat-process cadre-ui::*chat*))))
  (check "changing Claude effort resumes its conversation"
         (and (equal *effort-session* (cadre-ui::chat-session-id cadre-ui::*chat*))
              (chat-says (format nil "session=~a" *effort-session*))))
  (setf *effort-process* (cadre-ui::chat-process cadre-ui::*chat*))
  (choose-thinking nil)
  (chat-type "effort probe default"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*)) (chat-says "Claude effort=default")))
  (check "Claude Default omits the effort argument" (chat-says "Claude effort=default"))
  (check "returning to Default also restarts Claude"
         (not (eq *effort-process* (cadre-ui::chat-process cadre-ui::*chat*))))
  (choose-thinking "low")
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 1)
  (check "Codex offers the selected model's thinking levels"
         (equal '(nil "medium" "high") (cadre-ui::chat-effort-choices cadre-ui::*chat*)))
  (choose-thinking "high")
  (setf *effort-codex-server* (cadre-ui::chat-codex-process cadre-ui::*chat*))
  (chat-type "effort probe high"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*))
                 (chat-says "Codex effort=high model=fake-gpt-default")))
  (check "Codex receives the chosen effort in the existing thread"
         (and (eq *effort-codex-server* (cadre-ui::chat-codex-process cadre-ui::*chat*))
              (chat-says "Codex effort=high model=fake-gpt-default thread=fake-codex-thread")))
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 0)
  (check "switching back restores Claude's thinking choice"
         (equal "low" (nth (gtk:drop-down-get-selected (cadre-ui::chat-effort-dropdown cadre-ui::*chat*))
                            (cadre-ui::chat-effort-choices cadre-ui::*chat*))))
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 1)
  (check "switching back restores Codex's thinking choice"
         (= 2 (gtk:drop-down-get-selected (cadre-ui::chat-effort-dropdown cadre-ui::*chat*))))
  (gtk:drop-down-set-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*) 1)
  (check "changing models refreshes supported thinking levels"
         (equal '(nil "low") (cadre-ui::chat-effort-choices cadre-ui::*chat*)))
  (check "unsupported thinking levels display Default"
         (zerop (gtk:drop-down-get-selected (cadre-ui::chat-effort-dropdown cadre-ui::*chat*))))
  (chat-type "effort probe fallback"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*))
                 (chat-says "Codex effort=low model=fake-gpt-low")))
  (check "an unsupported effort sends the new model's default"
         (chat-says "Codex effort=low model=fake-gpt-low thread=fake-codex-thread"))
  (gtk:drop-down-set-selected (cadre-ui::chat-model-dropdown cadre-ui::*chat*) 0)
  (choose-thinking nil)
  (chat-type "effort probe default"))

(then-when ((and (not (cadre-ui::chat-busy cadre-ui::*chat*))
                 (chat-says "Codex effort=medium model=fake-gpt-default")))
  (check "Codex Default sends the advertised default in the same thread"
         (and (eq *effort-codex-server* (cadre-ui::chat-codex-process cadre-ui::*chat*))
              (chat-says "Codex effort=medium model=fake-gpt-default thread=fake-codex-thread")))
  (setf *effort-layout* (cadre-ui::window-layout *window*))
  (cadre-ui::remember-panel-size *window*)
  (setf *effort-panel-size* (copy-list (cadre-ui::window-panel-sizes *window*)))
  (cadre-ui::apply-layout *window* :vertical)
  (cadre-ui::panel-show (cadre-ui::window-panel *window*) "claude")
  (gtk:paned-set-position (cadre-ui::window-main-paned *window*)
                          (- (cadre-ui::paned-extent *window* :vertical) 360)))

(then 600
  (let* ((chat cadre-ui::*chat*)
         (panel (cadre-ui::panel-widget (cadre-ui::window-panel *window*)))
         (controls (list (cadre-ui::chat-provider-dropdown chat) (cadre-ui::chat-mode-dropdown chat)
                         (cadre-ui::chat-model-dropdown chat) (cadre-ui::chat-effort-dropdown chat)))
         (rows '()))
    (dolist (control controls)
      (multiple-value-bind (ok x y) (gtk:widget-translate-coordinates control panel 0d0 0d0)
        (check "a selector stays visible and inside the narrow panel"
               (and ok (gtk:widget-get-visible control) (gtk:widget-get-sensitive control)
                    (plusp (gtk:widget-get-width control)) (>= x 0)
                    (<= (+ x (gtk:widget-get-width control)) (gtk:widget-get-width panel)))
               (list x y (gtk:widget-get-width control) (gtk:widget-get-width panel)))
        (pushnew y rows :test #'=)))
    (setf *effort-narrow-rows* (length rows))
    (check "assistant selectors wrap into multiple rows" (> (length rows) 1) rows)
    (multiple-value-bind (ok x y) (gtk:widget-translate-coordinates (cadre-ui::chat-status chat) panel 0d0 0d0)
      (declare (ignore x))
      (check "assistant status has readable space below the selectors"
             (and ok (> y (apply #'max rows)) (> (gtk:widget-get-width (cadre-ui::chat-status chat)) 180))))
    (screenshot "18b-assistant-narrow"))
  (cadre-ui::apply-layout *window* :horizontal))

(then 600
  (let* ((chat cadre-ui::*chat*)
         (panel (cadre-ui::panel-widget (cadre-ui::window-panel *window*)))
         (rows '()))
    (dolist (control (list (cadre-ui::chat-provider-dropdown chat) (cadre-ui::chat-mode-dropdown chat)
                           (cadre-ui::chat-model-dropdown chat) (cadre-ui::chat-effort-dropdown chat)))
      (multiple-value-bind (ok x y) (gtk:widget-translate-coordinates control panel 0d0 0d0)
        (check "a selector stays inside the wide panel"
               (and ok (>= x 0) (plusp (gtk:widget-get-width control))
                    (<= (+ x (gtk:widget-get-width control)) (gtk:widget-get-width panel))))
        (pushnew y rows :test #'=)))
    (check "widening the panel compacts the selectors into fewer rows"
           (< (length rows) *effort-narrow-rows*) rows))
  (setf (cadre-ui::window-panel-sizes *window*) (copy-list *effort-panel-size*))
  (cadre-ui::apply-layout *window* *effort-layout*)
  ;; apply-layout remembers the temporary narrow size; restore both entries.
  (setf (cadre-ui::window-panel-sizes *window*) (copy-list *effort-panel-size*))
  (cadre-ui::restore-panel-size *window*)
  (gtk:drop-down-set-selected (cadre-ui::chat-provider-dropdown cadre-ui::*chat*) 0)
  (setf *claude-effort* *effort-claude-original*
        cadre-ui::*codex-effort* *effort-codex-original*)
  (cadre-ui::refresh-assistant-effort-dropdown))

;;; M5: Emacs depth
(section "emacs")
(defun type-keys (keys)
  "Type KEYS (\"C-u 3 x\") as if pressed, letting Cadre type the keys GTK would."
  (dolist (key (parse-keys keys))
    (cadre-ui::process-key *window* key
                           (cond ((string= key "SPC") " ")
                                 ((= 1 (length key)) key)
                                 ((and (= 3 (length key)) (string= "S-" key :end2 2)) (string-upcase (subseq key 2))))
                           :replaying t)))

(defun m5-text (string &optional (line 0) (column 0))
  (let ((buffer (find-buffer "m5.lisp")))
    (show-buffer-named "m5.lisp")
    (text-replace-contents (buffer-text buffer) string)
    (set-cursor line column)))

(defun buffer-text-string () (buffer-string (current-buffer)))
(defun point () (text-point (buffer-text (current-buffer))))

(then 300
  (open-file-path (merge-pathnames "src/m5.lisp" *root*)))

(then-when ((find-buffer "m5.lisp"))
  (setf *keybinding-profile* :emacs)
  (m5-text "(a b) c" 0 4)
  (type-keys "C-)")
  (check "C-) slurps the next expression" (string= "(a b c)" (line-text 0)) (line-text 0))
  (type-keys "C-}")
  (check "C-} barfs it again" (string= "(a b) c" (line-text 0)) (line-text 0))
  (m5-text "(f (g x))" 0 6)
  (type-keys "M-r")
  (check "M-r raises the expression at the cursor" (string= "(f x)" (line-text 0)) (line-text 0))
  (m5-text "(a (b c) d)" 0 5)
  (type-keys "M-s")
  (check "M-s splices the list" (string= "(a b c d)" (line-text 0)) (line-text 0))
  (m5-text "(f x)" 0 3)
  (type-keys "M-(")
  (check "M-( wraps the next expression" (string= "(f (x))" (line-text 0)) (line-text 0))
  ;; Paredit mode
  (call-command 'cadre-ui::paredit-mode)
  (check "paredit mode turns on in Lisp buffers" (minor-mode-enabled-p (find-buffer "m5.lisp") 'cadre-ui::paredit-mode))
  (m5-text "" 0 0)
  (type-keys "( d e f SPC (")
  (check "( inserts a pair in paredit mode" (string= "(def ())" (buffer-text-string)) (buffer-text-string))
  (type-keys ") )")
  (check ") moves past the close paren" (= (point) 8) (point))
  (type-keys "DEL")
  (check "DEL steps into a list instead of unbalancing it" (and (string= "(def ())" (buffer-text-string)) (= (point) 7)))
  (m5-text "(a ())" 0 4)
  (type-keys "DEL")
  (check "DEL deletes an empty pair" (string= "(a )" (buffer-text-string)) (buffer-text-string))
  (m5-text "(a (b
 c) d)" 0 3)
  (type-keys "C-k")
  (check "C-k kills whole expressions in paredit mode" (string= "(a  d)" (buffer-text-string)) (buffer-text-string))
  (call-command 'cadre-ui::paredit-mode)
  (check "paredit mode turns off" (not (minor-mode-enabled-p (find-buffer "m5.lisp") 'cadre-ui::paredit-mode)))
  ;; The kill ring
  (m5-text "one two three" 0 0)
  (kill-new "older")
  (setf *last-command-kind* nil)
  (type-keys "M-d M-d")
  (check "kills in a row join" (string= "one two" (first *kill-ring*)) (first *kill-ring*))
  (type-keys "C-e C-y")
  (check "C-y yanks the last kill" (string= " threeone two" (buffer-text-string)) (buffer-text-string))
  (type-keys "M-y")
  (check "M-y replaces it with the kill before" (string= " threeolder" (buffer-text-string)) (buffer-text-string))
  (type-keys "C-a C-k")
  (check "C-k kills to the end of the line" (and (string= "" (buffer-text-string))
                                                 (string= " threeolder" (first *kill-ring*))))
  ;; Prefix arguments
  (m5-text "abcdefgh" 0 0)
  (type-keys "C-u 3 C-f")
  (check "C-u 3 C-f moves three characters" (= 3 (point)) (point))
  (type-keys "C-u 4 x")
  (check "C-u 4 x types four x's" (string= "abcxxxxdefgh" (buffer-text-string)) (buffer-text-string))
  (type-keys "C-u C-u C-b")
  (check "C-u C-u means 16" (= 0 (point)) (point))
  ;; Case, comments, transposing
  (m5-text "hello world" 0 0)
  (type-keys "M-u M-c")
  (check "M-u and M-c change a word's case" (string= "HELLO World" (buffer-text-string)) (buffer-text-string))
  (m5-text "(foo)" 0 0)
  (type-keys "M-;")
  (check "M-; adds a comment at the end of the line" (search "(foo)" (line-text 0)) (line-text 0))
  (check "it starts with ; " (search "; " (line-text 0)))
  (m5-text "(def-thing 1)
(def" 1 4)
  (type-keys "M-/")
  (check "M-/ expands a word from the buffer" (string= "(def-thing" (line-text 1)) (line-text 1))
  ;; Incremental search
  (m5-text "alpha beta alpha gamma" 0 0)
  (type-keys "C-s")
  (check "C-s opens incremental search" (cadre-ui::isearch-active-p))
  (gtk:editable-set-text (cadre-ui::find-bar-entry (cadre-ui::window-find-bar *window*)) "alpha"))

(then 300
  (check "the cursor follows the first match" (= 5 (point)) (point))
  (type-keys "C-s")
  (check "C-s goes to the next match" (= 16 (point)) (point))
  (type-keys "C-g")
  (check "C-g goes back to where the search began" (and (= 0 (point)) (not (cadre-ui::isearch-active-p))))
  (type-keys "C-s")
  (gtk:editable-set-text (cadre-ui::find-bar-entry (cadre-ui::window-find-bar *window*)) "gam"))

(then 300
  (check "a new search starts from the cursor" (= 20 (point)) (point))
  (type-keys "C-f")
  (check "another command ends the search at the match and runs" (and (not (cadre-ui::isearch-active-p)) (= 21 (point)))
         (point))
  (check "the mark is left where the search began" (= 0 (cadre-ui::mark-offset (current-buffer))))
  ;; Query-replace
  (m5-text "foo Foo foo foo" 0 0)
  (cadre-ui::start-query-replace (current-view) "foo" "bar")
  (type-keys "y n y")
  (check "query-replace asks about each match" (string= "bar Foo bar foo" (buffer-text-string)) (buffer-text-string))
  (type-keys "!")
  (check "! replaces the rest" (and (string= "bar Foo bar bar" (buffer-text-string)) (null cadre-ui::*query-replace*))
         (buffer-text-string))
  (m5-text "Foo foo" 0 0)
  (cadre-ui::start-query-replace (current-view) "foo" "bar")
  (type-keys "!")
  (check "replacing keeps the case of each match" (string= "Bar bar" (buffer-text-string)) (buffer-text-string))
  ;; Keyboard macros
  (m5-text "a
b
c
d" 0 0)
  (type-keys "C-x ( C-a - SPC C-n C-x )")
  (check "a keyboard macro records keys" (and (string= "- a" (line-text 0)) cadre-ui::*last-macro*
                                              (not cadre-ui::*macro-recording-p*)))
  (type-keys "C-x e e")
  (check "C-x e runs it, and e again" (and (string= "- b" (line-text 1)) (string= "- c" (line-text 2)))
         (list (line-text 1) (line-text 2)))
  (type-keys "C-g C-u 1 C-x e")
  (check "with a count" (string= "- d" (line-text 3)) (line-text 3))
  ;; Help
  (type-keys "C-h k C-x C-f")
  (check "C-h k describes a key" (and (find-buffer "*Help*") (search "open-file" (buffer-string (find-buffer "*Help*")))))
  (call-command 'close-tab)
  ;; Splits
  (show-buffer-named "m5.lisp")
  (type-keys "C-x 3")
  (check "C-x 3 splits the editor" (= 2 (length (cadre-ui::window-groups *window*))))
  (check "both groups show the file"
         (= 2 (length (cadre-ui::buffer-views *window* (find-buffer "m5.lisp"))))))

(then 300
  (let* ((group (cadre-ui::window-active-group *window*))
         (strip (cadre-ui::group-strip group))
         (tabs (cadre-ui::strip-tabs strip)))
    (check "each tab in the strip is one of the group's pages"
           (equal (mapcar #'car tabs) (cadre-ui::group-pages group)))
    (check "tabs are compact: as wide as their names, one line tall"
           (every (lambda (tab) (and (< (gtk:widget-get-width (cdr tab)) 160) (< (gtk:widget-get-height (cdr tab)) 32)))
                  tabs)
           (mapcar (lambda (tab) (list (gtk:widget-get-width (cdr tab)) (gtk:widget-get-height (cdr tab)))) tabs))
    (check "the selected tab is marked"
           (gtk:widget-has-css-class (cdr (assoc (adw:tab-view-get-selected-page (cadre-ui::group-tab-view group)) tabs))
                                     "selected")))
  (screenshot "19-split")
  (let ((before (cadre-ui::window-active-group *window*)))
    (type-keys "C-x o")
    (check "C-x o moves to the other group" (not (eq before (cadre-ui::window-active-group *window*)))))
  (type-keys "C-x 2")
  (check "C-x 2 splits again, below" (= 3 (length (cadre-ui::window-groups *window*))))
  (cadre-ui::save-session *window*)
  (let ((state (cdr (assoc (uiop:native-namestring (cadre-ui::window-project *window*)) (cadre-ui::read-sessions)
                           :test #'equal))))
    (check "the session remembers the split groups"
           (eq :split (first (getf state :layout))) (getf state :layout))
    (check "the session remembers the files"
           (search "m5.lisp" (prin1-to-string (getf state :layout))))))

(then 300
  (type-keys "C-x 1"))

(then 300
  (check "C-x 1 leaves one group" (= 1 (length (cadre-ui::window-groups *window*))))
  (check "files open in other groups move into it" (find-buffer "m5.lisp"))
  (check "and no buffer is shown twice in it"
         (= 1 (length (cadre-ui::buffer-views *window* (find-buffer "m5.lisp")))))
  (cadre-ui::restore-layout *window* (cadre-ui::window-active-group *window*)
                            (list :split :below 0.5
                                  (list :group :files '())
                                  (list :group :files (list (list (namestring (merge-pathnames "src/m1.lisp" *root*)) 1 2))))))

(then 300
  (check "restoring a session rebuilds its split groups" (= 2 (length (cadre-ui::window-groups *window*))))
  (let ((second (second (cadre-ui::groups-in-order *window*))))
    (check "and reopens the files in them, with the cursor where it was"
           (let ((view (cadre-ui::group-selected-view *window* second)))
             (and view (string= "m1.lisp" (buffer-name (view-buffer view)))
                  (equal '(2 3) (multiple-value-list (cadre-ui::view-cursor-line-column view))))))))

(then 300
  (call-command 'cadre-ui::delete-other-groups))

(then 300
  (check "groups merge back" (= 1 (length (cadre-ui::window-groups *window*))))
  ;; The editor REPL
  (call-command 'cadre-ui::editor-repl)
  (let ((buffer (find-buffer "*cadre-repl*")))
    (check "the editor REPL opens in a tab" (and buffer (eq buffer (current-buffer))))
    (let ((view (first (cadre-ui::buffer-views *window* buffer))))
      (check "its tab is in the window's only group, selected"
             (and view (member (cadre-ui::view-group view) (cadre-ui::window-groups *window*))
                  (eq view (cadre-ui::selected-view *window*)))
             (list (length (cadre-ui::window-groups *window*))
                   (and view (member (cadre-ui::view-group view) (cadre-ui::window-groups *window*)) t)
                   (tab-titles))))
    (insert-at-cursor "(+ 1 2)")
    (type-keys "RET")
    (check "it evaluates in Cadre's image" (search (format nil "~%3~%") (buffer-string buffer)) (buffer-string buffer))
    (insert-at-cursor "(length (cadre:buffer-list))")
    (type-keys "RET")
    (check "with the editor's own functions"
           (search (format nil "~%~d~%" (length (buffer-list))) (buffer-string buffer)))
    (insert-at-cursor "(error \"boom\")")
    (type-keys "RET")
    (check "errors are reported, not fatal" (search "; Error: boom" (buffer-string buffer)))))

(then 300
  (screenshot "20-editor-repl")
  (check "no command shadows a core function"
         (null (loop for s being the external-symbols of :cadre
                     when (and (find-command s) (not (eq s 'cadre:lisp-mode))) collect s)))
  (let ((replaced (loop for command in (list-commands)
                        for name = (command-name command)
                        unless (equal (documentation name 'function) (command-documentation command))
                          collect name)))
    (check "no function replaces a command of the same name" (null replaced) replaced))
  (call-command 'close-tab)
  (setf *keybinding-profile* :standard))

;;; M6: themes, settings, files changed on disk
(section "settings")
(open-files "src/m1.lisp" "src/m5.lisp")
(then 50
  (unless (buffer-modified-p (find-buffer "m5.lisp"))
    (buffer-insert (find-buffer "m5.lisp") (format nil ";; mine~%") 0)))
(defun tag-color (buffer face)
  (let ((tag (cadre-ui::face-tag (buffer-text buffer) face)))
    (and (gobject:property tag :foreground-set)
         (gdk:rgba-to-string (gobject:property tag :foreground-rgba)))))

(then 300
  (setf cadre-ui::*light-theme* "solarized-light" cadre-ui::*color-scheme* :light)
  (cadre-ui::apply-theme))

(then 300
  (check "a theme colors the syntax" (equal "rgb(147,161,161)" (tag-color (find-buffer "m1.lisp") :comment))
         (tag-color (find-buffer "m1.lisp") :comment))
  (check "a theme with a background colors the editor" cadre-ui::*theme-provider*)
  (screenshot "22-solarized")
  (setf cadre-ui::*dark-theme* "high-contrast-dark" cadre-ui::*color-scheme* :dark)
  (cadre-ui::apply-theme))

(then 300
  (check "faces a theme leaves out come from the theme it inherits"
         (equal "rgb(127,132,142)" (tag-color (find-buffer "m1.lisp") :reader-conditional))
         (tag-color (find-buffer "m1.lisp") :reader-conditional))
  (setf cadre-ui::*light-theme* "cadre-light" cadre-ui::*dark-theme* "cadre-dark" cadre-ui::*color-scheme* :system)
  (cadre-ui::apply-theme)
  (call-command 'cadre-ui::settings))

(defun settings-row (title)
  (find-if (lambda (w) (and (typep w 'adw:preferences-row) (equal title (adw:preferences-row-get-title w))))
           (find-widgets cadre-ui::*settings-dialog* (lambda (w) (typep w 'adw:preferences-row)))))

(then 500
  (check "the settings page lists the options" (and (settings-row "Paredit") (settings-row "Editor font")))
  (screenshot "23-settings")
  (adw:switch-row-set-active (settings-row "Paredit") t)
  (check "a switch sets its option" cadre-ui::*paredit*)
  (check "and saves it" (cadre-ui::saved-option-value 'cadre-ui::*paredit*))
  (check "and applies it" (minor-mode-enabled-p (find-buffer "m1.lisp") 'cadre-ui::paredit-mode))
  (adw:switch-row-set-active (settings-row "Paredit") nil)
  (let ((row (settings-row "Kill ring max")))
    (adw:spin-row-set-value row 50d0)
    (check "a number field sets its option" (= 50 *kill-ring-max*)))
  (let ((row (settings-row "Explorer hidden types (Lisp)")))
    (gtk:editable-set-text row "(\"fasl\" \"tmp\")")
    (gobject:emit row :apply)
    (check "a Lisp field reads its value" (equal '("fasl" "tmp") cadre-ui::*explorer-hidden-types*))
    (gtk:editable-set-text row "(oops")
    (gobject:emit row :apply)
    (check "a bad value is refused" (and (equal '("fasl" "tmp") cadre-ui::*explorer-hidden-types*)
                                         (gtk:widget-has-css-class row "error"))))
  (setf cadre-ui::*explorer-hidden-types* '("fasl" "dx64fsl" "ufasl" "fas" "lx64fsl") *kill-ring-max* 120)
  (adw:dialog-close cadre-ui::*settings-dialog*)
  ;; Files changed on disk
  (with-open-file (o (merge-pathnames "src/m1.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "(defun area (w h)~%  (* w h 1))~%")))

(then-when ((search "(* w h 1)" (buffer-string (find-buffer "m1.lisp"))) :timeout 10)
  (check "an unmodified buffer reloads when its file changes" (search "(* w h 1)" (buffer-string (find-buffer "m1.lisp"))))
  (check "and stays unmodified" (not (buffer-modified-p (find-buffer "m1.lisp"))))
  (check "m5.lisp has unsaved changes" (buffer-modified-p (find-buffer "m5.lisp")))
  (with-open-file (o (merge-pathnames "src/m5.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "changed elsewhere~%")))

(then-when ((gtk:revealer-get-reveal-child cadre-ui::*conflict-bar*) :timeout 10)
  (check "a modified buffer whose file changes shows the conflict bar" (gtk:revealer-get-reveal-child cadre-ui::*conflict-bar*))
  (screenshot "24-conflict")
  (cadre-ui::resolve-conflict :keep)
  (check "keeping mine leaves the buffer alone" (not (search "changed elsewhere" (buffer-string (find-buffer "m5.lisp")))))
  (check "and hides the bar" (not (gtk:revealer-get-reveal-child cadre-ui::*conflict-bar*))))

;;; Editing tools: find options, wrapping, project search, rename, extract
(section "editing-tools")
(open-files "src/m5.lisp" "src/m1.lisp")
(defun find-bar () (cadre-ui::window-find-bar *window*))
(defun match-count () (length (cadre-ui::find-bar-matches (find-bar))))

(then 300
  (m5-text "foo foo-bar Foo fob" 0 0)
  (call-command 'find-text)
  (gtk:editable-set-text (cadre-ui::find-bar-entry (find-bar)) "foo"))

(then 300
  (check "find matches ignoring case" (= 3 (match-count)) (match-count))
  (gtk:toggle-button-set-active (cadre-ui::find-bar-word-button (find-bar)) t)
  (check "whole words leave out foo-bar" (= 2 (match-count)) (match-count))
  (gtk:toggle-button-set-active (cadre-ui::find-bar-case-button (find-bar)) t)
  (check "match case leaves out Foo" (= 1 (match-count)) (match-count))
  (gtk:toggle-button-set-active (cadre-ui::find-bar-word-button (find-bar)) nil)
  (gtk:toggle-button-set-active (cadre-ui::find-bar-case-button (find-bar)) nil)
  (gtk:toggle-button-set-active (cadre-ui::find-bar-regex-button (find-bar)) t)
  (gtk:editable-set-text (cadre-ui::find-bar-entry (find-bar)) "fo(.)"))

(then 300
  (check "a regular expression finds its matches" (= 4 (match-count)) (match-count))
  (gtk:editable-set-text (cadre-ui::find-bar-replace-entry (find-bar)) "[\\1]")
  (cadre-ui::find-replace-all (find-bar))
  (check "a regex replacement fills in groups" (string= "[o] [o]-bar [o] [b]" (buffer-text-string)) (buffer-text-string))
  (gtk:editable-set-text (cadre-ui::find-bar-entry (find-bar)) "(oops"))

(then 300
  (check "a bad regex says so" (string= "Bad regex" (gtk:label-get-text (cadre-ui::find-bar-status (find-bar)))))
  (gtk:toggle-button-set-active (cadre-ui::find-bar-regex-button (find-bar)) nil)
  (cadre-ui::find-close (find-bar))
  ;; Wrapping the selection
  (m5-text "abc def" 0 0)
  (let ((gtk-buffer (buffer-text (current-buffer))))
    (gtk:text-buffer-select-range gtk-buffer (cadre-ui::iter-at gtk-buffer 0) (cadre-ui::iter-at gtk-buffer 3)))
  (type-keys "(")
  (check "typing ( with a selection wraps it" (string= "(abc) def" (buffer-text-string)) (buffer-text-string))
  (type-keys "\"")
  (check "and keeps it selected, so \" wraps it again" (string= "(\"abc\") def" (buffer-text-string)) (buffer-text-string))
  (type-keys "[")
  (check "[ wraps too" (string= "(\"[abc]\") def" (buffer-text-string)) (buffer-text-string))
  ;; The right-click menu
  (let ((labels (mapcar #'car (menu-entries (cadre-ui::editor-context-menu (current-buffer)))))
        (popover (cadre-ui::show-context-menu (current-view) 40 40)))
    (check "Lisp editors have a right-click menu with Go to Definition and Copy"
           (and (member "Go to Definition" labels :test #'string=) (member "Copy" labels :test #'string=)) labels)
    ;; Short enough to fit above or below a click on a laptop's screen: a
    ;; menu macOS has to move takes its clicks where it was meant to be.
    (let ((height (nth-value 1 (gtk:widget-measure popover :vertical -1))))
      (check "and it is short enough to fit on the screen" (< height 420) height)
      ;; A submenu must not shrink it: macOS would leave it drawn where it
      ;; was while GTK takes its clicks where it moved to.
      (let ((stack (cadre-ui::find-descendant popover (lambda (w) (typep w 'gtk:stack)))))
        (gtk:stack-set-visible-child stack (gtk:widget-get-last-child stack))
        (check "a submenu keeps the menu's height"
               (= height (nth-value 1 (gtk:widget-measure popover :vertical -1)))
               (list height (nth-value 1 (gtk:widget-measure popover :vertical -1))))))
    (gtk:popover-popdown popover))
  ;; Find in the project
  (show-buffer-named "m1.lisp")
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil)
  (set-cursor 0 8)
  (call-command 'cadre-ui::find-in-project))

(defun search-results () (cadre-ui::ps-results-data cadre-ui::*project-search*))
(defun result-files () (mapcar (lambda (r) (file-namestring (first r))) (search-results)))

(then-when ((search "result" (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*))))
  (check "Find in Project searches for the symbol at the cursor"
         (string= "area" (gtk:editable-get-text (cadre-ui::ps-entry cadre-ui::*project-search*))))
  (check "and lists the files that contain it" (and (member "m1.lisp" (result-files) :test #'string=)
                                                    (member "m6.lisp" (result-files) :test #'string=))
         (result-files))
  (check "the sidebar shows the Search page" (equal "search" (cadre-ui::sidebar-page *window*)))
  (screenshot "25-project-search")
  (show-buffer-named "m1.lisp")
  (set-cursor 0 8)
  (call-command 'cadre-ui::rename-symbol))

(then-when ((and (eq :symbol (cadre-ui::ps-mode cadre-ui::*project-search*))
                 (search "place" (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*)))))
  (let ((m6 (find "m6.lisp" (search-results) :key (lambda (r) (file-namestring (first r))) :test #'string=)))
    (check "renaming finds the symbol, not the word in strings and comments"
           (and m6 (= 1 (length (second m6)))) (and m6 (length (second m6)))))
  (gtk:editable-set-text (cadre-ui::ps-replace-entry cadre-ui::*project-search*) "surface")
  (cadre-ui::apply-project-replace)
  (check "rename changes open buffers" (search "(defun surface" (buffer-string (find-buffer "m1.lisp"))))
  (check "and files that aren't open, on disk"
         (search "(surface 2 3)" (uiop:read-file-string (merge-pathnames "src/m6.lisp" *root*))))
  (check "leaving strings and comments alone"
         (search "area in a string" (uiop:read-file-string (merge-pathnames "src/m6.lisp" *root*))))
  (gtk:text-buffer-undo (buffer-text (find-buffer "m1.lisp")))
  (check "one undo takes back the rename in an open buffer" (search "(defun area" (buffer-string (find-buffer "m1.lisp"))))
  ;; Extract variable and function
  (show-buffer-named "m1.lisp")
  (text-replace-contents (buffer-text (find-buffer "m1.lisp")) "(defun area (w h)
  (let ((pad 2))
    (* (+ w pad) h)))")
  (let* ((gtk-buffer (buffer-text (find-buffer "m1.lisp")))
         (start (search "(+ w pad)" (buffer-string (find-buffer "m1.lisp")))))
    (gtk:text-buffer-select-range gtk-buffer (cadre-ui::iter-at gtk-buffer start) (cadre-ui::iter-at gtk-buffer (+ start 9))))
  (call-command 'cadre-ui::extract-function)
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "padded")
  (cadre-ui::choose (picker)))

(then 300
  (check "Extract Function defines the function with the variables it uses"
         (search "(defun padded (w pad)" (buffer-string (find-buffer "m1.lisp")))
         (buffer-string (find-buffer "m1.lisp")))
  (check "and calls it in place" (search "(* (padded w pad) h)" (buffer-string (find-buffer "m1.lisp"))))
  (let* ((gtk-buffer (buffer-text (find-buffer "m1.lisp")))
         (start (search "(padded w pad)" (buffer-string (find-buffer "m1.lisp")))))
    (gtk:text-buffer-select-range gtk-buffer (cadre-ui::iter-at gtk-buffer start) (cadre-ui::iter-at gtk-buffer (+ start 14))))
  (call-command 'cadre-ui::extract-variable)
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "width")
  (cadre-ui::choose (picker)))

(then 300
  (check "Extract Variable binds it with let around the form"
         (and (search "(let ((width (padded w pad)))" (buffer-string (find-buffer "m1.lisp")))
              (search "(* width h)" (buffer-string (find-buffer "m1.lisp"))))
         (buffer-string (find-buffer "m1.lisp")))
  (screenshot "26-refactor")
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil))

;;; Completion as you type, and hints from the source

;;; Rename: a local binding in its form; a symbol by its package across files.
(section "rename")

(then 50
  (write-root-file "src/rn1.lisp" "(defpackage #:rn-a (:use #:cl) (:export #:total))
(in-package #:rn-a)
(defun total (items)
  (let ((sum 0))
    (dolist (i items sum) (incf sum i))))
(defun twice (sum) (* 2 sum))
")
  (write-root-file "src/rn2.lisp" "(defpackage #:rn-b (:use #:cl))
(in-package #:rn-b)
(defun total (x) x)
(defun use () (rn-a:total '(1 2)))
"))

(open-files "src/rn1.lisp" "src/rn2.lisp")

(defun rename-rows ()
  "(file line kind checked) of each Search page row."
  (loop for (path rows) in (search-results)
        append (loop for (line nil nil check kind) in rows
                     collect (list (file-namestring path) line kind (gtk:check-button-get-active check)))))

(then 300
  (show-buffer-named "rn1.lisp")
  (set-cursor 3 9)                      ; sum, in (let ((sum 0))
  (call-command 'cadre-ui::rename-symbol))

(then 300
  (check "renaming a local variable previews its places in its form"
         (and (eq :lexical (cadre-ui::ps-mode cadre-ui::*project-search*))
              (equal '(("rn1.lisp" 3 :binding t) ("rn1.lisp" 4 :binding t) ("rn1.lisp" 4 :binding t))
                     (rename-rows)))
         (rename-rows))
  (gtk:editable-set-text (cadre-ui::ps-replace-entry cadre-ui::*project-search*) "acc")
  (cadre-ui::apply-project-replace)
  (check "and renames only that binding"
         (and (search "(let ((acc 0))" (buffer-string (find-buffer "rn1.lisp")))
              (search "(dolist (i items acc) (incf acc i))" (buffer-string (find-buffer "rn1.lisp")))
              (search "(defun twice (sum) (* 2 sum))" (buffer-string (find-buffer "rn1.lisp"))))
         (buffer-string (find-buffer "rn1.lisp")))
  (set-cursor 2 8)                      ; total, in (defun total
  (call-command 'cadre-ui::rename-symbol))

(then-when ((and (eq :symbol (cadre-ui::ps-mode cadre-ui::*project-search*))
                 (search "place" (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*))))
            :timeout 20)
  (let ((rows (rename-rows)))
    (check "renaming a symbol checks its own places: the definition, the export and pkg:name"
           (and (member '("rn1.lisp" 0 :export t) rows :test #'equal)
                (member '("rn1.lisp" 2 :same t) rows :test #'equal)
                (member '("rn2.lisp" 3 :same t) rows :test #'equal))
           rows)
    (check "and leaves another package's symbol of that name unchecked"
           (let ((other (find-if (lambda (r) (and (equal (first r) "rn2.lisp") (= (second r) 2))) rows)))
             (and other (member (third other) '(:unknown :other)) (not (fourth other))))
           rows))
  (when (cadre-ui::connected-p)
    (check "with a Lisp connected, the packages come from it"
           (search "from the running Lisp" (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*)))
           (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*))))
  (screenshot "rename-preview")
  (gtk:editable-set-text (cadre-ui::ps-replace-entry cadre-ui::*project-search*) "sum-of")
  (cadre-ui::apply-project-replace)
  (let ((rn1 (buffer-string (find-buffer "rn1.lisp"))) (rn2 (buffer-string (find-buffer "rn2.lisp"))))
    (check "the rename changes the symbol's places only"
           (and (search "(:export #:sum-of)" rn1) (search "(defun sum-of (items)" rn1)
                (search "(rn-a:sum-of '(1 2))" rn2) (search "(defun total (x) x)" rn2))
           (list rn1 rn2)))
  (dolist (name '("rn1.lisp" "rn2.lisp")) (setf (buffer-modified-p (find-buffer name)) nil)))

(section "completion")
(open-files "src/m1.lisp")
(defun arglist-markup () (gtk:label-get-label (cadre-ui::window-status-arglist *window*)))

(then 100
  (show-buffer-named "m1.lisp")
  (cadre-ui::focus-view (current-view))
  (setf cadre-ui::*project-definitions-time* 0)
  (cadre-ui::refresh-project-definitions))

(then-when ((gethash "scale-shape" cadre-ui::*project-definitions*))
  (check "the project's definitions are read from files that aren't open"
         (gethash "scale-shape" cadre-ui::*project-definitions*))
  (let ((buffer (find-buffer "m1.lisp")))
    (text-replace-contents (buffer-text buffer) (format nil "(defun area (w h) (* w h))~%"))
    (set-cursor 1 0)
    (insert-at-cursor "(scale-s")
    ;; As if typed: the key goes in, and the change starts completion.
    (setf cadre-ui::*typed-key* (cons buffer "h"))
    (insert-at-cursor "h")))

(then-when ((cadre-ui::completion-open-p) :timeout 5)
  (check "typing a symbol opens completions by itself" (cadre-ui::completion-open-p))
  (check "offering a function defined in another file"
         (string= "scale-shape" (first (cadre-ui::selected-completion)))
         (cadre-ui::selected-completion))
  (check "showing its parameters below the list"
         (search "shape factor &amp;key (round t)"
                 (gtk:label-get-label (cadre-ui::cp-detail cadre-ui::*completion*)))
         (gtk:label-get-label (cadre-ui::cp-detail cadre-ui::*completion*)))
  (screenshot "27-auto-complete" (cadre-ui::cp-popover cadre-ui::*completion*))
  (press "TAB")
  (check "Tab inserts the completion" (string= "(scale-shape" (line-text 1)) (line-text 1))
  (check "and closes the popup" (not (cadre-ui::completion-open-p)))
  (let ((items '(("gtk:window") ("gtk:widget") ("gdk:window") ("gtk:window-new") ("getf"))))
    (check "a package prefix keeps only that package's names"
           (null (set-exclusive-or '("gtk:window" "gtk:window-new" "gtk:widget")
                                   (mapcar #'first (cadre-ui::rank-completions "gtk:wi" items))
                                   :test #'string=))
           (mapcar #'first (cadre-ui::rank-completions "gtk:wi" items)))
    (check "and leaves out what's already typed"
           (equal '("gtk:window-new") (mapcar #'first (cadre-ui::rank-completions "gtk:window" items))))
    (check "gtk: and gtk:: are the same package"
           (cadre-ui::same-qualifier-p (cadre-ui::prefix-qualifier "gtk::wi") (cadre-ui::prefix-qualifier "gtk:wi"))))
  (insert-at-cursor " 'square ")
  (cadre-ui::request-autodoc (current-view)))

(then-when ((search "factor" (arglist-markup)) :timeout 5)
  (check "without a connection, the status bar shows the source's arglist with the argument marked"
         (search "<b>factor</b>" (arglist-markup)) (arglist-markup))
  (let ((description (cadre-ui::symbol-description "scale-shape" (current-buffer))))
    (check "hovering a function you wrote describes its parameters and documentation"
           (and (string= "(scale-shape shape factor &key (round t))" (first description))
                (search "Scale SHAPE by FACTOR." (cadre-ui::description-markup description)))
           description))
  (check "and standard functions too"
         (equal "(mapcar function list &rest more-lists)"
                (first (cadre-ui::symbol-description "mapcar" (current-buffer)))))
  (check "the text view asks for symbol tooltips"
         (gtk:widget-get-has-tooltip (view-text-view (current-view))))
  (insert-at-cursor "; scale")
  (check "completions don't appear inside comments" (not (cadre-ui::auto-complete-p (current-view))))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil)
  ;; In the Emacs profile on macOS, ⌘ keys Emacs leaves alone act as in Standard.
  (let ((cadre-ui::*keybinding-profile* :emacs))
    (check "Emacs profile: ⌘↩ still evaluates, ⌘S still saves"
           (or (not (cadre-ui::macos-p))
               (and (eq 'compile-or-eval-defun (cadre-ui::mac-command-fallback *window* "s-RET"))
                    (eq 'save-buffer (cadre-ui::mac-command-fallback *window* "s-s"))))))
  (check "the Standard profile needs no fallback" (null (cadre-ui::mac-command-fallback *window* "s-RET")))
  ;; Closing doesn't ask about Cadre's own buffers.
  (call-command 'cadre-ui::editor-repl)
  (insert-at-cursor "(+ 1 2)")
  (check "a REPL with typed input doesn't need saving"
         (and (buffer-modified-p (find-buffer "*cadre-repl*"))
              (not (cadre-ui::buffer-needs-saving-p (find-buffer "*cadre-repl*")))
              (not (cadre-ui::buffer-needs-saving-p (find-buffer "*repl*")))))
  (let ((asked (remove-if-not (lambda (b) (and (null (buffer-file b)) (cadre-ui::buffer-needs-saving-p b)))
                              (buffer-list))))
    (check "so closing the window asks only about files and untitled buffers" (null asked)
           (mapcar #'buffer-name asked))))

;;; Markdown
(section "markdown")
(defun preview-buffer () (find-buffer "Preview guide.md"))
(defun preview-text () (buffer-string (preview-buffer)))
(defun tab-markdown-switch (view)
  (let* ((page (cadre-ui::view-page *window* view))
         (strip (cadre-ui::group-strip (cadre-ui::view-group view)))
         (tab (cdr (assoc page (cadre-ui::strip-tabs strip)))))
    (find-if (lambda (widget) (and (typep widget 'gtk:button)
                                   (member (gtk:widget-get-tooltip-text widget)
                                           '("Preview Markdown" "Show Markdown source")
                                           :test #'equal)))
             (find-widgets tab (lambda (widget) (typep widget 'gtk:button))))))

(then 100
  (open-file-path (merge-pathnames "guide.md" *root*)))

(then 800
  (check "a .md file opens in Markdown mode" (eq 'markdown-mode (buffer-major-mode (current-buffer)))
         (buffer-major-mode (current-buffer)))
  (check "headings are highlighted" (has-face-p 0 3 :md-heading))
  (check "bold text is highlighted" (has-face-p 2 7 :md-strong))
  (check "link text is highlighted" (has-face-p 2 26 :md-link))
  (check "list markers are highlighted" (has-face-p 4 0 :md-list))
  (check "fenced Lisp is highlighted as Lisp" (and (has-face-p 8 2 :definer) (has-face-p 8 2 :md-code-block)))
  ;; Open the preview from the tab's right-click menu, with the focus elsewhere.
  (let* ((view (current-view))
         (page (cadre-ui::view-page *window* view))
         (entries (menu-entries (cadre-ui::tab-menu-model page)))
         (preview (find "Open Preview" entries :key #'car :test #'string=)))
    (check "a Markdown file's tab menu offers Open Preview" preview (mapcar #'car entries))
    (cadre-ui::show-repl-page :focus t)
    (cadre-ui::select-tab *window* (cadre-ui::view-group view) page)
    (gio:action-group-activate-action (gtk:window-get-application (cadre-ui::window-gtk-window *window*))
                                      "command" (glib:variant-new-string (cdr preview)))))

(then 600
  (let ((preview (preview-buffer)))
    (check "the preview opens" (and preview (cadre-ui::buffer-views *window* preview)))
    (check "beside the source, in another group"
           (and preview (not (eq (cadre-ui::view-group (first (cadre-ui::buffer-views *window* preview)))
                                 (cadre-ui::view-group (current-view))))))
    (check "the source keeps the focus" (string= "guide.md" (buffer-name (current-buffer))))
    (let* ((source-view (current-view))
           (page (cadre-ui::view-page *window* source-view))
           (button (tab-markdown-switch source-view)))
      (check "the selected Markdown tab has a Preview button" (gtk:widget-get-visible button))
      (gobject:emit button :clicked)
      (check "Preview switches the selected tab to the rendered view"
             (cadre-ui::buffer-local (cadre-ui::view-buffer (current-view)) :preview-of))
      (check "Preview replaces the current tab" (eq page (cadre-ui::view-page *window* (current-view))))
      (gobject:emit (tab-markdown-switch (current-view)) :clicked)
      (check "Source switches back to the Markdown file"
             (eq (find-buffer "guide.md") (current-buffer))))
    (check "other tabs' menus don't"
           (not (find "Open Preview" (menu-entries (cadre-ui::tab-menu-model
                                                    (cadre-ui::view-page *window* (first (cadre-ui::buffer-views *window* preview)))))
                      :key #'car :test #'string=)))
    (check "the preview shows the text without markup"
           (and (search "Some bold text and a link." (preview-text)) (not (search "**" (preview-text))))
           (preview-text))
    (check "with list bullets and the code" (and (search "•" (preview-text)) (search "(defun hi () 1)" (preview-text))))
    (check "headings are drawn large"
           (gtk:text-iter-has-tag (cadre-ui::iter-at (buffer-text preview) 0)
                                  (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table (buffer-text preview)) "md-p-h1")))
    (check "links are remembered for clicking"
           (find "https://example.com" (cadre-ui::buffer-local preview :links) :key #'third :test #'string=))
    (check "the preview needs no saving" (not (cadre-ui::buffer-needs-saving-p preview)))
    (let ((state (cadre-ui::view-state (first (cadre-ui::buffer-views *window* preview)))))
      (check "the session remembers the preview by its source"
             (equal (list :preview (uiop:native-namestring (merge-pathnames "guide.md" *root*))) state) state)
      ;; Restoring it: close the preview, then restore its group's state.
      (let ((group (cadre-ui::view-group (first (cadre-ui::buffer-views *window* preview)))))
        (cadre-ui::close-view *window* (first (cadre-ui::buffer-views *window* preview)))
        (cadre-ui::restore-group *window* group (list :group :files (list state) :selected 0))
        (check "and restores it" (and (preview-buffer) (cadre-ui::buffer-views *window* (preview-buffer))))))))

;; Restoring the preview rebuilds the tabs; allow allocation and the deferred
;; scroll-to-selected-tab callback to run before inspecting the adjustment.
(then 150
  (let* ((strip (cadre-ui::group-strip (cadre-ui::view-group (current-view))))
         (tab (cdr (assoc (cadre-ui::view-page *window* (current-view)) (cadre-ui::strip-tabs strip))))
         (adjustment (gtk:scrolled-window-get-hadjustment (cadre-ui::strip-scroller strip))))
    (multiple-value-bind (ok x) (gtk:widget-translate-coordinates tab (cadre-ui::strip-box strip) 0d0 0d0)
      (check "the selected tab is scrolled into view"
             (and ok (>= x (gtk:adjustment-get-value adjustment))
                  (<= (+ x (gtk:widget-get-width tab))
                      (+ (gtk:adjustment-get-value adjustment) (gtk:adjustment-get-page-size adjustment) 1)))
             (list x (gtk:adjustment-get-value adjustment) (gtk:adjustment-get-page-size adjustment) (gtk:adjustment-get-upper adjustment)))))
  (screenshot "28-markdown-preview")
  (set-cursor 0 7)
  (insert-at-cursor " Book"))

(then 600
  (check "the preview follows edits" (search "Guide Book" (preview-text)) (subseq (preview-text) 0 20))
  (set-cursor 5 5)
  (call-command 'cadre-ui::markdown-newline)
  (check "Return continues a list" (string= "- " (line-text 6)) (line-text 6))
  (call-command 'cadre-ui::markdown-newline)
  (check "and ends it on an empty item" (string= "" (line-text 6)) (list (line-text 5) (line-text 6) (line-text 7)))
  (set-cursor 2 1)
  (call-command 'cadre-ui::markdown-bold)
  (check "bold wraps the word at the cursor" (search "**Some**" (line-text 2)) (line-text 2))
  (call-command 'cadre-ui::markdown-bold)
  (check "and again unwraps it" (string= "Some **bold**" (subseq (line-text 2) 0 13)) (line-text 2))
  (let ((items (mapcar #'second (markdown-headings (buffer-string (current-buffer))))))
    (check "headings for Go to Heading" (equal '("Guide Book" "Usage") items) items))
  (setf (buffer-modified-p (current-buffer)) nil)
  (call-command 'close-tab))

(then 300
  (check "closing the source closes its preview" (null (preview-buffer)))
  ;; From the explorer: a preview without an editor.
  (let ((list-view (gtk:scrolled-window-get-child (adw:bin-get-child (cadre-ui::window-explorer-holder *window*)))))
    (cadre-ui::show-explorer-menu list-view (merge-pathnames "guide.md" *root*) 10 10)
    (let ((popover (find-if (lambda (w) (typep w 'gtk:popover))
                            (find-widgets list-view (lambda (w) (typep w 'gtk:popover))))))
      (check "right-clicking a Markdown file offers Open Preview"
             (and popover (member "Open Preview" (label-texts popover) :test #'string=))
             (and popover (label-texts popover)))
      (when popover (gtk:popover-popdown popover)))
    (cadre-ui::show-explorer-menu list-view (merge-pathnames "src/m1.lisp" *root*) 10 10)
    (let ((popover (car (last (find-widgets list-view (lambda (w) (typep w 'gtk:popover)))))))
      (check "and other files don't"
             (and popover (not (member "Open Preview" (label-texts popover) :test #'string=))))
      (when popover (gtk:popover-popdown popover))))
  (cadre-ui::open-markdown-preview (merge-pathnames "guide.md" *root*)))

(then 400
  (check "the explorer's menu can open just the preview"
         (and (preview-buffer) (cadre-ui::buffer-views *window* (preview-buffer))
              (null (cadre-ui::buffer-views *window* (find-buffer "guide.md")))))
  (check "showing the file" (search "Guide" (preview-text)))
  (let* ((view (current-view))
         (page (cadre-ui::view-page *window* view)))
    (gobject:emit (tab-markdown-switch view) :clicked)
    (check "an Explorer preview becomes a full source editor in the same tab"
           (and (eq page (cadre-ui::view-page *window* view))
                (cadre-ui::view-gutter view)
                (gtk:widget-get-visible (cadre-ui::view-gutter view))
                (cadre-ui::view-source-controllers-p view)
                (gtk:text-view-get-editable (cadre-ui::view-text-view view))))
    (let ((callback (cadre-ui::view-on-cursor-moved view)) (moves 0))
      (unwind-protect
           (progn
             (setf (cadre-ui::view-on-cursor-moved view)
                   (lambda (v) (incf moves) (when callback (funcall callback v))))
             (set-cursor 0 2)
             (check "cursor callbacks follow the source buffer" (plusp moves)))
        (setf (cadre-ui::view-on-cursor-moved view) callback)))
    (insert-at-cursor " unsaved")
    (let ((source (current-buffer)))
      (gobject:emit (tab-markdown-switch view) :clicked)
      (let ((confirm (symbol-function 'cadre-ui::confirm-close-buffers)) (asked nil))
        (unwind-protect
             (progn
               (setf (symbol-function 'cadre-ui::confirm-close-buffers)
                     (lambda (win buffers continuation)
                       (declare (ignore win))
                       (setf asked buffers)
                       (funcall continuation nil)))
               (call-command 'close-tab)
               (check "closing Preview asks about its unsaved source"
                      (and (equal asked (list source)) (buffer-modified-p source)))
               (check "cancel keeps the preview tab open"
                      (eq page (cadre-ui::view-page *window* view))))
          (setf (symbol-function 'cadre-ui::confirm-close-buffers) confirm)))
      (setf (buffer-modified-p source) nil)))
  (call-command 'close-tab))

(then 300
  (check "closing that preview lets the file go too" (and (null (preview-buffer)) (null (find-buffer "guide.md")))))

;;; The HyperSpec
(section "hyperspec")
(defun hyperspec-buffer () (find-if #'cadre-ui::hyperspec-buffer-p (buffer-list)))
(defun hyperspec-text () (buffer-string (hyperspec-buffer)))
(defun hyperspec-page () (cadre-ui::buffer-local (hyperspec-buffer) :hyperspec))

(open-files "src/m1.lisp")

(then 200
  ;; Earlier sections edit m1.lisp; start from what's on disk.
  (text-replace-contents (buffer-text (find-buffer "m1.lisp"))
                         (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil)
  (set-cursor 0 3)
  (check "Lisp's right-click menu offers Look Up in HyperSpec"
         (find "Look Up in HyperSpec" (menu-entries (cadre-ui::editor-context-menu (current-buffer)))
               :key #'car :test #'string=))
  (call-command 'cadre-ui::hyperspec-look-up))

(then 600
  (let ((buffer (hyperspec-buffer)))
    (check "it opens a HyperSpec tab" (and buffer (cadre-ui::buffer-views *window* buffer)))
    (check "beside the code, in another group"
           (and buffer (not (eq (cadre-ui::view-group (first (cadre-ui::buffer-views *window* buffer)))
                                (cadre-ui::view-group (current-view))))))
    (check "the code keeps the focus" (string= "m1.lisp" (buffer-name (current-buffer))) (buffer-name (current-buffer)))
    (check "showing DEFUN's entry"
           (and (search "defun   Macro" (hyperspec-text)) (search "Syntax" (hyperspec-text))
                (search "Examples" (hyperspec-text)))
           (subseq (hyperspec-text) 0 (min 200 (length (hyperspec-text)))))
    (check "named for it" (string= "HyperSpec: defun" (buffer-name buffer)) (buffer-name buffer))
    (check "with links to other entries"
           (find :entry (cadre-ui::sp-links (hyperspec-page)) :key (lambda (l) (first (third l)))))
    (check "examples are highlighted as Lisp"
           (let* ((gtk-buffer (buffer-text buffer))
                  (at (search "(defun " (hyperspec-text) :from-end t)))
             (and at (gtk:text-iter-has-tag (cadre-ui::iter-at gtk-buffer (1+ at))
                                            (cadre-ui::face-tag gtk-buffer :definer)))))
    (check "it needs no saving" (not (cadre-ui::buffer-needs-saving-p buffer)))
    (let ((state (cadre-ui::view-state (first (cadre-ui::buffer-views *window* buffer)))))
      (check "the session remembers the entry" (equal '(:hyperspec "common-lisp" "macro/defun") state) state)
      (let ((group (cadre-ui::view-group (first (cadre-ui::buffer-views *window* buffer)))))
        (cadre-ui::close-view *window* (first (cadre-ui::buffer-views *window* buffer)))
        (cadre-ui::restore-group *window* group (list :group :files (list state) :selected 0))
        (check "and restores it"
               (and (cadre-ui::buffer-views *window* (hyperspec-buffer))
                    (equal "macro/defun" (cadre-ui::sp-entry-id (hyperspec-page))))))))
  (screenshot "hyperspec")
  (let ((link (find-if (lambda (l) (and (eq :entry (first (third l)))
                                        (not (equal "macro/defun" (third (third l))))))
                       (reverse (cadre-ui::sp-links (hyperspec-page))))))
    (cadre-ui::follow-hyperspec-link (third link))))

(then 400
  (let ((page (hyperspec-page)))
    (check "a link opens its entry in the same tab"
           (and (not (equal "macro/defun" (cadre-ui::sp-entry-id page)))
                (= 1 (count-if #'cadre-ui::hyperspec-buffer-p (buffer-list))))
           (cadre-ui::sp-entry-id page))
    (check "with Back at the top" (string= "← Back" (subseq (hyperspec-text) 0 6)))
    (press "M-Left")))

(then 400
  (check "Alt+Left goes back" (equal "macro/defun" (cadre-ui::sp-entry-id (hyperspec-page)))
         (cadre-ui::sp-entry-id (hyperspec-page)))
  (check "and Forward is offered" (search "Forward →" (subseq (hyperspec-text) 0 30)))
  (press "M-Right"))

(then 400
  (check "Alt+Right goes forward" (not (equal "macro/defun" (cadre-ui::sp-entry-id (hyperspec-page)))))
  (let ((view (first (cadre-ui::buffer-views *window* (find-buffer "m1.lisp")))))
    (cadre-ui::activate-group *window* (cadre-ui::view-group view))
    (cadre-ui::focus-view view))
  (set-cursor 1 3)
  (call-command 'cadre-ui::hyperspec-look-up))

(then 400
  (let ((items (mapcar (lambda (p) (docset:record-id (cdr p))) (cadre-ui::picker-items (picker)))))
    (check "* is a function and a variable: a choice of the two"
           (equal '("function/*" "variable/*") (sort (copy-list items) #'string<)) items))
  (cadre-ui::choose (picker)))

(then 400
  (check "choosing one shows it" (member (cadre-ui::sp-entry-id (hyperspec-page)) '("function/*" "variable/*") :test #'equal)
         (cadre-ui::sp-entry-id (hyperspec-page)))
  (cadre-ui::show-hyperspec-entry "common-lisp" "function/equal"))

(then 400
  (screenshot "hyperspec-lists")
  (check "a qualified name too"
         (equal "function/mapc" (docset:record-id (cdr (first (cadre-ui::hyperspec-records "#'cl:mapcar"))))))
  (let ((*window* *window*))
    (cadre-ui::hyperspec-search-for "hash-table-count"))
  (check "Search HyperSpec picks from every entry"
         (> (length (cadre-ui::picker-items (picker))) 1000) (length (cadre-ui::picker-items (picker))))
  (cadre-ui::close-picker (picker))
  (let ((m1 (find-buffer "m1.lisp")))
    (loop for (code . expected) in '(("(gethash k table)" . "accessor/gethash")
                                     ("(apply #'+ args)" . "function/apply")
                                     ("(let ((x 1)) x)" . "special-operator/let")
                                     ("(mapcar #'1+ xs)" . "function/mapc"))
          do (let ((view (first (cadre-ui::buffer-views *window* m1))))
               (cadre-ui::activate-group *window* (cadre-ui::view-group view))
               (cadre-ui::focus-view view)
               (text-replace-contents (buffer-text m1) code)
               (set-cursor 0 2)
               (call-command 'cadre-ui::hyperspec-look-up)
               (check (format nil "looking up ~a" code)
                      (and (equal expected (cadre-ui::sp-entry-id (hyperspec-page)))
                           (search "Syntax" (hyperspec-text)))
                      (list (cadre-ui::sp-entry-id (hyperspec-page)) (subseq (hyperspec-text) 0 (min 60 (length (hyperspec-text))))
                            (gtk:label-get-text (cadre-ui::window-status-message *window*))))))
    ;; A selection that takes in a paren.
    (text-replace-contents (buffer-text m1) "(gethash k table)")
    (gtk:text-buffer-select-range (buffer-text m1) (cadre-ui::iter-at (buffer-text m1) 0)
                                  (cadre-ui::iter-at (buffer-text m1) 8))
    (cadre-ui::show-hyperspec-entry "common-lisp" "function/equal")
    (call-command 'cadre-ui::hyperspec-look-up)
    (check "a selected (gethash is looked up as gethash"
           (equal "accessor/gethash" (cadre-ui::sp-entry-id (hyperspec-page))) (cadre-ui::sp-entry-id (hyperspec-page)))
    (text-replace-contents (buffer-text m1) (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))
    (setf (buffer-modified-p m1) nil)
    ;; In the REPL: its symbol, not the one at the cursor in the file's tab.
    (cadre-ui::show-repl-page :focus t)
    (let* ((repl (cadre-ui::repl-view))
           (gtk-buffer (cadre-ui::view-gtk-buffer repl)))
      (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) "(apply #'+ xs" -1)
      (gtk:text-buffer-place-cursor gtk-buffer (cadre-ui::iter-at gtk-buffer (- (gtk:text-buffer-get-char-count gtk-buffer) 10)))
      (cadre-ui::focus-view repl)
      (call-command 'cadre-ui::hyperspec-look-up)
      (check "looking up in the REPL looks up the REPL's symbol"
             (equal "function/apply" (cadre-ui::sp-entry-id (hyperspec-page))) (cadre-ui::sp-entry-id (hyperspec-page)))
      (check "and the REPL keeps the keyboard" (eq repl (cadre-ui::focused-view *window*)))
      (gtk:text-buffer-delete gtk-buffer (cadre-ui::iter-at gtk-buffer (- (gtk:text-buffer-get-char-count gtk-buffer) 13))
                              (gtk:text-buffer-get-end-iter gtk-buffer))
      ;; Leave the file's tab with the focus, as the sections after this expect.
      (let ((view (first (cadre-ui::buffer-views *window* m1))))
        (cadre-ui::activate-group *window* (cadre-ui::view-group view))
        (cadre-ui::focus-view view))))
  (let ((view (first (cadre-ui::buffer-views *window* (hyperspec-buffer)))))
    (cadre-ui::close-view *window* view)))

(then 300
  (check "the tab closes" (null (cadre-ui::buffer-views *window* (hyperspec-buffer)))))

;;; Files from the explorer
(section "explorer")
(defun choose-name (text)
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) text)
  (cadre-ui::choose (picker)))

(then 100
  (cadre-ui::new-file-in (uiop:ensure-directory-pathname *root*))
  (choose-name "docs/new.lisp"))

(then 400
  (let ((path (merge-pathnames "docs/new.lisp" *root*)))
    (check "New File makes the file, and its folders" (probe-file path))
    (check "and opens it" (and (find-file-buffer path) (string= "new.lisp" (buffer-name (current-buffer))))))
  (insert-at-cursor "(defun x ())")
  (cadre-ui::rename-in-explorer (merge-pathnames "docs/new.lisp" *root*))
  (check "Rename starts with the old name" (string= "new.lisp" (gtk:editable-get-text (cadre-ui::picker-entry (picker)))))
  (choose-name "renamed.md"))

(then 400
  (let ((new (merge-pathnames "docs/renamed.md" *root*)))
    (check "Rename moves the file" (and (probe-file new) (not (probe-file (merge-pathnames "docs/new.lisp" *root*)))))
    (check "and its buffer follows, unsaved changes and all"
           (let ((b (find-file-buffer new)))
             (and b (string= "renamed.md" (buffer-name b)) (buffer-modified-p b)
                  (search "(defun x ())" (buffer-string b))))
           (mapcar #'buffer-name (buffer-list)))
    (check "taking the mode of its new name" (eq 'markdown-mode (buffer-major-mode (find-file-buffer new))))
    (check "and the tab its name" (member "renamed.md ●" (tab-titles) :test #'string=) (tab-titles)))
  (cadre-ui::save-buffer))

(then-when ((not (buffer-modified-p (current-buffer))))
  (cadre-ui::rename-in-explorer (uiop:ensure-directory-pathname (merge-pathnames "docs/" *root*)))
  (choose-name "notes"))

(then 400
  (let ((new (merge-pathnames "notes/renamed.md" *root*)))
    (check "renaming a folder carries the buffers of files inside it"
           (and (probe-file new) (find-file-buffer new))
           (list (probe-file new) (mapcar (lambda (b) (list (buffer-name b) (buffer-file b))) (buffer-list))
                 (gtk:label-get-text (cadre-ui::window-status-message *window*))))
    (let* ((trashed '())
           (cadre-ui::*trash-function* (lambda (path) (push path trashed))))
      (cadre-ui::trash-path (uiop:ensure-directory-pathname (merge-pathnames "notes/" *root*)))
      (check "Move to Trash trashes the folder" (= 1 (length trashed)))
      (check "and closes the unmodified buffers of files in it" (null (find-file-buffer new))))))

;;; Pinned tabs
(section "pinned-tabs")
(defun group-titles (group) (mapcar #'adw:tab-page-get-title (cadre-ui::group-pages group)))

(then 100
  (dolist (file '("src/m1.lisp" "src/m2.lisp" "src/m3.lisp"))
    (open-file-path (merge-pathnames file *root*))))

(then 500
  (show-buffer-named "m3.lisp")
  (call-command 'cadre-ui::toggle-pin-tab))

(then 300
  (let* ((group (cadre-ui::view-group (current-view)))
         (page (cadre-ui::view-page *window* (current-view)))
         (strip (cadre-ui::group-strip group))
         (tab (cdr (assoc page (cadre-ui::strip-tabs strip)))))
    (check "a pinned tab moves to the front" (string= "m3.lisp" (first (group-titles group))) (group-titles group))
    (check "and shows a pin instead of ×"
           (and tab (gtk:widget-has-css-class tab "pinned")
                (find-widgets tab (lambda (w) (and (typep w 'gtk:button) (gtk:widget-has-css-class w "cadre-tab-pin"))))))
    (check "the session remembers it"
           (let ((state (cadre-ui::group-state *window* group)))
             (equal "m3.lisp" (file-namestring (first (nth (first (getf (rest state) :pinned)) (getf (rest state) :files))))))
           (cadre-ui::group-state *window* group)))
  (screenshot "29-pinned-tab")
  (show-buffer-named "m1.lisp")
  (dolist (b (buffer-list)) (setf (buffer-modified-p b) nil))
  (call-command 'cadre-ui::close-other-tabs))

(then 400
  (let ((titles (group-titles (cadre-ui::view-group (current-view)))))
    (check "Close Others leaves pinned tabs" (equal '("m3.lisp" "m1.lisp") titles) titles))
  (show-buffer-named "m3.lisp")
  (call-command 'cadre-ui::toggle-pin-tab))

(then 300
  (check "and unpinning puts it back among the others"
         (not (adw:tab-page-get-pinned (cadre-ui::view-page *window* (current-view))))))

;;; Outline
(section "outline")
(defun outline-labels ()
  (mapcar #'first (cadre-ui::ol-items cadre-ui::*outline*)))

(then 100
  (open-file-path (merge-pathnames "src/m1.lisp" *root*)))

(then 400
  (text-replace-contents (buffer-text (current-buffer))
                         (format nil "(defvar *size* 3)~%(defun area (w h)~%  (* w h))~%(defmacro twice (x) `(progn ,x ,x))~%"))
  (set-cursor 2 2)
  (call-command 'cadre-ui::show-outline))

(then 300
  (check "the Outline lists the file's definitions" (equal '("*size*" "area" "twice") (outline-labels)) (outline-labels))
  (check "with the one at the cursor selected"
         (let ((row (gtk:list-box-get-selected-row (cadre-ui::ol-list cadre-ui::*outline*))))
           (and row (= 1 (gtk:list-box-row-get-index row)))))
  (screenshot "30-outline")
  (set-cursor 3 0)
  (insert-at-cursor (format nil "(defun perimeter (w h) (* 2 (+ w h)))~%")))

(then 800
  (check "and follows edits" (member "perimeter" (outline-labels) :test #'string=) (outline-labels))
  (gtk:widget-activate (gtk:list-box-get-row-at-index (cadre-ui::ol-list cadre-ui::*outline*) 0))
  (check "clicking an item goes there" (equal '(0 0) (cursor)) (cursor))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil)
  (open-file-path (merge-pathnames "guide.md" *root*)))

(then 500
  (check "for Markdown, the headings" (member "Usage" (outline-labels) :test #'string=) (outline-labels)))

(defun project-outline-labels ()
  (mapcar (lambda (row) (if (eq (first row) :group) (format nil "[~a]" (second row)) (definition-name (third (second row)))))
          (cadre-ui::ol-rows cadre-ui::*outline*)))

(then 100
  (ensure-directories-exist (merge-pathnames "pkg/" *root*))
  (with-open-file (o (merge-pathnames "pkg/app.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "(in-package #:my-app)~%(defun start () 1)~%(defvar *mode* :dev)~%"))
  (with-open-file (o (merge-pathnames "pkg/more.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "(in-package :my-app)~%(defun stop () 2)~%"))
  (with-open-file (o (merge-pathnames "pkg/core.clj" *root*) :direction :output :if-exists :supersede)
    (format o "(ns app.core)~%(defn greet [n] n)~%"))
  (open-file-path (merge-pathnames "pkg/app.lisp" *root*)))

(then 300
  (gtk:toggle-button-set-active
   (find-if (lambda (w) (and (typep w 'gtk:toggle-button) (equal "Project" (gtk:button-get-label w))))
            (find-widgets (cadre-ui::ol-widget cadre-ui::*outline*) (constantly t)))
   t))

(then-when ((member "[my-app]" (project-outline-labels) :test #'string=) :timeout 15)
  (let ((labels (project-outline-labels)))
    (check "Project groups the definitions by package and namespace"
           (and (member "[app.core]" labels :test #'string=) (member "[my-app]" labels :test #'string=)) labels)
    (check "a package's definitions from all its files, its group open for the current file"
           (let ((at (position "[my-app]" labels :test #'string=)))
             (subsetp '("start" "*mode*" "stop") (subseq labels (1+ at)) :test #'string=))
           labels)
    (check "other groups start closed" (not (member "greet" labels :test #'string=)) labels))
  (screenshot "30b-project-outline")
  (gtk:editable-set-text (cadre-ui::ol-filter cadre-ui::*outline*) "gre"))

(then 400
  (let ((labels (project-outline-labels)))
    (check "the filter finds definitions in any group" (equal '("[app.core]" "greet") labels) labels))
  (gtk:widget-activate (gtk:list-box-get-row-at-index (cadre-ui::ol-project-list cadre-ui::*outline*) 1)))

(then 400
  (check "clicking one opens its file at it" (and (string= "core.clj" (buffer-name (current-buffer))) (equal '(1 0) (cursor)))
         (list (buffer-name (current-buffer)) (cursor)))
  (gtk:editable-set-text (cadre-ui::ol-filter cadre-ui::*outline*) "")
  (gtk:drop-down-set-selected (cadre-ui::ol-group-dropdown cadre-ui::*outline*) 1))

(then 400
  (check "or groups them by file" (member "[pkg/more.lisp]" (project-outline-labels) :test #'string=)
         (project-outline-labels))
  (call-command 'go-to-symbol-in-project))

(then 300
  (check "Go to Symbol in Project offers every definition"
         (find "stop" (cadre-ui::picker-items (picker)) :key (lambda (e) (definition-name (third e))) :test #'string=))
  (cadre-ui::close-picker (picker)))

(then 100
  (gtk:drop-down-set-selected (cadre-ui::ol-group-dropdown cadre-ui::*outline*) 0)
  (setf (gethash "my-app" (cadre-ui::ol-expanded cadre-ui::*outline*)) t)
  (gtk:check-button-set-active
   (find-if (lambda (w) (and (typep w 'gtk:check-button) (equal "Sort by name" (gtk:check-button-get-label w))))
            (find-widgets (cadre-ui::ol-widget cadre-ui::*outline*) (constantly t)))
   t))

(then 300
  (let* ((labels (project-outline-labels))
         (at (position "[my-app]" labels :test #'string=)))
    (check "Sort by name puts a package's definitions in order, across its files"
           (equal '("*mode*" "start" "stop") (subseq labels (1+ at) (+ at 4))) labels))
  (check "and is remembered" (eq t (cadre-ui::setting :outline-sort-by-name)))
  (gtk:toggle-button-set-active
   (find-if (lambda (w) (and (typep w 'gtk:toggle-button) (equal "This File" (gtk:button-get-label w))))
            (find-widgets (cadre-ui::ol-widget cadre-ui::*outline*) (constantly t)))
   t)
  (open-file-path (merge-pathnames "pkg/app.lisp" *root*)))

(then 400
  (check "the file's outline is sorted too" (equal '("*mode*" "start") (outline-labels)) (outline-labels))
  (set-cursor 1 3)
  (cadre-ui::refresh-outline))

(then 200
  (check "with the definition at the cursor still the one selected"
         (let ((row (gtk:list-box-get-selected-row (cadre-ui::ol-list cadre-ui::*outline*))))
           (and row (string= "start" (first (nth (gtk:list-box-row-get-index row) (cadre-ui::ol-items cadre-ui::*outline*)))))))
  (setf (cadre-ui::setting :outline-sort-by-name) nil))

;;; Claude edit
(section "claude-edit")
(defvar *chat-bottom* nil)

(then 50
  ;; Lisp's *earmuffs* in code spans once made markup GTK refused, and the
  ;; reply showed as nothing.
  (let ((box (make-instance 'gtk:box :orientation :vertical)))
    (cadre-ui::render-markdown box (format nil "`move-entity` is right: `*scale*` and `*knight*` stay as they are.~%Some *emphasis* and **bold** too."))
    (let ((text (format nil "~{~a~}" (label-texts box))))
      (check "a reply with *earmuffs* in code shows its text"
             (and (search "*scale*" text) (search "*knight*" text) (search "emphasis" text))
             text)))
  ;; The panel's tabs scroll without a scrollbar, which covered them.
  (check "the panel's tabs have no scrollbar to cover them"
         (eq :external (gtk:scrolled-window-get-policy
                        (gtk:widget-get-parent (gtk:widget-get-parent
                                                (first (find-widgets (cadre-ui::panel-widget (cadre-ui::window-panel *window*))
                                                                     (lambda (w) (typep w 'gtk:stack-switcher))))))))))

(then 100
  ;; Focusing an earlier reply mustn't scroll the chat back up.
  (cadre-ui::show-claude-page)
  (dotimes (i 40) (cadre-ui::chat-note (format nil "Reply ~d" i))))

(then 500
  ;; The messages, whether or not Claude is set up here.
  (gtk:stack-set-visible-child-name (cadre-ui::chat-stack cadre-ui::*chat*) "chat")
  (cadre-ui::chat-scroll-to-end))

(then 500
  (let* ((adj (gtk:scrolled-window-get-vadjustment (cadre-ui::chat-scroller cadre-ui::*chat*)))
         (first-label (first (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*)
                                           (lambda (w) (and (typep w 'gtk:label) (gtk:label-get-selectable w)))))))
    (setf *chat-bottom* (gtk:adjustment-get-value adj))
    (gtk:widget-grab-focus first-label)))

(then 300
  (let ((adj (gtk:scrolled-window-get-vadjustment (cadre-ui::chat-scroller cadre-ui::*chat*))))
    ;; Thinking with text is shown, folded; the usual empty thinking adds nothing.
    (let ((before (length (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*)
                                        (lambda (w) (typep w 'gtk:expander))))))
      (cadre-ui::handle-claude-event (cadre::make-claude-event :kind :thinking :text ""))
      (cadre-ui::handle-claude-event (cadre::make-claude-event :kind :thinking :text (format nil "Mostly done — three cleanups remain.~%Details here.")))
      (let ((expanders (find-widgets (cadre-ui::chat-messages cadre-ui::*chat*) (lambda (w) (typep w 'gtk:expander)))))
        (check "Claude's thinking, when it has text, shows as one folded row"
               (and (= (length expanders) (1+ before))
                    (search "Mostly done" (gtk:label-get-text (gtk:expander-get-label-widget (car (last expanders)))))
                    (search "Details here" (format nil "~{~a~}" (label-texts (gtk:expander-get-child (car (last expanders)))))))
               (length expanders))))
    (check "focusing an earlier reply leaves the chat where it was"
           (and (plusp *chat-bottom*) (= *chat-bottom* (gtk:adjustment-get-value adj)))
           (list *chat-bottom* (gtk:adjustment-get-value adj)))))

(open-files "src/m1.lisp")
(then 100
  (show-buffer-named "m1.lisp")
  (set-cursor 1 3)                      ; in (defun area …)
  (call-command 'cadre-ui::claude-edit)
  (check "Claude edit asks what to change"
         (search "describe how" (gtk:search-entry-get-placeholder-text (cadre-ui::picker-entry (picker)))))
  (choose-name "explain the formula"))

(then-when ((cadre-ui::review-buffer-p (find-buffer "m1.lisp")) :timeout 20)
  (check "and Claude's change comes back as a diff to review" (cadre-ui::review-buffer-p (find-buffer "m1.lisp")))
  (call-command 'cadre-ui::accept-edit))

(then 500
  (check "accepting it changes the code"
         (search (format nil ";; explain the formula~%(defun area") (buffer-string (find-buffer "m1.lisp")))
         (buffer-string (find-buffer "m1.lisp")))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil))

;;; Everyday things
(section "everyday")
(open-files "src/m1.lisp")
(defun m1-text () (buffer-string (find-buffer "m1.lisp")))
(defmacro with-current-buffer-repl (&body body)
  "Run BODY as if typed in the REPL (its buffer current)."
  `(let ((*frontend* *frontend*))
     (cadre-ui::focus-view (cadre-ui::repl-view))
     ,@body))

(then 100
  (show-buffer-named "m1.lisp")
  (cadre-ui::focus-view (current-view))
  ;; Zoom
  (call-command 'cadre-ui::zoom-in)
  (call-command 'cadre-ui::zoom-in)
  (check "zooming in makes the text bigger" (and (= 2 cadre-ui::*editor-zoom*) (search "pt" (cadre-ui::zoomed-font-size)))
         (cadre-ui::zoomed-font-size))
  (call-command 'cadre-ui::zoom-reset)
  (check "and zoom-reset puts it back" (zerop cadre-ui::*editor-zoom*))
  ;; Word wrap
  (call-command 'cadre-ui::toggle-word-wrap)
  (check "word wrap turns on" (eq :word-char (gtk:text-view-get-wrap-mode (view-text-view (current-view)))))
  (call-command 'cadre-ui::toggle-word-wrap)
  ;; Lines
  (text-replace-contents (buffer-text (find-buffer "m1.lisp")) (format nil "one~%two~%three"))
  (set-cursor 1 1)
  (call-command 'cadre-ui::move-lines-up)
  (check "moving a line up" (string= (format nil "two~%one~%three") (m1-text)) (m1-text))
  (check "takes the cursor along" (equal '(0 1) (cursor)) (cursor))
  (call-command 'cadre-ui::move-lines-down)
  (call-command 'cadre-ui::move-lines-down)
  (check "and down, to the last line" (string= (format nil "one~%three~%two") (m1-text)) (m1-text))
  (call-command 'cadre-ui::move-lines-down)
  (check "but not past it" (string= (format nil "one~%three~%two") (m1-text)))
  (set-cursor 0 0)
  (call-command 'cadre-ui::duplicate-lines-down)
  (check "duplicating a line puts the copy below, with the cursor"
         (and (string= (format nil "one~%one~%three~%two") (m1-text)) (equal '(1 0) (cursor))) (list (m1-text) (cursor)))
  (gtk:text-buffer-undo (buffer-text (find-buffer "m1.lisp")))
  (check "in one undo step" (string= (format nil "one~%three~%two") (m1-text)))
  ;; Rectangles
  (text-replace-contents (buffer-text (find-buffer "m1.lisp")) (format nil "abcd~%efgh~%ijkl"))
  (cadre-ui::push-mark (find-buffer "m1.lisp") 1)
  (set-cursor 2 3)
  (call-command 'cadre-ui::kill-rectangle)
  (check "killing a rectangle takes those columns from each line"
         (string= (format nil "ad~%eh~%il") (m1-text)) (m1-text))
  (set-cursor 0 0)
  (call-command 'cadre-ui::yank-rectangle)
  (check "and yanking puts them back as a rectangle"
         (string= (format nil "bcad~%fgeh~%jkil") (m1-text)) (m1-text))
  ;; Recent
  (check "recent files are remembered"
         (find "m1.lisp" (cadre-ui::setting :recent-files) :key #'file-namestring :test #'string=))
  (check "and recent folders"
         (find (uiop:native-namestring (cadre-ui::window-project *window*)) (cadre-ui::setting :recent-projects)
               :test #'string=))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) nil))

(then 200
  ;; The REPL's history, at the prompt
  (let ((cadre-ui::*repl* cadre-ui::*repl*))
    (cadre-ui::show-repl-page :focus t)
    ;; Run alone, nothing has been typed at the prompt yet.
    (cadre-ui::ensure-repl-history-loaded)
    (when (zerop (length (cadre-ui::repl-history cadre-ui::*repl*)))
      (vector-push-extend "(list :a 42)" (cadre-ui::repl-history cadre-ui::*repl*))
      (cadre-ui::save-repl-history))
    (cadre-ui::set-repl-input "draft")
    (let ((cadre-ui::*this-command* nil))
      (with-current-buffer-repl
        (call-command 'cadre-ui::repl-up)))
    (check "Up at the prompt brings back the last input"
           (let ((history (cadre-ui::repl-history cadre-ui::*repl*)))
             (string= (aref history (1- (length history))) (cadre-ui::repl-input)))
           (cadre-ui::repl-input))
    (with-current-buffer-repl (call-command 'cadre-ui::repl-down))
    (check "and Down past the newest brings back what was being typed"
           (string= "draft" (cadre-ui::repl-input)) (cadre-ui::repl-input))
    (check "the history is saved for next time"
           (search "(list :a" (uiop:read-file-string (cadre-ui::repl-history-file))))
    (cadre-ui::set-repl-input "")))

(then 50
  (setf cadre-ui::*project-definitions-time* 0)
  (cadre-ui::refresh-project-definitions))

(then-when ((gethash "scale-shape" cadre-ui::*project-definitions*) :timeout 10)
  (if (cadre-ui::connected-p)
      (check "Go to Definition without a Lisp (skipped: one is running)" t)
      (progn
        (show-buffer-named "m1.lisp")
        (text-replace-contents (buffer-text (find-buffer "m1.lisp")) (format nil "(scale-shape 'sq 2)~%"))
        (set-cursor 0 3)
        (call-command 'edit-definition))))

(then 500
  (unless (cadre-ui::connected-p)
    (check "Go to Definition without a Lisp finds it in the project's files"
           (and (string= "hints.lisp" (buffer-name (current-buffer))) (equal 0 (first (cursor))))
           (list (buffer-name (current-buffer)) (cursor)))
    (call-command 'pop-definition)
    (setf (buffer-modified-p (find-buffer "m1.lisp")) nil)
    (set-cursor 0 3)
    (call-command 'find-references)))

(then 800
  (unless (cadre-ui::connected-p)
    (check "and Find References searches the project for the symbol"
           (and (eq :symbol (cadre-ui::ps-mode cadre-ui::*project-search*))
                (string= "scale-shape" (gtk:editable-get-text (cadre-ui::ps-entry cadre-ui::*project-search*)))))))

;;; Git
(section "git")
(open-files "src/m1.lisp")
(defun m1-git () (cadre-ui::buffer-git (find-buffer "m1.lisp")))

(then 100
  (show-buffer-named "m1.lisp")
  (cadre-ui::show-source-control)
  (check "a folder that isn't a repository says so"
         (gtk:widget-get-visible (cadre-ui::sc-not-repo cadre-ui::*source-control*)))
  ;; Make the project a repository with one commit.
  (text-replace-contents (buffer-text (find-buffer "m1.lisp")) (format nil "(defun area (w h)~%  (* w h))~%"))
  (setf (buffer-modified-p (find-buffer "m1.lisp")) t)
  (call-command 'cadre-ui::save-buffer))

(then-when ((string= (format nil "(defun area (w h)~%  (* w h))~%")
                      (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*))))
  (check "the file is saved before committing"
         (string= (format nil "(defun area (w h)~%  (* w h))~%") (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))
         (list (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)) (buffer-file (find-buffer "m1.lisp"))
               (gtk:label-get-text (cadre-ui::window-status-message *window*))))
  (git-ok *root* "init" "-q")
  (git-ok *root* "config" "user.email" "smoke@example.com")
  (git-ok *root* "config" "user.name" "Smoke")
  (git-ok *root* "add" "-A")
  (git-ok *root* "commit" "-q" "-m" "Start")
  (cadre-ui::git-project-opened)
  (dolist (b (buffer-list)) (cadre-ui::git-attach b)))

(then-when ((and (m1-git) cadre-ui::*git-branch*) :timeout 10)
  (check "the status bar shows the branch"
         (let ((b (gethash :status-branch cadre-ui::*named-widgets*)))
           (and (gtk:widget-get-visible b)
                (search cadre-ui::*git-branch* (gtk:label-get-text (gethash :status-branch-label cadre-ui::*named-widgets*))))))
  (check "and the page shows the repository" (gtk:widget-get-visible (cadre-ui::sc-body cadre-ui::*source-control*)))
  (set-cursor 1 5)
  (insert-at-cursor "1 ")                 ; (* 1 w h)
  (set-cursor 2 0)
  (insert-at-cursor (format nil ";; new~%")))

(then-when ((cadre-ui::gf-hunks (m1-git)) :timeout 10)
  (check "the gutter marks changed and added lines, before saving"
         (equal '(:modified) (mapcar #'hunk-kind (cadre-ui::gf-hunks (m1-git))))
         (list (cadre-ui::gf-hunks (m1-git)) (m1-text) (cadre-ui::head-text-of (m1-git))))
  (screenshot "31-git-gutter")
  (set-cursor 1 0)
  (call-command 'cadre-ui::git-revert-change)
  (check "reverting a change puts the committed lines back"
         (string= (format nil "(defun area (w h)~%  (* w h))~%") (m1-text)) (m1-text))
  (set-cursor 2 0)
  (insert-at-cursor (format nil ";; new~%"))
  (call-command 'cadre-ui::save-buffer))

(then-when ((equal '(:added) (mapcar #'hunk-kind (cadre-ui::gf-hunks (m1-git)))) :timeout 10)
  (set-cursor 0 0)
  (call-command 'cadre-ui::git-next-change)
  (check "next change goes to the added line" (= 2 (first (cursor))) (cursor)))

(then-when ((cadre-ui::git-status-of (merge-pathnames "src/m1.lisp" *root*)) :timeout 10)
  (check "the explorer knows the saved file is modified"
         (eq :modified (first (cadre-ui::git-status-of (merge-pathnames "src/m1.lisp" *root*))))
         (list (loop for k being the hash-keys of cadre-ui::*git-status* using (hash-value v) collect (list k v))
               cadre-ui::*git-entries* (namestring (merge-pathnames "src/m1.lisp" *root*))))
  (check "the page lists it under Changes"
         (find "src/m1.lisp" cadre-ui::*git-entries* :key #'third :test #'string=))
  (call-command 'cadre-ui::git-diff-file))

(then-when ((find-buffer "*Diff m1.lisp*") :timeout 10)
  (check "Diff shows the change" (search "+;; new" (buffer-string (find-buffer "*Diff m1.lisp*")))
         (buffer-string (find-buffer "*Diff m1.lisp*")))
  (call-command 'close-tab)
  (show-buffer-named "m1.lisp")
  (call-command 'cadre-ui::git-stage-file))

(then-when ((find #\M cadre-ui::*git-entries* :key #'first) :timeout 10)
  (check "staging moves it to Staged Changes" (find #\M cadre-ui::*git-entries* :key #'first))
  (screenshot "32-source-control")
  (text-replace-contents (gtk:text-view-get-buffer (cadre-ui::sc-message cadre-ui::*source-control*)) "Add a comment")
  (cadre-ui::commit-from-page))

(then-when ((null (find "src/m1.lisp" cadre-ui::*git-entries* :key #'third :test #'string=)) :timeout 10)
  (check "committing records it" (search "Add a comment" (git-ok *root* "log" "--oneline")))
  (check "and clears the message"
         (string= "" (cadre:text-string (gtk:text-view-get-buffer (cadre-ui::sc-message cadre-ui::*source-control*))))))

(then-when ((null (cadre-ui::gf-hunks (m1-git))) :timeout 10)
  (check "after the commit, the gutter has nothing to mark" (null (cadre-ui::gf-hunks (m1-git)))))

;;; Branches, push and pull, with a remote on disk
(section "git-remote" :needs ("git"))
(defvar *remote* (merge-pathnames "cadre-smoke-remote.git/" (uiop:temporary-directory)))
(defvar *other* (merge-pathnames "cadre-smoke-other/" (uiop:temporary-directory)))

(then 100
  (uiop:delete-directory-tree *remote* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *other* :validate t :if-does-not-exist :ignore)
  (ensure-directories-exist *remote*)
  (git-ok *remote* "init" "-q" "--bare")
  (git-ok *root* "remote" "add" "origin" (uiop:native-namestring *remote*))
  (cadre-ui::git-changed)
  (call-command 'cadre-ui::push-changes))

(then-when ((and (null cadre-ui::*git-busy*) (eql 0 cadre-ui::*git-ahead*)) :timeout 20)
  (check "the first push sets up the upstream" (git-upstream *root*) (git-upstream *root*))
  (check "and then the branch is up to date" (string= "up to date" (gtk:label-get-text (cadre-ui::sc-sync cadre-ui::*source-control*)))
         (gtk:label-get-text (cadre-ui::sc-sync cadre-ui::*source-control*)))
  (git-ok *root* "commit" "-q" "--allow-empty" "-m" "Local work")
  (cadre-ui::git-changed))

(then-when ((eql 1 cadre-ui::*git-ahead*) :timeout 10)
  (check "a new commit shows as one to push"
         (search "↑1" (gtk:label-get-text (gethash :status-branch-label cadre-ui::*named-widgets*)))
         (gtk:label-get-text (gethash :status-branch-label cadre-ui::*named-widgets*)))
  (call-command 'cadre-ui::push-changes))

(then-when ((and (null cadre-ui::*git-busy*) (eql 0 cadre-ui::*git-ahead*)) :timeout 20)
  (check "Push sends it" (eql 0 cadre-ui::*git-ahead*))
  ;; Someone else pushes a commit.
  (git-ok (uiop:temporary-directory) "clone" "-q" (uiop:native-namestring *remote*) (uiop:native-namestring *other*))
  (git-ok *other* "config" "user.email" "other@example.com")
  (git-ok *other* "config" "user.name" "Other")
  (with-open-file (o (merge-pathnames "from-other.txt" *other*) :direction :output) (write-line "hi" o))
  (git-ok *other* "add" "-A")
  (git-ok *other* "commit" "-q" "-m" "Theirs")
  (git-ok *other* "push" "-q")
  (call-command 'cadre-ui::fetch-changes))

(then-when ((and (null cadre-ui::*git-busy*) (eql 1 cadre-ui::*git-behind*)) :timeout 20)
  (check "Fetch shows the commit to pull"
         (search "↓1" (gtk:label-get-text (cadre-ui::sc-sync cadre-ui::*source-control*)))
         (gtk:label-get-text (cadre-ui::sc-sync cadre-ui::*source-control*)))
  (call-command 'cadre-ui::pull-changes))

(then-when ((and (null cadre-ui::*git-busy*) (eql 0 cadre-ui::*git-behind*)) :timeout 20)
  (check "Pull brings it in" (probe-file (merge-pathnames "from-other.txt" *root*)))
  (call-command 'cadre-ui::create-branch)
  (choose-name "feature"))

(then-when ((equal "feature" cadre-ui::*git-branch*) :timeout 10)
  (check "New branch makes it and switches to it" (equal "feature" cadre-ui::*git-branch*))
  (check "the status bar shows it"
         (search "feature" (gtk:label-get-text (gethash :status-branch-label cadre-ui::*named-widgets*))))
  (call-command 'cadre-ui::switch-branch))

(then 1000
  (check "switching offers the branches and a new one"
         (let ((labels (mapcar (cadre-ui::picker-label (picker)) (cadre-ui::picker-items (picker)))))
           (and (member "+ New branch…" labels :test #'string=)
                (member "feature  (current)" labels :test #'string=)
                (find-if (lambda (l) (search "main" l)) labels)))
         (mapcar (cadre-ui::picker-label (picker)) (cadre-ui::picker-items (picker))))
  (cadre-ui::close-picker (picker))
  (cadre-ui::switch-to-branch *root* (git-branch-named-main)))

(defun git-branch-named-main ()
  (getf (find-if (lambda (b) (and (not (getf b :remote)) (not (getf b :current)))) (git-branches *root*)) :name))

(then-when ((and cadre-ui::*git-branch* (not (equal "feature" cadre-ui::*git-branch*))) :timeout 10)
  (check "and switching goes back" (not (equal "feature" cadre-ui::*git-branch*)))
  (uiop:delete-directory-tree *remote* :validate t :if-does-not-exist :ignore)
  (uiop:delete-directory-tree *other* :validate t :if-does-not-exist :ignore))

;;; History, blame, stashes
(section "git-history" :needs ("git-remote"))
(defun history-subjects () (mapcar (lambda (c) (getf c :subject)) (cadre-ui::hist-commits cadre-ui::*history*)))

(then 100
  (show-buffer-named "m1.lisp")
  (call-command 'cadre-ui::show-history))

(then-when ((cadre-ui::hist-commits cadre-ui::*history*) :timeout 10)
  (check "History lists the project's commits, newest first"
         (and (string= "Theirs" (first (history-subjects))) (member "Start" (history-subjects) :test #'string=))
         (history-subjects))
  (check "on the History page" (string= "history" (gtk:stack-get-visible-child-name (cadre-ui::panel-stack (cadre-ui::window-panel *window*)))))
  (gtk:widget-activate (gtk:list-box-get-row-at-index (cadre-ui::hist-list cadre-ui::*history*) 0)))

(then-when ((find-if (lambda (b) (search "*Commit" (buffer-name b))) (buffer-list)) :timeout 10)
  (check "clicking a commit shows it"
         (search "Theirs" (buffer-string (find-if (lambda (b) (search "*Commit" (buffer-name b))) (buffer-list)))))
  (call-command 'close-tab)
  (show-buffer-named "m1.lisp")
  (call-command 'cadre-ui::show-file-history))

(then-when ((equal "src/m1.lisp" (cadre-ui::hist-path cadre-ui::*history*)) :timeout 10)
  (then-wait-a-moment))

(defun then-wait-a-moment () nil)

(then 1000
  (check "File History lists the file's commits"
         (and (member "Add a comment" (history-subjects) :test #'string=)
              (not (member "Theirs" (history-subjects) :test #'string=)))
         (history-subjects))
  (let ((width (gtk:widget-get-width (cadre-ui::view-gutter (current-view)))))
    (setf (cadre-ui::buffer-local (current-buffer) :gutter-before) width))
  (call-command 'cadre-ui::toggle-blame))

(then-when ((cadre-ui::buffer-local (current-buffer) :blame) :timeout 10)
  (let ((blame (cadre-ui::buffer-local (current-buffer) :blame)))
    (check "Blame knows each line's commit"
           (and (string= "Start" (getf (aref blame 0) :summary))
                (string= "Add a comment" (getf (aref blame 2) :summary)))
           (map 'list (lambda (b) (getf b :summary)) blame)))
  (screenshot "33-blame")
  (check "the Source Control page fits the sidebar"
         (<= (gtk:widget-get-width (cadre-ui::sc-widget cadre-ui::*source-control*))
             (gtk:widget-get-width (cadre-ui::window-sidebar *window*)))
         (list (gtk:widget-get-width (cadre-ui::sc-widget cadre-ui::*source-control*))
               (gtk:widget-get-width (cadre-ui::window-sidebar *window*))))
  (check "and the gutter widens for it"
         (> (gtk:widget-get-width (cadre-ui::view-gutter (current-view))) (cadre-ui::buffer-local (current-buffer) :gutter-before)))
  (set-cursor 0 0)
  (insert-at-cursor (format nil ";; mine~%")))

(then-when ((let ((b (cadre-ui::buffer-local (current-buffer) :blame))) (and b (getf (aref b 0) :uncommitted))) :timeout 10)
  (check "an edited line shows as not committed yet" t)
  (call-command 'cadre-ui::toggle-blame)
  (call-command 'cadre-ui::save-buffer))

(then-when ((cadre-ui::git-status-of (merge-pathnames "src/m1.lisp" *root*)) :timeout 10)
  (call-command 'cadre-ui::stash-changes)
  (choose-name "wip"))

(then-when ((and cadre-ui::*git-stash-list* (null (cadre-ui::git-status-of (merge-pathnames "src/m1.lisp" *root*)))) :timeout 10)
  (check "Stash puts the change aside" (not (search ";; mine" (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))))
  (check "and lists the stash" (search "wip" (getf (first cadre-ui::*git-stash-list*) :subject)) cadre-ui::*git-stash-list*)
  (cadre-ui::apply-stash-entry (project-git-root*) (first cadre-ui::*git-stash-list*) :pop t))

(defun project-git-root* () (cadre-ui::project-git-root))

(then-when ((and (null cadre-ui::*git-stash-list*) (cadre-ui::git-status-of (merge-pathnames "src/m1.lisp" *root*))) :timeout 10)
  (check "Pop brings it back" (search ";; mine" (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))
         (list (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)) (gtk:label-get-text (cadre-ui::window-status-message *window*)))))

;;; Staging single changes, and merge conflicts
(section "git-staging" :needs ("git-history"))
(defun staged-diff () (git *root* "diff" "--cached" "--no-color" "-U0"))
(defun m1-disk () (uiop:read-file-string (merge-pathnames "src/m1.lisp" *root*)))
(defun set-m1 (text)
  (text-replace-contents (buffer-text (find-buffer "m1.lisp")) text)
  (setf (buffer-modified-p (find-buffer "m1.lisp")) t)
  (call-command 'cadre-ui::save-buffer))

(then 100
  (show-buffer-named "m1.lisp")
  (set-m1 (format nil "(defun area (w h)~%  (* w h))~%;; a~%~%~%~%~%~%~%~%~%~%;; b~%")))

(then-when ((search ";; b" (m1-disk)) :timeout 10)
  (git-ok *root* "add" "-A")
  (git-ok *root* "commit" "-q" "-m" "Before staging")
  (set-m1 (format nil "(defun area (w h)~%  (* w h))~%;; one~%~%~%~%~%~%~%~%~%~%;; two~%")))

(then-when ((search ";; two" (m1-disk)) :timeout 10)
  (cadre-ui::stage-hunk-lines (current-view) 3 3))

(then-when ((search "+;; one" (staged-diff)) :timeout 10)
  (check "staging one change stages only it"
         (and (search "+;; one" (staged-diff)) (not (search "+;; two" (staged-diff)))) (staged-diff))
  (cadre-ui::open-git-diff (cadre-ui::project-git-root) "src/m1.lisp" :kind :worktree))

(then-when ((find-buffer "*Diff m1.lisp (unstaged)*") :timeout 10)
  (let ((buffer (find-buffer "*Diff m1.lisp (unstaged)*")))
    (check "a file's unstaged changes open as a live diff" (eq 'cadre-ui::git-diff-mode (buffer-major-mode buffer)))
    (show-buffer-named "*Diff m1.lisp (unstaged)*")
    (set-cursor (position-if (lambda (l) (search "@@" l)) (split-text-lines (buffer-string buffer))) 0)
    (call-command 'cadre-ui::diff-stage-hunk)))

(then-when ((search "+;; two" (staged-diff)) :timeout 10)
  (check "s in a diff stages the change at the cursor" (search "+;; two" (staged-diff)))
  (call-command 'close-tab)
  (cadre-ui::open-git-diff (cadre-ui::project-git-root) "src/m1.lisp" :kind :staged))

(then-when ((find-buffer "*Diff m1.lisp (staged)*") :timeout 10)
  (show-buffer-named "*Diff m1.lisp (staged)*")
  (let ((lines (split-text-lines (buffer-string (current-buffer)))))
    (set-cursor (position-if (lambda (l) (search "+;; two" l)) lines) 0))
  (call-command 'cadre-ui::diff-unstage-hunk))

(then-when ((not (search "+;; two" (staged-diff))) :timeout 10)
  (check "and u in a staged diff unstages it" (and (search "+;; one" (staged-diff)) (not (search "+;; two" (staged-diff)))))
  (call-command 'close-tab)
  ;; A conflict: the same line changed on two branches.
  (git-ok *root* "add" "-A")
  (git-ok *root* "commit" "-q" "-m" "Two comments")
  (git-ok *root* "checkout" "-q" "-b" "side")
  (with-open-file (o (merge-pathnames "src/m1.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "(defun area (w h)~%  (* w h 2))~%"))
  (git-ok *root* "commit" "-q" "-am" "Double it")
  (git-ok *root* "checkout" "-q" "-")
  (with-open-file (o (merge-pathnames "src/m1.lisp" *root*) :direction :output :if-exists :supersede)
    (format o "(defun area (w h)~%  (* w h 3))~%"))
  (git-ok *root* "commit" "-q" "-am" "Triple it")
  (cadre-ui::merge-into-current (cadre-ui::project-git-root) "side"))

(then-when ((and (eq :merge cadre-ui::*git-operation*) (search "<<<<<<<" (buffer-string (find-buffer "m1.lisp")))) :timeout 15)
  (check "a merge with a conflict shows the merge banner"
         (gtk:widget-get-visible (adw:bin-get-child (cadre-ui::sc-banner cadre-ui::*source-control*))))
  (check "and the conflicted file under Merge Conflicts"
         (find :conflict cadre-ui::*git-entries* :key (lambda (e) (git-status-kind (first e) (second e)))))
  (check "with the merge's message ready"
         (search "Merge branch" (cadre:text-string (gtk:text-view-get-buffer (cadre-ui::sc-message cadre-ui::*source-control*))))))

(then-when ((cadre-ui::buffer-local (find-buffer "m1.lisp") :conflicts) :timeout 10)
  (show-buffer-named "m1.lisp")
  (check "the conflict is found in the file" (= 1 (length (cadre-ui::buffer-local (find-buffer "m1.lisp") :conflicts))))
  (check "with buttons to resolve it" (= 1 (length (gethash (current-view) cadre-ui::*conflict-buttons*))))
  (check "and its sides colored" (and (has-face-p* 2 :conflict-ours) (has-face-p* 4 :conflict-theirs)))
  (show-buffer-named "m1.lisp"))

(then 500
  (screenshot "34-conflict")
  (set-cursor 2 0)
  (call-command 'cadre-ui::accept-incoming-change))

(defun has-face-p* (line face)
  (let ((gtk-buffer (buffer-text (current-buffer))))
    (gtk:text-iter-has-tag (cadre-ui::line-iter gtk-buffer line 0)
                           (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table gtk-buffer)
                                                      (format nil "cadre-~(~a~)" face)))))

(then 400
  (check "Accept Incoming keeps their side"
         (and (search "(* w h 2)" (m1-text)) (not (search "<<<<<<<" (m1-text))) (not (search "(* w h 3)" (m1-text))))
         (m1-text))
  (check "and the buttons go" (null (gethash (current-view) cadre-ui::*conflict-buttons*)))
  (call-command 'cadre-ui::save-buffer))

(then-when ((and (search "(* w h 2)" (m1-disk))       ; the save has finished:
                 (not (buffer-modified-p (current-buffer)))) ; Stage refuses an unsaved file
            :timeout 10)
  (call-command 'cadre-ui::git-stage-file))

(then-when ((null (find :conflict cadre-ui::*git-entries* :key (lambda (e) (git-status-kind (first e) (second e))))) :timeout 10)
  (git-run-continue))

(defun git-run-continue ()
  (let ((root (cadre-ui::project-git-root)))
    (cadre-ui::git-run root (lambda () (git-continue root :merge)))))

(then-when ((null cadre-ui::*git-operation*) :timeout 15)
  (check "after staging, Commit finishes the merge" (search "Merge branch" (git-ok *root* "log" "-1" "--format=%s"))
         (format nil "status ~s; git status ~s; log ~s" (status-text)
                 (git-ok *root* "status" "--short") (git-ok *root* "log" "-1" "--format=%s")))
  (check "and the banner goes" (null (adw:bin-get-child (cadre-ui::sc-banner cadre-ui::*source-control*)))))

;;; Folding
(section "folding")

(defun folds () (cadre-ui::buffer-folds (current-buffer)))
(defun fold-lines-now () (sort (mapcar (lambda (f) (cadre-ui::fold-first-line (buffer-text (current-buffer)) f)) (folds)) #'<))
(defun line-hidden-p (line)
  (gtk:text-iter-has-tag (cadre-ui::line-iter (buffer-text (current-buffer)) line 1)
                         (cadre-ui::folded-tag (buffer-text (current-buffer)))))
(defun line-height (line)
  (nth-value 1 (gtk:text-view-get-line-yrange (cadre-ui::view-text-view (current-view))
                                              (cadre-ui::line-iter (buffer-text (current-buffer)) line))))

(then 300
  (open-file-path (merge-pathnames "src/m7.lisp" *root*)))

(then-when ((find-buffer "m7.lisp"))
  (show-buffer-named "m7.lisp")
  (set-cursor 2 5)
  (call-command 'cadre-ui::toggle-fold))

(then 400
  (check "Toggle Fold folds the innermost form" (equal '(2) (fold-lines-now)) (fold-lines-now))
  (check "its lines are hidden" (and (line-hidden-p 3) (not (line-hidden-p 2))))
  (check "and take no room" (zerop (line-height 3)) (line-height 3))
  (call-command 'cadre-ui::fold-block))

(then 400
  (check "Fold again folds the form around it" (equal '(1 2) (fold-lines-now)) (fold-lines-now))
  (check "the cursor leaves the hidden text" (= 1 (first (cursor))) (cursor))
  (check "a folded line is marked"
         (gtk:text-iter-has-tag (cadre-ui::line-iter (buffer-text (current-buffer)) 1 1)
                                (gtk:text-tag-table-lookup (gtk:text-buffer-get-tag-table (buffer-text (current-buffer)))
                                                           "cadre-fold-header")))
  (check "the text itself is untouched" (search "(+ (double-it y) 1)" (buffer-string (current-buffer)))))

(then 300
  (screenshot "37-folded")
  (set-cursor 3 4))

(then 300
  (check "moving the cursor into folded text unfolds it" (null (folds)) (fold-lines-now))
  (call-command 'cadre-ui::fold-all))

(then 300
  (check "Fold All folds the top-level forms" (equal '(1) (fold-lines-now)) (fold-lines-now))
  (gtk:text-buffer-insert (buffer-text (current-buffer)) (cadre-ui::line-iter (buffer-text (current-buffer)) 3 0) "x" -1))

(then 300
  (check "editing folded text unfolds it" (null (folds)))
  (let ((gtk-buffer (buffer-text (current-buffer))))
    (gtk:text-buffer-delete gtk-buffer (cadre-ui::line-iter gtk-buffer 3 0) (cadre-ui::line-iter gtk-buffer 3 1)))
  (open-file-path (merge-pathnames "guide.md" *root*)))

(then-when ((find-buffer "guide.md"))
  (show-buffer-named "guide.md")
  (set-cursor 8 2)
  (call-command 'cadre-ui::toggle-fold))

(then 400
  (check "in Markdown, a fenced block folds" (equal '(7) (fold-lines-now)) (fold-lines-now))
  (set-cursor 12 0)
  (call-command 'cadre-ui::toggle-fold))

(then 400
  (check "and a section under its heading" (equal '(7 11) (fold-lines-now)) (fold-lines-now))
  (call-command 'cadre-ui::unfold-all))

(then 300
  (check "Unfold All shows everything" (and (null (folds)) (not (line-hidden-p 13)))))

;;; A new project
(section "new-project")

(defun np (key) (getf cadre-ui::*new-project-dialog* key))
(defvar *new-parent* (merge-pathnames "new-projects/" *root*))

(then 300
  (ensure-directories-exist *new-parent*)
  (setf (cadre-ui::setting :new-project-folder) (namestring *new-parent*))
  (call-command 'cadre-ui::new-project))

(then 600
  (check "New Project opens its dialog, titled New Project"
         (and (np :dialog) (equal "New Project" (adw:dialog-get-title (np :dialog)))))
  (gtk:editable-set-text (np :name) "Bad Name"))

(then 200
  (check "a bad name can't be created" (not (gtk:widget-get-sensitive (np :create))))
  (gtk:editable-set-text (np :name) "smoke-proj")
  (gtk:editable-set-text (np :description) "A smoke test project."))

(then 300
  (check "a good name says where it goes"
         (and (gtk:widget-get-sensitive (np :create))
              (search "new-projects/smoke-proj" (gtk:label-get-text (np :where))))
         (gtk:label-get-text (np :where)))
  (adw:combo-row-set-selected (np :kind) (position :clojure-project cadre-ui::*project-kinds* :key #'car))
  (let ((hidden (not (gtk:widget-get-visible (np :tests)))))
    (adw:combo-row-set-selected (np :kind) 0)
    (check "the test framework is offered only for Common Lisp projects"
           (and hidden (gtk:widget-get-visible (np :tests)))))
  (screenshot "38-new-project")
  (gtk:widget-activate (np :create)))

(defun proj-file (path) (merge-pathnames (concatenate 'string "smoke-proj/" path) *new-parent*))

(then-when ((find-buffer "main.lisp") :timeout 10)
  (check "Create writes the project"
         (every (lambda (p) (probe-file (proj-file p)))
                '("smoke-proj.asd" "src/package.lisp" "src/main.lisp" "tests/main.lisp" "Makefile" "README.md" ".gitignore" "LICENSE")))
  (check "with a Git repository" (uiop:directory-exists-p (proj-file ".git/")))
  (check "and the description in the system" (search "A smoke test project." (uiop:read-file-string (proj-file "smoke-proj.asd"))))
  (check "and opens it" (equal (truename (cadre-ui::window-project *window*)) (truename (proj-file ""))))
  (check "showing its main file" (string= "main.lisp" (buffer-name (current-buffer)))))

(then 1500
  (check "Source Control follows the new project's repository"
         (equal (truename (cadre-ui::project-git-root)) (truename (proj-file "")))
         (cadre-ui::project-git-root))
  (check "and forgets the last one's message and history"
         (and (string= "" (cadre:text-string (gtk:text-view-get-buffer (cadre-ui::sc-message cadre-ui::*source-control*))))
              (null (cadre-ui::hist-commits cadre-ui::*history*))))
  (screenshot "39-new-project-open"))

;;; Private saved revisions and recovery checkpoints.
(section "local-history")
(defvar *history-smoke-buffer* nil)
(defvar *history-smoke-recovered* nil)
(defvar *history-smoke-entry* nil)

(defun snapshot-button (text)
  (find text (find-widgets (adw:dialog-get-child cadre-ui::*snapshot-dialog*)
                          (lambda (w) (typep w 'gtk:button))) :key #'gtk:button-get-label :test #'equal))

(then 50
  (with-open-file (out (merge-pathnames "history-smoke.lisp" *root*) :direction :output :if-exists :supersede)
    (write-string "old saved text" out))
  (open-file-path (merge-pathnames "history-smoke.lisp" *root*)))

(then-when ((find-file-buffer (merge-pathnames "history-smoke.lisp" *root*)))
  (setf *history-smoke-buffer* (find-file-buffer (merge-pathnames "history-smoke.lisp" *root*)))
  (text-replace-contents (buffer-text *history-smoke-buffer*) "new saved text")
  (setf (buffer-modified-p *history-smoke-buffer*) t)
  (call-command 'save-buffer))

(then-when ((not (buffer-modified-p *history-smoke-buffer*)))
  (check "saving retains both the previous and new local revisions"
         (equal '("new saved text" "old saved text")
                (mapcar (lambda (entry) (getf (cdr entry) :text))
                        (cadre-ui::saved-revisions (buffer-file *history-smoke-buffer*)))))
  (call-command 'cadre-ui::local-history)
  (let* ((picker (cadre-ui::window-picker *window*)) (entry (second (cadre-ui::picker-items picker))))
    (check "Local History offers the retained revisions" entry)
    (cadre-ui::close-picker picker)
    (funcall (cadre-ui::picker-on-choose picker) entry)))

(then 300
  (check "history comparison shows the buffer and saved revision"
         (and cadre-ui::*snapshot-dialog* (snapshot-button "Restore to Buffer")))
  (gobject:emit (snapshot-button "Restore to Buffer") :clicked)
  (check "Restore changes only the buffer, leaving newer disk contents intact"
         (and (equal "old saved text" (buffer-string *history-smoke-buffer*))
              (buffer-modified-p *history-smoke-buffer*)
              (equal "new saved text" (uiop:read-file-string (buffer-file *history-smoke-buffer*)))))
  (gtk:text-buffer-undo (buffer-text *history-smoke-buffer*))
  (check "history restore is one undo step" (equal "new saved text" (buffer-string *history-smoke-buffer*)))
  (text-replace-contents (buffer-text *history-smoke-buffer*) "unsaved checkpoint text")
  (setf (buffer-modified-p *history-smoke-buffer*) t))

(then 1300
  (let ((path (buffer-local *history-smoke-buffer* :checkpoint-path)))
    (check "unsaved edits are checkpointed without saving the file"
           (and path (equal "unsaved checkpoint text" (getf (cadre-ui::read-snapshot path) :text))
                (equal "new saved text" (uiop:read-file-string (buffer-file *history-smoke-buffer*))))))
  ;; Model a previous process's checkpoint, alongside the current live session.
  (let* ((path (merge-pathnames "99999999-dead/draft.sexp" (cadre-ui::snapshot-directory :recovery)))
         (data (list :version 1 :name "history-smoke.lisp" :file (namestring (buffer-file *history-smoke-buffer*))
                     :time (cadre-ui::snapshot-time) :text "edits from exited process")))
    (cadre-ui::write-snapshot path data)
    (setf *history-smoke-entry* (cons path data)))
  (call-command 'cadre-ui::recover-buffers)
  (let ((picker (cadre-ui::window-picker *window*)))
    (check "recovery lists exited processes and excludes this live session"
           (and (= 1 (length (cadre-ui::picker-items picker)))
                (equal (car *history-smoke-entry*) (caar (cadre-ui::picker-items picker)))))
    (cadre-ui::close-picker picker)
    (funcall (cadre-ui::picker-on-choose picker) *history-smoke-entry*)))

(then 300
  (check "recovery offers a copy rather than overwriting the source"
         (not (gtk:widget-get-sensitive (snapshot-button "Restore to Buffer"))))
  (gobject:emit (snapshot-button "Open Copy") :clicked))

(then 400
  (setf *history-smoke-recovered* (view-buffer (cadre-ui::selected-view *window*)))
  (check "recovered text opens as an unsaved buffer without a file"
         (and (null (buffer-file *history-smoke-recovered*)) (buffer-modified-p *history-smoke-recovered*)
              (equal "edits from exited process" (buffer-string *history-smoke-recovered*))))
  (check "the newer disk file survives recovery"
         (equal "new saved text" (uiop:read-file-string (buffer-file *history-smoke-buffer*))))
  (check "a recovered copy has its own checkpoint before the old one is removed"
         (and (probe-file (buffer-local *history-smoke-recovered* :checkpoint-path))
              (not (probe-file (car *history-smoke-entry*)))))
  (dolist (buffer (list *history-smoke-recovered* *history-smoke-buffer*))
    (setf (buffer-modified-p buffer) nil)
    (dolist (view (cadre-ui::buffer-views *window* buffer)) (cadre-ui::close-view *window* view))))

;;; Recent process runs, retained output and controls.
(section "runs")
(defvar *smoke-first-run* nil)
(defvar *smoke-failed-run* nil)

(then 50
  (with-open-file (out (merge-pathnames "run-location.lisp" *root*) :direction :output :if-exists :supersede)
    (format out "; diagnostic target~%(+ 1 2)~%"))
  (cadre-ui::run-project-process *root*
                               '(:program "/bin/sh" :arguments ("-c" "printf 'first output\\n'")
                                 :description "successful fixture") :what "Smoke success")
  (setf *smoke-first-run* (getf cadre-ui::*build* :run)))

(then-when ((null cadre-ui::*build*) :timeout 10)
  (check "a successful process is retained in Runs" (eq :passed (cadre-ui::pr-status *smoke-first-run*)))
  (check "process runs open the Runs page" (equal "runs" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (check "the completed run has output and timing"
         (and (search "first output" (cadre-ui::project-run-output-text *smoke-first-run*))
              (cadre-ui::pr-end *smoke-first-run*)))
  (cadre-ui::run-project-process *root*
                               '(:program "/bin/sh" :arguments ("-c" "printf 'run-location.lisp:2:1: error fixture\\nother line\\n'; exit 7")
                                 :description "failing fixture") :what "Smoke failure")
  (setf *smoke-failed-run* (getf cadre-ui::*build* :run)))

(then-when ((null cadre-ui::*build*) :timeout 10)
  (check "Runs records a failure and its exit code"
         (and (eq :failed (cadre-ui::pr-status *smoke-failed-run*)) (= 7 (cadre-ui::pr-exit-code *smoke-failed-run*))))
  (check "the previous run keeps its own output"
         (not (search "error fixture" (cadre-ui::project-run-output-text *smoke-first-run*))))
  (let ((page cadre-ui::*runs-page*))
    (let* ((holder (cdr (assoc "runs" (cadre-ui::panel-holders (cadre-ui::window-panel *window*)) :test #'string=)))
           (minimum (gtk:widget-measure holder :horizontal -1)))
      (check "Runs fits a narrow panel" (< minimum 320) minimum))
    (check "completed runs offer Rerun and disable Stop"
           (and (gtk:widget-get-sensitive (cadre-ui::rnp-rerun page))
                (not (gtk:widget-get-sensitive (cadre-ui::rnp-stop page)))))
    (check "diagnostics offer clickable file locations" (plusp (length (find-widgets (cadre-ui::rnp-locations page)
                                                                                  (lambda (w) (typep w 'gtk:button))))))
    (gobject:emit (first (find-widgets (cadre-ui::rnp-locations page) (lambda (w) (typep w 'gtk:button)))) :clicked)
    (gtk:editable-set-text (cadre-ui::rnp-search page) "ERROR")))

(then 400
  (check "a diagnostic opens its file and line"
         (and (equal (buffer-file (current-buffer)) (merge-pathnames "run-location.lisp" *root*))
              (= 1 (first (cursor)))))
  (let* ((page cadre-ui::*runs-page*)
         (text (text-string (gtk:text-view-get-buffer (cadre-ui::rnp-output page)))))
    (check "Runs filters output without changing retained logs"
           (and (search "error fixture" text) (not (search "other line" text))
                (search "other line" (cadre-ui::project-run-output-text *smoke-failed-run*))))
    (gtk:editable-set-text (cadre-ui::rnp-search page) "")
    (screenshot "runs-failure")
    (gobject:emit (cadre-ui::rnp-rerun page) :clicked)))

(then-when ((null cadre-ui::*build*) :timeout 10)
  (check "Rerun creates a separate history entry with the original command"
         (let ((run (first cadre-ui::*project-runs*)))
           (and (not (eq run *smoke-failed-run*))
                (equal (cadre-ui::pr-plan run) (cadre-ui::pr-plan *smoke-failed-run*))
                (eq :failed (cadre-ui::pr-status run)))))
  (cadre-ui::run-project-process *root* '(:program "/bin/sh" :arguments ("-c" "exec sleep 30")
                                        :description "stoppable fixture") :what "Smoke stop"))

(then 200
  (check "Runs enables Stop while the selected process runs"
         (gtk:widget-get-sensitive (cadre-ui::rnp-stop cadre-ui::*runs-page*)))
  (gobject:emit (cadre-ui::rnp-stop cadre-ui::*runs-page*) :clicked))

(then-when ((null cadre-ui::*build*) :timeout 10)
  (check "stopping records Stopped rather than Failed"
         (eq :stopped (cadre-ui::pr-status (first cadre-ui::*project-runs*))))
  (handler-case
      (cadre-ui::run-project-process *root* '(:program "/no-such-cadre-program" :arguments nil
                                            :description "missing executable") :what "Smoke launch failure")
    (editor-error () nil))
  (check "a launch error is retained as a failed run"
         (and (null cadre-ui::*build*)
              (eq :failed (cadre-ui::pr-status (first cadre-ui::*project-runs*)))
              (search "Could not run" (cadre-ui::project-run-output-text (first cadre-ui::*project-runs*)))))
  (check "finished runs are saved for the next session"
         (let ((titles (mapcar #'cadre-ui::pr-title (cadre-ui::read-project-runs))))
           (and (member "Smoke success" titles :test #'string=)
                (member "Smoke failure" titles :test #'string=)))
         (mapcar #'cadre-ui::pr-title (cadre-ui::read-project-runs)))
  ;; A run of another project, so the filter has a choice to make.
  (let ((other (cadre-ui::begin-project-run #P"/tmp/cadre-smoke-other-project/" '(:description "elsewhere") "Other project")))
    (cadre-ui::finish-project-run other 0))
  (let* ((page cadre-ui::*runs-page*)
         (roots (cadre-ui::rnp-project-roots page))
         (index (position (cadre-ui::project-root-key *root*) roots
                          :key (lambda (r) (and r (cadre-ui::project-root-key r))) :test #'equal)))
    (check "the project filter offers each project" (and index (> (length roots) 2)) roots)
    (gtk:drop-down-set-selected (cadre-ui::rnp-project-filter page) index)))

(then 100
  (let* ((page cadre-ui::*runs-page*)
         (rows (find-widgets (cadre-ui::rnp-list page) (lambda (w) (typep w 'gtk:list-box-row)))))
    (check "choosing a project lists only its runs"
           (and rows (= (length rows) (length (cadre-ui::visible-project-runs *root*)))
                (< (length rows) (length cadre-ui::*project-runs*))
                (every (lambda (run) (equal (cadre-ui::project-root-key *root*) (cadre-ui::project-root-key (cadre-ui::pr-root run))))
                       (cadre-ui::visible-project-runs *root*)))
           (length rows))
    (screenshot "runs-project-filter")
    (gtk:drop-down-set-selected (cadre-ui::rnp-project-filter page) 0)
    (check "Clear is offered while there are finished runs" (gtk:widget-get-sensitive (cadre-ui::rnp-clear page)))
    ;; Clearing the other project's runs (the button asks first; this is what it does).
    (cadre-ui::clear-project-runs #P"/tmp/cadre-smoke-other-project/")
    (check "clearing a project forgets its runs, here and for later sessions"
           (and (null (cadre-ui::visible-project-runs #P"/tmp/cadre-smoke-other-project/"))
                (not (member "Other project" (mapcar #'cadre-ui::pr-title (cadre-ui::read-project-runs)) :test #'string=))
                (cadre-ui::visible-project-runs *root*)
                (= 2 (length (cadre-ui::rnp-project-roots page))))
           (cadre-ui::rnp-project-roots page))))

;;; Build Project
(section "build" :needs ("new-project"))

(defun status-text () (gtk:label-get-text (cadre-ui::window-status-message *window*)))

(then 300
  (call-command 'cadre-ui::build-project))

(then 300
  (check "a library has nothing to build" (search "Nothing to build" (status-text)) (status-text))
  (cadre-ui::make-new-project *new-parent* "smoke-app" :kind :application :tests :parachute :license "MIT"))

(then-when ((equal (truename (cadre-ui::window-project *window*)) (truename (merge-pathnames "smoke-app/" *new-parent*))))
  (call-command 'cadre-ui::build-project)
  (check "Build Project starts the build" cadre-ui::*build*)
  (check "and shows its output" (equal "runs" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*)))))

(then-when ((null cadre-ui::*build*) :timeout 240)
  (let ((program (merge-pathnames "smoke-app/bin/smoke-app" *new-parent*)))
    (check "the build makes the program" (probe-file program) (status-text))
    (check "and says so" (search "Built " (status-text)) (status-text))
    (check "the program runs"
           (and (probe-file program)
                (string= "Hello, Cadre!" (string-trim '(#\Newline)
                                                      (uiop:run-program (list (uiop:native-namestring program) "Cadre")
                                                                        :output :string :ignore-error-status t))))))
  (check "the build's output is on the Output page"
         (search "Build in " (cadre-ui::panel-output-string (cadre-ui::window-panel *window*))))
  (screenshot "40-build"))

;;; Run Tests
(section "run-tests" :needs ("build"))

(defun app-file (path) (merge-pathnames (concatenate 'string "smoke-app/" path) *new-parent*))

;; Quicklisp knows where Parachute is.
(load-quicklisp)

(then 300
  (call-command 'cadre-ui::run-tests))

(then-when ((let ((page cadre-ui::*tests-page*))
              (and (cadre-ui::tsp-tests page) (not (cadre-ui::tsp-running page))
                   (every #'source-test-status (cadre-ui::tsp-tests page))))
            :timeout 120)
  (check "Run Tests runs the project's parachute tests on the Tests page"
         (equal "tests" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (check "and they pass"
         (every (lambda (test) (eq :passed (source-test-status test))) (cadre-ui::tsp-tests cadre-ui::*tests-page*))
         (mapcar (lambda (test) (list (source-test-name test) (source-test-status test) (source-test-messages test)))
                 (cadre-ui::tsp-tests cadre-ui::*tests-page*)))
  (check "with no errors along the way"
         (not (search "Error:" (subseq (cadre-ui::panel-output-string (cadre-ui::window-panel *window*))
                                       (or (search "Created smoke-app" (cadre-ui::panel-output-string (cadre-ui::window-panel *window*))) 0))))
         (cadre-ui::panel-output-string (cadre-ui::window-panel *window*)))
  (call-command 'cadre-ui::run-tests-in-new-lisp))

(then-when ((null cadre-ui::*build*) :timeout 120)
  (check "Run Tests in a New Lisp passes" (search "Tests passed" (status-text)) (status-text)))

(then 1100                              ; file times are to the second: let ASDF see the change
  ;; Break a test.
  (let ((tests (app-file "tests/main.lisp")))
    (with-open-file (o tests :direction :output :if-exists :supersede)
      (write-string (cl-ppcre:regex-replace "Hello, Lisp!" (uiop:read-file-string tests) "Hello, Nobody!") o)))
  (call-command 'cadre-ui::run-tests-in-new-lisp))

(then-when ((null cadre-ui::*build*) :timeout 120)
  (check "a failing test fails the run" (search "Test run failed" (status-text)) (status-text))
  (check "and shows the Runs page" (equal "runs" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (screenshot "41-tests"))

;;; Run GTK App
(section "gtk-app")
(load-quicklisp)
(then 50 (ensure-directories-exist *new-parent*))

(then 300
  (cadre-ui::make-new-project *new-parent* "smoke-gtk" :kind :gtk-application :tests :parachute :license nil))

(then-when ((equal (truename (cadre-ui::window-project *window*)) (truename (merge-pathnames "smoke-gtk/" *new-parent*))))
  (call-command 'cadre-ui::run-gtk-app))

(then 500
  (check "Run GTK App starts the app" (cadre-ui::gtk-app-running-p))
  (check "and the status bar says evaluation is on the GTK thread"
         (search "GTK" (gtk:button-get-label (cadre-ui::window-status-connection *window*)))
         (gtk:button-get-label (cadre-ui::window-status-connection *window*)))
  (check "REPL input is wrapped for the GTK thread"
         (and (search "glib:in-main-thread" (cadre-ui::gtk-thread-source "(foo)"))
              (string= "(in-package :foo)" (cadre-ui::gtk-thread-source "(in-package :foo)"))))
  (check "the Profiler's tab appears, to profile the app"
         (gtk:stack-page-get-visible (cadre-ui::panel-page (cadre-ui::window-panel *window*) "profiler"))))

(defun gtk-title-form (&optional marker)
  (if marker
      (format nil
              "(let* ((app cl-user::*cadre-run-gtk-application*) (window (and app (gtk:application-get-active-window app)))) (format nil ~s (and window (gtk:window-get-title window))))"
              (concatenate 'string marker ":~a"))
      "(let ((app cl-user::*cadre-run-gtk-application*)) (and app (gtk:application-get-active-window app) (gtk:window-get-title (gtk:application-get-active-window app))))"))

(then 3000
  (cadre-ui::repl-eval (gtk-title-form)))

(then-when ((search "\"smoke-gtk\"" (repl-text)) :timeout 180)
  (check "the app's window is up, and the REPL reaches it on the GTK thread" (search "\"smoke-gtk\"" (repl-text)))
  (screenshot "42-gtk-app")
  (call-command 'cadre-ui::profile-all-threads))

(then-when ((let ((p (cadre-ui::pp-profile cadre-ui::*profiler-page*))) (and p (profile-running p))) :timeout 20)
  ;; Busy work on the GTK thread, as a slow callback would be
  (cadre-ui::repl-eval "(labels ((gtk-fib (n) (if (< n 2) n (+ (gtk-fib (- n 1)) (gtk-fib (- n 2)))))) (loop repeat 300 sum (gtk-fib 25)))"))

(then 2500
  (call-command 'cadre-ui::stop-profiling))

(then-when ((let ((p (cadre-ui::pp-profile cadre-ui::*profiler-page*))) (and p (not (profile-running p)))) :timeout 20)
  (let ((profile (cadre-ui::pp-profile cadre-ui::*profiler-page*)))
    (check "Profile All Threads sees work on the GTK app's thread"
           (find-if (lambda (f) (search "GTK-FIB" (profile-function-name f))) (profile-functions profile))
           (mapcar #'profile-function-name (subseq (profile-functions profile) 0 (min 8 (length (profile-functions profile)))))))
  (screenshot "42b-gtk-app-profile")
  (cadre-ui::repl-eval "(glib:idle-add glib:+priority-default+ (lambda () (error \"callback boom\")))"))

(then-when ((find-if (lambda (d) (search "callback boom" (first (cadre-ui::dl-condition d)))) cadre-ui::*debug-levels*) :timeout 20)
  (let ((level (first cadre-ui::*debug-levels*)))
    (check "an error in a GTK callback opens the debugger" (search "callback boom" (first (cadre-ui::dl-condition level))))
    (check "with a restart that returns from the callback"
           (search "GTK callback" (second (first (cadre-ui::dl-restarts level))))
           (cadre-ui::dl-restarts level)))
  (call-command 'cadre-ui::debugger-abort))

(then-when ((null cadre-ui::*debug-levels*) :timeout 20)
  (check "Abort returns from the callback, and the app keeps running" (cadre-ui::gtk-app-running-p))
  (call-command 'cadre-ui::stop-gtk-app))

(then-when ((not (cadre-ui::gtk-app-running-p)) :timeout 30)
  (check "Stop GTK App quits it, and Cadre notices" (not (cadre-ui::gtk-app-running-p)))
  (check "evaluation is back in the REPL's thread" (string= "(foo)" (cadre-ui::gtk-thread-source "(foo)"))))

;;; Run App: the ▶ button, for a GTK app and for one that reads its input in the REPL
(section "run-app" :needs ("gtk-app"))

(defun run-button () (gethash :run-button cadre-ui::*named-widgets*))
(defvar *run-app-window-seen* nil)

(then 300
  (setf *run-app-window-seen* nil)
  (check "▶ is shown for a project with an :entry-point" (gtk:widget-get-visible (run-button)))
  (call-command 'cadre-ui::run-app))

(then-when ((cadre-ui::gtk-app-running-p) :timeout 30)
  (check "GTK apps have an active Runs entry"
         (eq :running (cadre-ui::pr-status (getf cadre-ui::*gtk-app* :run))))
  (check "▶ runs a gtk4 app as Run GTK App does" (cadre-ui::gtk-app-running-p))
  (check "and becomes a stop button" (equal "Stop the app" (gtk:widget-get-tooltip-text (run-button))))
  ;; Match the user's red close button: closing the last window should end the
  ;; application, and the Run button should start a fresh application instance.
  (cadre-ui::repl-eval (gtk-title-form "run-app-first")))

(then-when ((search "run-app-first:smoke-gtk" (repl-output-text)) :timeout 30)
  (if (search "run-app-first:smoke-gtk" (repl-output-text))
      (progn
        (setf *run-app-window-seen* t)
        (cadre-ui::repl-eval
         "(format nil \"run-app-front:~a\" (gtk:window-is-active (gtk:application-get-active-window cl-user::*cadre-run-gtk-application*)))"))
      (progn
        (check "Run App opens the GTK window before close testing" nil (repl-output-text))
        (call-command 'cadre-ui::stop-app))))

(then-when ((search "run-app-front:" (repl-output-text)) :timeout 10)
  (let ((output (repl-output-text)))
    (check "Run App brings the GTK window in front of Cadre"
           (search "run-app-front:T" output)
           output)
    (cadre-ui::repl-eval
     "(gtk:window-close (gtk:application-get-active-window cl-user::*cadre-run-gtk-application*))")))

(then-when ((not (cadre-ui::gtk-app-running-p)) :timeout 30)
  (when *run-app-window-seen*
    (check "closing the GTK window ends the app" (not (cadre-ui::gtk-app-running-p)))
    (call-command 'cadre-ui::run-app)))

(then-when ((cadre-ui::gtk-app-running-p) :timeout 30)
  (when *run-app-window-seen*
    (check "▶ starts the GTK app again after its window closes" (cadre-ui::gtk-app-running-p))
    (cadre-ui::repl-eval (gtk-title-form "run-app-restart"))))

(then-when ((search "run-app-restart:smoke-gtk" (repl-output-text)) :timeout 30)
  (if (search "run-app-restart:smoke-gtk" (repl-output-text))
      (progn
        (check "the restarted GTK app opens a fresh window" t)
        (call-command 'cadre-ui::stop-app))
      (progn
        (check "the restarted GTK app opens a fresh window" nil (repl-output-text))
        (when (cadre-ui::gtk-app-running-p) (call-command 'cadre-ui::stop-app)))))

(then-when ((not (cadre-ui::gtk-app-running-p)) :timeout 30)
  (check "stopping the GTK app marks its Runs entry stopped"
         (eq :stopped (cadre-ui::pr-status (getf cadre-ui::*gtk-app* :run))))
  (check "Stop App quits the restarted GTK app" (not (cadre-ui::gtk-app-running-p)))
  (cadre-ui::make-new-project *new-parent* "smoke-cli" :kind :application :tests :parachute :license nil))

(then-when ((equal (truename (cadre-ui::window-project *window*)) (truename (merge-pathnames "smoke-cli/" *new-parent*))))
  (with-open-file (out (merge-pathnames "smoke-cli/src/main.lisp" *new-parent*) :direction :output :if-exists :supersede)
    (format out "(in-package #:smoke-cli)~%(defun hello (&optional (who \"World\")) (format nil \"Hello, ~~a!\" who))~%(defun main ()~%  (format t \"Your name? \") (finish-output)~%  (write-line (hello (read-line))))~%"))
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 120)
  (check "▶ runs a console app in the REPL, which asks for its input" (search "Your name?" (repl-text)) (repl-text))
  (cadre-ui::set-repl-input "Pat")
  (call-command 'cadre-ui::repl-return))

(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (check "console app output and successful completion are retained in Runs"
         (let ((run (first cadre-ui::*project-runs*)))
           (and (eq :passed (cadre-ui::pr-status run))
                (search "Hello, Pat!" (cadre-ui::project-run-output-text run)))))
  (check "RET sends the line, and the app's output follows" (search "Hello, Pat!" (repl-text)) (repl-text))
  (check "▶ is back" (equal "Run smoke-cli:main (in the REPL)" (gtk:widget-get-tooltip-text (run-button)))
         (gtk:widget-get-tooltip-text (run-button)))
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 30)
  (call-command 'cadre-ui::stop-app))

(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (check "stopping a console app marks its Runs entry stopped"
         (eq :stopped (cadre-ui::pr-status (first cadre-ui::*project-runs*))))
  (check "Stop App aborts a console app waiting for input" (null cadre-ui::*console-app*))
  (check "and the REPL takes forms again" (not (cadre-ui::repl-busy cadre-ui::*repl*))))

(then-when ((search "Stopped" (status-text)) :timeout 10)
  (check "the status bar names what stopped" (search "Stopped smoke-cli:main" (status-text)) (status-text)))

;; Run App compiles first: what it recompiles gets its notes on the Problems page.
(defvar *notes-before* nil)

(defun cli-main-notes ()
  (remove-if-not (lambda (s) (search "smoke-cli/src/main.lisp" (namestring (or (cadre-ui::sn-file s) ""))))
                 cadre-ui::*notes*))

(defun write-cli-main (helper)
  (with-open-file (out (merge-pathnames "smoke-cli/src/main.lisp" *new-parent*) :direction :output :if-exists :supersede)
    (format out "(in-package #:smoke-cli)~%~a~%(defun hello (&optional (who \"World\")) (format nil \"Hello, ~~a!\" who))~%(defun main ()~%  (format t \"Your name? \") (finish-output)~%  (write-line (hello (read-line))))~%"
            helper)))

(then 1100                               ; a new second: ASDF's file dates are in seconds
  (write-cli-main "(defun initial (s) (let ((unused 1)) (char s 0)))")
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 120)
  (check "▶ compiles first: a style warning goes to the Problems page" (cli-main-notes)
         (format nil "~s ~s" (status-text)
                 (mapcar (lambda (s) (cadre::compiler-note-message (cadre-ui::sn-note s))) cadre-ui::*notes*)))
  (check "and the app still runs" cadre-ui::*console-app*)
  (call-command 'cadre-ui::stop-app))

(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s 0))")
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 120)
  (check "fixed and run again, the note is gone" (null (cli-main-notes))
         (mapcar (lambda (s) (cadre::compiler-note-message (cadre-ui::sn-note s))) (cli-main-notes)))
  (call-command 'cadre-ui::stop-app))

(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s))")
  (call-command 'cadre-ui::run-app))

(then-when ((search "Compiled smoke-cli failed" (status-text)) :timeout 60)
  (check "a full warning fails the compile (as ASDF has it on SBCL)" (search "warning" (status-text)) (status-text))
  (check "so the app doesn't start" (null cadre-ui::*console-app*))
  (check "and the warning is on the Problems page" (cli-main-notes))
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s 0)")
  (call-command 'cadre-ui::run-app))

(then-when ((and (search "Compiled smoke-cli failed" (status-text)) (search "error" (status-text))
                 (notany (lambda (s) (search "CHAR" (cadre::compiler-note-message (cadre-ui::sn-note s))))
                         (cli-main-notes)))
            :timeout 60)
  (check "with a compile error, the app doesn't start" (null cadre-ui::*console-app*))
  (check "and the error replaces the old warning on the Problems page" (cli-main-notes)
         (mapcar (lambda (s) (cadre::compiler-note-message (cadre-ui::sn-note s))) (cli-main-notes)))
  (setf *notes-before* (length cadre-ui::*notes*))
  (cadre-ui::message "Running it again")
  (call-command 'cadre-ui::run-app))

(then-when ((search "Compiled smoke-cli failed" (status-text)) :timeout 60)
  ;; ASDF's "COMPILE-FILE-ERROR while compiling …" is in no file: it used to pile up.
  (check "failing again replaces the errors, not adds to them" (= *notes-before* (length cadre-ui::*notes*))
         (list *notes-before* (length cadre-ui::*notes*)))
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s 0))")
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 120)
  (check "and once it compiles, no error is left, in a file or not"
         (notany (lambda (s) (eql 0 (cadre::severity-rank (cadre::compiler-note-severity (cadre-ui::sn-note s)))))
                 cadre-ui::*notes*)
         (mapcar (lambda (s) (cadre::compiler-note-message (cadre-ui::sn-note s))) cadre-ui::*notes*))
  (call-command 'cadre-ui::stop-app))

;; Load (the Systems sidebar's button, Load Project) compiles first too.
(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s 0)")
  (cadre-ui::message "Loading")
  (cadre-ui::load-system-in-repl (merge-pathnames "smoke-cli/" *new-parent*) "smoke-cli"))

(then-when ((search "Compiled smoke-cli failed" (status-text)) :timeout 60)
  (check "Load puts the compile's errors on the Problems page" (cli-main-notes))
  (sleep 1.1)
  (write-cli-main "(defun initial (s) (char s 0))")
  (cadre-ui::message "Loading")
  (cadre-ui::load-system-in-repl (merge-pathnames "smoke-cli/" *new-parent*) "smoke-cli"))

(then-when ((and (search "Compiled smoke-cli" (status-text)) (not (search "failed" (status-text)))) :timeout 60)
  (check "and loading it fixed clears them"
         (notany (lambda (s) (eql 0 (cadre::severity-rank (cadre::compiler-note-severity (cadre-ui::sn-note s)))))
                 cadre-ui::*notes*)
         (mapcar (lambda (s) (cadre::compiler-note-message (cadre-ui::sn-note s))) cadre-ui::*notes*)))

(then-when ((not (cadre-ui::repl-busy cadre-ui::*repl*)) :timeout 60))

;;; Pictures open in a tab, to be looked at
(section "image-view")

(defun write-test-png (path width height rgba)
  (let ((pixbuf (gdk-pixbuf:pixbuf-new :rgb t 8 width height)))
    (gdk-pixbuf:pixbuf-fill pixbuf rgba)
    (gdk-pixbuf:pixbuf-savev pixbuf (uiop:native-namestring path) "png" nil nil)))

(defvar *test-png* (merge-pathnames "knight.png" *root*))
(defun image-text () (buffer-string (current-buffer)))

(then 100
  (write-test-png *test-png* 16 28 #xc0404080)
  (open-file-path *test-png*))

(then 500
  (let ((buffer (current-buffer)))
    (check "a .png opens in Image mode" (eq 'image-mode (buffer-major-mode buffer)) (buffer-major-mode buffer))
    (check "with no file to save" (null (buffer-file buffer)))
    (check "showing the picture"
           (gtk:text-iter-get-paintable (gtk:text-buffer-get-start-iter (buffer-text buffer))))
    (check "a small one scaled up, with its size" (search "16 × 28 pixels · 900%" (image-text)) (image-text))
    (check "read-only" (not (gtk:text-view-get-editable (cadre-ui::view-text-view (current-view)))))
    (check "and not modified" (not (cadre-ui::buffer-needs-saving-p buffer)))
    (check "the session remembers it"
           (equal (list :image (uiop:native-namestring *test-png*)) (cadre-ui::view-state (current-view)))
           (cadre-ui::view-state (current-view))))
  (screenshot "image-view")
  (press "-"))

(then 200
  (check "- zooms out" (search "· 800%" (image-text)) (image-text))
  (press "C-0"))

(then 200
  (check "C-0 shows it at its own size" (search "· 100%" (image-text)) (image-text))
  (press "C-="))

(then 200
  (check "C-= zooms in" (search "· 200%" (image-text)) (image-text))
  (press "S-g"))

(then 200
  (let ((text (gtk:editable-get-text (cadre-ui::picker-entry (picker)))))
    (check "Shift+G asks for the grid's cells" (equal "16x16" text) text))
  (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "8x14")
  (cadre-ui::choose (picker)))

(then 200
  (check "the grid is drawn, and the line says so" (search "grid 8x14" (image-text)) (image-text))
  (press "+") (press "+"))

(then 300
  (screenshot "image-grid")
  (press "-") (press "-")
  (check "grids parse" (equal '((16 16 0 0) (16 28 128 100) nil)
                              (mapcar #'cadre-ui::parse-image-grid '("16" "16x28+128+100" "x"))))
  (press "g"))

(then 200
  (check "g hides it" (not (search "grid" (image-text))) (image-text))
  (check "the status bar gives a pixel's place and color"
         (equal "x 3, y 5 · #c0404080" (cadre-ui::image-status-text (current-buffer) '(3 5)))
         (cadre-ui::image-status-text (current-buffer) '(3 5)))
  (check "and the picture's size otherwise"
         (equal "16 × 28" (gtk:label-get-text (cadre-ui::window-status-position *window*)))
         (gtk:label-get-text (cadre-ui::window-status-position *window*)))
  (let* ((view (current-view))
         (text-view (cadre-ui::view-text-view view))
         (rect (gtk:text-view-get-iter-location text-view (gtk:text-buffer-get-start-iter (cadre-ui::view-gtk-buffer view)))))
    ;; Pixel 3, 5 at 200%: 7, 11 into the picture, in widget coordinates.
    (multiple-value-bind (wx wy) (gtk:text-view-buffer-to-window-coords text-view :widget
                                                                        (+ (gdk:rectangle-x rect) 7)
                                                                        (+ (gdk:rectangle-y rect) 11))
      (check "the pointer's place maps to the picture's pixel"
             (equal '(3 5) (cadre-ui::image-pixel-at view wx wy))
             (cadre-ui::image-pixel-at view wx wy))))
  (let ((count (length (buffer-list))))
    (open-file-path *test-png*)
    (check "opening it again selects its tab" (= count (length (buffer-list)))))
  (write-test-png *test-png* 32 32 #x40c040ff))

(then-when ((search "32 × 32" (image-text)) :timeout 10)
  (check "it is shown again when the file changes" (search "32 × 32" (image-text)) (image-text))
  (call-command 'cadre-ui::close-tab))

(then 300
  (check "closing the tab lets the picture go" (null (cadre-ui::find-image-buffer *test-png*))))

;;; Run App for a raylib game: rl:run returns at once, so the app runs until
;;; rl:running-p says the game ended, and Stop App calls rl:stop.
(section "run-raylib-app")

(defvar *raylib-installed* (and (ql:where-is-system "raylib") t))

(defun raylib-stop-tooltip-p () (equal "Stop the app" (gtk:widget-get-tooltip-text (run-button))))

(if (not *raylib-installed*)
    (then 50 (format t "~&(raylib isn't installed: skipping Run App for a raylib game)~%"))
    (progn
      (load-quicklisp)
      (then 50
        (ensure-directories-exist *new-parent*)
        (cadre-ui::make-new-project *new-parent* "smoke-raylib" :kind :application :tests :parachute :license nil))
      (then-when ((equal (truename (cadre-ui::window-project *window*)) (truename (merge-pathnames "smoke-raylib/" *new-parent*))))
        (let* ((asd (merge-pathnames "smoke-raylib/smoke-raylib.asd" *new-parent*))
               (text (uiop:read-file-string asd)))
          (with-open-file (out asd :direction :output :if-exists :supersede)
            (write-string (ppcre:regex-replace ":depends-on \\(\\)" text ":depends-on (:raylib)") out)))
        (with-open-file (out (merge-pathnames "smoke-raylib/src/main.lisp" *new-parent*) :direction :output :if-exists :supersede)
          (format out "(in-package #:smoke-raylib)~%(defun game () (rl:with-window (200 100 \"smoke\") (rl:game-loop () (rl:with-drawing (:black)))))~%(defun main () (rl:run 'game))~%"))
        (check "an app depending on raylib is one" (getf (cadre::project-app (cadre-ui::window-project *window*)) :raylib))
        (call-command 'cadre-ui::run-app))
      (then-when ((getf cadre-ui::*console-app* :game) :timeout 180)
        (check "▶ runs a raylib game, and the entry point returns" (getf cadre-ui::*console-app* :game)))
      (then 1500
        (check "the app runs on after the entry point returned" (getf cadre-ui::*console-app* :game))
        (check "■ stays while the game runs" (raylib-stop-tooltip-p) (gtk:widget-get-tooltip-text (run-button)))
        (check "and the REPL is free" (not (cadre-ui::repl-busy cadre-ui::*repl*)))
        (call-command 'cadre-ui::stop-app))
      (then-when ((null cadre-ui::*console-app*) :timeout 30)
        (check "Stop App ends the game with rl:stop, and Cadre notices" (null cadre-ui::*console-app*))
        (check "▶ is back" (not (raylib-stop-tooltip-p)) (gtk:widget-get-tooltip-text (run-button))))))

;;; The main menu: a short list of submenus, each item a command
(section "main-menu")

(defun menu-entries (model)
  "(label . target) for each item under MODEL, through its sections and submenus,
and the number of items MODEL itself shows."
  (let ((entries '()) (shown 0))
    (labels ((walk (model top)
               (dotimes (i (gio:menu-model-get-n-items model))
                 (let ((section (gio:menu-model-get-item-link model i "section"))
                       (submenu (gio:menu-model-get-item-link model i "submenu")))
                   (cond (section (walk section top))
                         (t (when top (incf shown))
                            (if submenu
                                (walk submenu nil)
                                (let ((label (gio:menu-model-get-item-attribute-value model i "label" nil))
                                      (target (gio:menu-model-get-item-attribute-value model i "target" nil)))
                                  (push (cons (glib:variant-get-string label)
                                              (glib:variant-get-string target))
                                        entries)))))))))
      (walk model t))
    (values (nreverse entries) shown)))

(then 100
  (let ((button (gethash :main-menu cadre-ui::*named-widgets*)))
    (multiple-value-bind (entries shown) (menu-entries (gtk:menu-button-get-menu-model button))
      (check "the main menu shows a dozen entries, not every command" (<= shown 12) shown)
      (check "every menu item is a command"
             (every (lambda (e) (cadre:find-command (find-symbol (string-upcase (cdr e)) :cadre-ui))) entries)
             (remove-if (lambda (e) (cadre:find-command (find-symbol (string-upcase (cdr e)) :cadre-ui))) entries))
      (check "and Run App is in it" (find "run-app" entries :key #'cdr :test #'string=)))
    (gtk:menu-button-popup button)))

(then 400
  (screenshot "44-main-menu" (gtk:menu-button-get-popover (gethash :main-menu cadre-ui::*named-widgets*))))

(then 100
  (gtk:menu-button-popdown (gethash :main-menu cadre-ui::*named-widgets*)))

;;; Racket and Clojure: editing with no Lisp running (docs/RACKET-CLOJURE.md, R1)
(section "lisp-dialects")

(defun write-root-file (name text)
  (let ((path (merge-pathnames name *root*)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    path))

(defun key-binding (keys)
  (lookup-keys (cadre-ui::view-keymaps (current-view)) (parse-keys keys)))

(then 300
  (write-root-file "dialects/shapes.rkt"
                   (format nil "#lang racket/base~%(define (area w h)~%  (* w h))~%#;(old-area 1 2)~%(struct point (x y))~%"))
  (write-root-file "dialects/shapes.clj"
                   (format nil "(ns shapes.core)~%(defn area [w h]~%  (* w h))~%#_(old-area 1 2)~%(def ^:private origin {:x 0, :y 0})~%"))
  (open-file-path (merge-pathnames "dialects/shapes.rkt" *root*)))

(then-when ((and (find-buffer "shapes.rkt") (eq (current-buffer) (find-buffer "shapes.rkt"))))
  (check "a .rkt file opens in Racket mode" (eq 'racket-mode (buffer-major-mode (current-buffer)))))

(then 600
  (check "Racket: define is a definer and area its name"
         (and (has-face-p 1 1 :definer) (has-face-p 1 9 :definition-name)))
  (check "#lang is reader syntax" (has-face-p 0 1 :quote))
  (check "what #; comments out looks like a comment" (and (has-face-p 3 0 :comment) (has-face-p 3 5 :comment)))
  (check "the Racket outline lists area and point"
         (equal '(("area" :function 1 0) ("point" :class 4 0)) (cadre-ui::outline-items (current-buffer)))
         (cadre-ui::outline-items (current-buffer)))
  (check "C-M-f moves over forms in Racket too" (eq 'cadre-ui::forward-sexp (key-binding "C-M-f")))
  (check "C-RET evaluates, and F5 runs the file, in Racket"
         (and (eq 'cadre-ui::compile-or-eval-defun (key-binding "C-RET"))
              (eq 'cadre-ui::racket-run (key-binding "F5")))
         (list (key-binding "C-RET") (key-binding "F5")))
  (screenshot "47-racket")
  (set-minor-mode (current-buffer) 'cadre-ui::paredit-mode t)
  (set-cursor 5 0)
  (insert-at-cursor "(let (")
  (press "["))

(then 300
  (check "[ makes a pair of brackets in Racket" (string= "(let ([]" (line-text 5)) (line-text 5))
  (check "with the cursor between them" (equal '(5 7) (cursor)) (cursor))
  (let ((text (buffer-text (current-buffer))))
    (gtk:text-buffer-insert text (cadre-ui::line-iter text 2 0) "    " -1))
  (set-cursor 2 0)
  (press "TAB"))

(then 300
  (check "TAB indents a Racket body by 2" (string= "  (* w h))" (line-text 2)) (line-text 2))
  (open-file-path (merge-pathnames "dialects/shapes.clj" *root*)))

(then-when ((and (find-buffer "shapes.clj") (eq (current-buffer) (find-buffer "shapes.clj"))))
  (check "a .clj file opens in Clojure mode" (eq 'clojure-mode (buffer-major-mode (current-buffer)))))

(then 600
  (check "Clojure: defn is a definer and area its name"
         (and (has-face-p 1 1 :definer) (has-face-p 1 6 :definition-name)))
  (check "metadata doesn't hide a def's name" (has-face-p 4 16 :definition-name))
  (check "keywords are keywords" (has-face-p 4 24 :keyword))
  (check "what #_ comments out looks like a comment" (has-face-p 3 5 :comment))
  (check "the Clojure outline lists the namespace, area and origin"
         (equal '(("shapes.core" :namespace 0 0) ("area" :function 1 0) ("origin" :variable 4 0))
                (cadre-ui::outline-items (current-buffer)))
         (cadre-ui::outline-items (current-buffer)))
  (screenshot "48-clojure")
  (set-cursor 2 4)
  (call-command 'cadre-ui::toggle-fold))

(then 300
  (check "a Clojure defn folds" (equal '(1) (fold-lines-now)) (fold-lines-now))
  (call-command 'cadre-ui::unfold-all)
  (set-cursor 2 2)
  (call-command 'cadre-ui::toggle-comment))

(then 300
  (check "Toggle Comment uses ;; in Clojure" (string= "  ;; (* w h))" (line-text 2)) (line-text 2))
  (call-command 'cadre-ui::toggle-comment)
  (set-cursor 1 0)
  (press "C-M-f"))

(then 300
  (check "C-M-f moves over a Clojure form with [ ] in it" (equal '(2 10) (cursor)) (cursor)))

;;; Clojure through nREPL (docs/RACKET-CLOJURE.md, R3). Needs the Clojure CLI;
;;; the first run downloads nREPL and cider-nrepl.
;; Scribble: prose in @-expressions' bodies is text, not code
(then 300
  (write-root-file "dialects/doc.scrbl"
                   (format nil "#lang scribble/manual~%@title{Don't \"quote\" me}~%@racket[(define x 1)]~%"))
  (open-file-path (merge-pathnames "dialects/doc.scrbl" *root*)))

(then-when ((and (find-buffer "doc.scrbl") (eq (current-buffer) (find-buffer "doc.scrbl"))))
  (check "a .scrbl file is lexed with @-expressions"
         (eq cadre:*scribble-dialect* (cadre:syntax-dialect (cadre-ui::buffer-sexp-syntax (current-buffer))))))

(then 600
  (check "prose with a quote in it isn't drawn as a string" (not (has-face-p 1 15 :string)))
  (check "and code in @racket[…] is still code" (has-face-p 2 10 :definer)))

(section "clojure")
(use-root-project)

;; clj-kondo on save, with a stand-in that reports one finding.
(then 300
  (write-root-file "kondo/fake-clj-kondo"
                   (format nil "#!/bin/sh~%cat > /dev/null~%echo '<stdin>:1:2: warning: fake finding'~%"))
  (sb-posix:chmod (namestring (merge-pathnames "kondo/fake-clj-kondo" *root*)) #o755)
  (write-root-file "kondo/lint.clj" (format nil "(ns lint)~%(def x 1)~%"))
  (write-root-file "kondo/.cljfmt.edn" "{:indents {smoke-block [[:block 1]]}}")
  (setf cadre:*clj-kondo-command* (namestring (merge-pathnames "kondo/fake-clj-kondo" *root*))
        cadre-ui::*clj-kondo-found* :unknown)
  (open-file-path (merge-pathnames "kondo/lint.clj" *root*)))

(then 500
  (let ((text (buffer-text (current-buffer))))
    (gtk:text-buffer-insert text (gtk:text-buffer-get-end-iter text) (format nil "(def y 2)~%") -1))
  (call-command 'cadre-ui::save-buffer))

(then-when ((find "fake finding" cadre-ui::*notes* :key (lambda (s) (compiler-note-message (cadre-ui::sn-note s)))
                  :test #'string=)
            :timeout 20)
  (check "opening a Clojure file learns the project's .cljfmt.edn indentation"
         (gethash "smoke-block" cadre:*clojure-indent-rules*))
  (check "saving a Clojure file shows clj-kondo's findings in Problems"
         (find "fake finding" cadre-ui::*notes* :key (lambda (s) (compiler-note-message (cadre-ui::sn-note s)))
               :test #'string=))
  (setf cadre:*clj-kondo-command* "clj-kondo" cadre-ui::*clj-kondo-found* :unknown))

(defvar *have-clojure* (ignore-errors (uiop:run-program '("clojure" "-Sdescribe") :output nil) t))

(then 300
  (unless *have-clojure* (format t "~&(the Clojure CLI isn't installed: skipping the Clojure checks)~%"))
  (when *have-clojure*
    (write-root-file "clj-demo/deps.edn" "{:paths [\"src\"]}")
    (write-root-file "clj-demo/src/demo/core.clj"
                     (format nil "(ns demo.core)~%~%(defn area [w h]~%  (* w h))~%(area 3 4)~%"))
    (open-file-path (merge-pathnames "clj-demo/src/demo/core.clj" *root*))))

;; By its file: the outline section's pkg/core.clj is open too, by the same name.
(then-when ((or (not *have-clojure*)
                (let ((buffer (find-file-buffer (merge-pathnames "clj-demo/src/demo/core.clj" *root*))))
                  (and buffer (eq (current-buffer) buffer)))))
  (when *have-clojure*
    (check "a .clj file in a deps.edn project opens in Clojure mode" (eq 'clojure-mode (buffer-major-mode (current-buffer))))
    (set-cursor 2 1)
    (call-command 'compile-or-eval-defun)))

(then-when ((or (not *have-clojure*) (cadre-ui::clojure-connection)) :timeout 300)
  (when *have-clojure*
    (check "compiling a Clojure form starts the project's REPL" (cadre-ui::clojure-connection))
    (check "the status bar shows Clojure"
           (search "Clojure" (gtk:button-get-label (cadre-ui::window-status-connection *window*)))
           (gtk:button-get-label (cadre-ui::window-status-connection *window*)))))

(then-when ((or (not *have-clojure*) (search "Compiled" (status-text))) :timeout 60)
  (when *have-clojure*
    (check "the defn compiles" (search "Compiled" (status-text)) (status-text))
    (set-cursor 4 3)
    (call-command 'compile-or-eval-defun)))

(then-when ((or (not *have-clojure*) (cadre-ui::buffer-local (current-buffer) :inline-result)) :timeout 60)
  (when *have-clojure*
    (let ((shown (cadre-ui::buffer-local (current-buffer) :inline-result)))
      (check "evaluating a call shows its value inline, in the file's namespace"
             (and shown (search "⇒ 12" (gtk:label-get-text (cdr shown))))
             (and shown (gtk:label-get-text (cdr shown)))))
    (screenshot "49-clojure-eval")
    (set-cursor 4 8)                    ; (area 3 |4)
    (cadre-ui::focus-view (current-view))
    (cadre-ui::request-autodoc (current-view))))

(then-when ((or (not *have-clojure*) (search "area" (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))) :timeout 30)
  (when *have-clojure*
    (check "the status bar shows the arglist, with the argument at the cursor"
           (search "(area [w h])" (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
           (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
    (set-cursor 4 2)
    (call-command 'edit-definition)))

(then 1500
  (when *have-clojure*
    (check "M-. goes to the definition, from cider-nrepl" (equal '(2 0) (cursor)) (cursor))
    (set-cursor 4 2)
    (call-command 'describe-symbol)))

(then-when ((or (not *have-clojure*) (find-buffer "*Help*")) :timeout 30)
  (when *have-clojure*
    (check "describing shows the qualified name and arglist"
           (search "demo.core/area" (buffer-string (find-buffer "*Help*")))
           (buffer-string (find-buffer "*Help*")))
    (open-file-path (merge-pathnames "clj-demo/src/demo/core.clj" *root*))))

(then 500
  (when *have-clojure*
    (let ((text (buffer-text (current-buffer))))
      (gtk:text-buffer-insert text (gtk:text-buffer-get-end-iter text) (format nil "(defn bad [] (nope))~%") -1))
    (set-cursor 5 1)
    (call-command 'compile-defun)))

(then-when ((or (not *have-clojure*) (search "failed" (status-text))) :timeout 60)
  (when *have-clojure*
    (check "a compile error fails the compile, with a note" (search "1 error" (status-text)) (status-text))
    (call-command 'cadre-ui::clojure-repl-tab)))

(then 800
  (when *have-clojure*
    (check "the Clojure REPL is on the REPL page, titled for Clojure"
           (and (eq 'cadre-ui::clojure-repl-mode (buffer-major-mode (current-buffer)))
                (equal "clojure" (gtk:stack-get-visible-child-name cadre-ui::*repl-stack*))
                (equal "REPL · Clojure" (panel-page-title "repl")))
           (panel-page-title "repl"))
    (cadre-ui::with-buffer-repl () (cadre-ui::set-repl-input "(demo.core/area 6 7)"))
    (call-command 'cadre-ui::repl-return)))

(then-when ((or (not *have-clojure*) (search "42" (buffer-string (current-buffer)))) :timeout 30)
  (when *have-clojure*
    (check "the Clojure REPL evaluates what is typed" (search "42" (buffer-string (current-buffer)))
           (buffer-string (current-buffer)))
    (screenshot "50-clojure-repl")
    (cadre-ui::with-buffer-repl () (cadre-ui::set-repl-input "(/ 1 0)"))
    (call-command 'cadre-ui::repl-return)))

(then-when ((or (not *have-clojure*) (cadre-ui::post-mortem-p (first cadre-ui::*debug-levels*))) :timeout 30)
  (when *have-clojure*
    (check "a Clojure error's stack shows on the Debugger page"
           (and (equal "java.lang.ArithmeticException" (second (cadre-ui::dl-condition (first cadre-ui::*debug-levels*))))
                (cadre-ui::dl-frames (first cadre-ui::*debug-levels*))))
    (call-command 'cadre-ui::debugger-abort)
    (write-root-file "clj-demo/src/demo/probe_test.clj"
                     (format nil "(ns demo.probe-test~%  (:require [clojure.test :refer [deftest is]]))~%~%(deftest passes (is (= 1 1)))~%~%(deftest fails~%  (is (= 1 2)))~%"))
    (open-file-path (merge-pathnames "clj-demo/src/demo/probe_test.clj" *root*))))

(then 800
  (when *have-clojure*
    (call-command 'cadre-ui::clojure-run-tests)))

(then-when ((or (not *have-clojure*) (search "tests," (status-text))) :timeout 60)
  (when *have-clojure*
    (check "Run Tests reports the namespace's results" (search "2 tests, 1 passed, 1 failed" (status-text)) (status-text))
    (check "and the failure is a note at its line"
           (find-if (lambda (s) (search "fails fail" (compiler-note-message (cadre-ui::sn-note s)))) cadre-ui::*notes*))
    ;; The same tests on the Tests page
    (call-command 'cadre-ui::show-test-explorer)
    (let ((tests (remove :clojure (cadre-ui::tsp-tests cadre-ui::*tests-page*)
                         :key #'source-test-framework :test-not #'eq)))
      (check "the Tests page finds Clojure's deftests"
             (equal '("passes" "fails") (mapcar #'source-test-name tests)) (mapcar #'source-test-name tests))
      (cadre-ui::run-tests-of tests))))

(then-when ((or (not *have-clojure*)
                (and (not (cadre-ui::tsp-running cadre-ui::*tests-page*))
                     (find :clojure (cadre-ui::tsp-tests cadre-ui::*tests-page*) :key #'source-test-framework)))
            :timeout 60)
  (when *have-clojure*
    (let ((tests (remove :clojure (cadre-ui::tsp-tests cadre-ui::*tests-page*)
                         :key #'source-test-framework :test-not #'eq)))
      (check "and runs them through nREPL, each passed or failed"
             (equal '(:passed :failed) (mapcar #'source-test-status tests))
             (list (status-text)
                   (mapcar (lambda (test) (list (source-test-name test) (source-test-status test) (source-test-messages test))) tests)))
      (check "with the failure's expected and actual" (let ((m (or (first (source-test-messages (second tests))) "")))
                                                         (and (search "expected: 1" m) (search "actual: 2" m)))
             (source-test-messages (second tests))))
    (cadre-ui::inspect-string "{:a 1 :b [1 2]}" "user" :clojure)))

(then-when ((or (not *have-clojure*) (cadre-ui::ins-clojure cadre-ui::*inspector*)) :timeout 30)
  (when *have-clojure*
    (check "the Inspector shows a Clojure value"
           (string= "clojure.lang.PersistentArrayMap" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
    (screenshot "54-clojure-inspector")
    (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "xdg-data/" *root*)) 1)
    (call-command 'cadre-ui::clojure-build-docs)))

(then-when ((or (not *have-clojure*) (search "docs are ready" (status-text))) :timeout 120)
  (when *have-clojure*
    (check "Clojure: Build Docs makes the Clojure docset" (search "Clojure docs are ready" (status-text)) (status-text))
    (open-file-path (merge-pathnames "clj-demo/src/demo/core.clj" *root*))))

(then 500
  (when *have-clojure*
    (set-cursor 3 3)                    ; (|* w h)
    (call-command 'cadre-ui::look-up-documentation)))

(then-when ((or (not *have-clojure*) (find-buffer "Clojure: *")) :timeout 30)
  (when *have-clojure*
    (check "Look Up in Docs shows clojure.core's docstring"
           (and (find-buffer "Clojure: *") (search "Returns the product" (buffer-string (find-buffer "Clojure: *")))))
    (sb-posix:unsetenv "XDG_DATA_HOME")
    (cadre-ui::reload-docsets)
    (call-command 'cadre-ui::clojure-disconnect)))

(then-when ((or (not *have-clojure*) (null (cadre-ui::clojure-connection))) :timeout 30)
  (when *have-clojure*
    (check "disconnecting stops the REPL Cadre started" (null (cadre-ui::clojure-connection)))))

;;; Racket, through Cadre's back end (if Racket is installed)
(section "racket")
(use-root-project)

(defvar *racket-tool-results* nil)
(defvar *have-racket* (ignore-errors (uiop:run-program '("racket" "--version") :output nil) t))

(then 300
  (unless *have-racket* (format t "~&(Racket isn't installed: skipping the Racket checks)~%"))
  (when *have-racket*
    (write-root-file "rkt-demo/info.rkt" (format nil "#lang info~%(define collection \"rkt-demo\")~%"))
    (write-root-file "rkt-demo/demo.rkt"
                     (format nil "#lang racket/base~%(require racket/list)~%~%(define (area w h)~%  (* w h))~%(area 3 4)~%(define (twice x) (area x (first (list 2))))~%"))
    (open-file-path (merge-pathnames "rkt-demo/demo.rkt" *root*))))

(then-when ((or (not *have-racket*) (and (find-buffer "demo.rkt") (eq (current-buffer) (find-buffer "demo.rkt")))))
  (when *have-racket*
    (check "a .rkt file opens in Racket mode" (eq 'racket-mode (buffer-major-mode (current-buffer))))
    (set-cursor 3 1)
    (call-command 'compile-or-eval-defun)))

(then-when ((or (not *have-racket*) (cadre-ui::racket-connection)) :timeout 60)
  (when *have-racket*
    (check "compiling a Racket form starts Racket" (cadre-ui::racket-connection))
    (check "the status bar shows Racket"
           (search "Racket" (gtk:button-get-label (cadre-ui::window-status-connection *window*)))
           (gtk:button-get-label (cadre-ui::window-status-connection *window*)))))

(then-when ((or (not *have-racket*) (search "Compiled" (status-text))) :timeout 60)
  (when *have-racket*
    (check "the define compiles" (search "Compiled" (status-text)) (status-text))
    (set-cursor 5 3)
    (call-command 'compile-or-eval-defun)))

(then-when ((or (not *have-racket*) (cadre-ui::buffer-local (current-buffer) :inline-result)) :timeout 60)
  (when *have-racket*
    (let ((shown (cadre-ui::buffer-local (current-buffer) :inline-result)))
      (check "evaluating a call shows its value inline, in the file's module"
             (and shown (search "⇒ 12" (gtk:label-get-text (cdr shown))))
             (and shown (gtk:label-get-text (cdr shown)))))
    (screenshot "51-racket-eval")
    (set-cursor 5 8)                    ; (area 3 |4)
    (cadre-ui::focus-view (current-view))
    (cadre-ui::request-autodoc (current-view))))

(then-when ((or (not *have-racket*) (search "area" (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))) :timeout 30)
  (when *have-racket*
    (check "the status bar shows the arglist, from the source"
           (search "(area w h)" (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
           (gtk:label-get-text (cadre-ui::window-status-arglist *window*)))
    (set-cursor 6 20)                   ; (define (twice x) (ar|ea …
    (call-command 'edit-definition)))

(then-when ((or (not *have-racket*) (equal '(3 9) (cursor))) :timeout 30)
  (when *have-racket*
    (check "M-. goes to the definition, from check-syntax" (equal '(3 9) (cursor)) (cursor))
    (set-cursor 6 28)                   ; (fi|rst (list 2))
    (call-command 'describe-symbol)))

(then-when ((or (not *have-racket*)
                (and (find-buffer "*Help*") (search "(first lst)" (buffer-string (find-buffer "*Help*")))))
            :timeout 30)
  (when *have-racket*
    (check "describing shows the documented signature"
           (search "(first lst)" (buffer-string (find-buffer "*Help*")))
           (buffer-string (find-buffer "*Help*")))
    (open-file-path (merge-pathnames "rkt-demo/demo.rkt" *root*))
    (call-command 'cadre-ui::racket-run)))

(then-when ((or (not *have-racket*) (search "Ran demo.rkt" (status-text))) :timeout 60)
  (when *have-racket*
    (check "Run File runs the module" (search "Ran demo.rkt" (status-text)) (status-text))
    (call-command 'cadre-ui::racket-repl-tab)))

(then 800
  (when *have-racket*
    (check "the Racket REPL is on the REPL page, titled for Racket"
           (and (eq 'cadre-ui::racket-repl-mode (buffer-major-mode (current-buffer)))
                (equal "REPL · Racket" (panel-page-title "repl")))
           (panel-page-title "repl"))
    (check "the REPL is in the module it ran" (search "demo.rkt>" (buffer-string (current-buffer)))
           (buffer-string (current-buffer)))
    (cadre-ui::with-buffer-repl () (cadre-ui::set-repl-input "(area 6 7)"))
    (call-command 'cadre-ui::repl-return)))

(then-when ((or (not *have-racket*) (search "42" (buffer-string (current-buffer)))) :timeout 30)
  (when *have-racket*
    (check "the Racket REPL evaluates what is typed, in the module" (search "42" (buffer-string (current-buffer)))
           (buffer-string (current-buffer)))
    (screenshot "52-racket-repl")
    (write-root-file "rkt-demo/switch.lisp" (format nil "(in-package #:cl-user)~%"))
    (open-file-path (merge-pathnames "rkt-demo/switch.lisp" *root*))))

(then 600
  (when *have-racket*
    (check "selecting a Common Lisp tab puts its REPL back on the page"
           (and (equal "common-lisp" (gtk:stack-get-visible-child-name cadre-ui::*repl-stack*))
                (equal "REPL" (panel-page-title "repl")))
           (list (panel-page-title "repl")
                 (let ((v (cadre-ui::selected-view *window*))) (and v (buffer-name (cadre-ui::view-buffer v))))
                 (let ((v (cadre-ui::selected-view *window*))) (and v (buffer-major-mode (cadre-ui::view-buffer v))))))
    (open-file-path (merge-pathnames "rkt-demo/demo.rkt" *root*))
    (cadre-ui::show-racket-repl)
    (cadre-ui::with-buffer-repl () (cadre-ui::set-repl-input "(car 5)"))
    (call-command 'cadre-ui::repl-return)))

(then-when ((or (not *have-racket*) (cadre-ui::post-mortem-p (first cadre-ui::*debug-levels*))) :timeout 30)
  (when *have-racket*
    (check "a Racket error shows on the Debugger page"
           (search "car" (first (cadre-ui::dl-condition (first cadre-ui::*debug-levels*)))))
    (screenshot "53-racket-error")
    (call-command 'cadre-ui::debugger-abort)
    (check "Dismiss closes it" (null cadre-ui::*debug-levels*))
    (open-file-path (merge-pathnames "rkt-demo/demo.rkt" *root*))))

(then 500
  (when *have-racket*
    (set-cursor 3 14)                   ; (define (area |w h)
    (call-command 'cadre-ui::rename-symbol)))

(then-when ((or (not *have-racket*) (and (picker) (gtk:widget-get-visible (cadre-ui::picker-popover (picker))))))
  (when *have-racket*
    (gtk:editable-set-text (cadre-ui::picker-entry (picker)) "width")
    (cadre-ui::choose (picker))))

(then 500
  (when *have-racket*
    (let ((text (buffer-string (current-buffer))))
      (check "Rename Symbol in Racket renames the binding and its uses"
             (and (search "(define (area width h)" text) (search "(* width h)" text)) text))))

(then 500
  (when *have-racket*
    (let ((text (buffer-text (current-buffer))))
      (gtk:text-buffer-insert text (gtk:text-buffer-get-end-iter text) (format nil "(define (bad) (if))~%") -1))
    (set-cursor 7 1)
    (call-command 'compile-defun)))

(then-when ((or (not *have-racket*) (search "failed" (status-text))) :timeout 60)
  (when *have-racket*
    (check "a syntax error fails the compile, with a note" (search "1 error" (status-text)) (status-text))
    (call-command 'cadre-ui::racket-check-syntax)))

(then-when ((or (not *have-racket*) (search "problem" (status-text))) :timeout 60)
  (when *have-racket*
    (check "Check Syntax finds the error without running the file" (search "1 problem" (status-text)) (status-text))
    ;; The docs go to a data folder of the run's own, not ~/.local/share.
    (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "xdg-data/" *root*)) 1)
    (call-command 'cadre-ui::racket-build-docs)))

(then-when ((or (not *have-racket*) (search "docs are ready" (status-text))) :timeout 120)
  (when *have-racket*
    (check "Racket: Build Docs makes the Racket docset" (search "docs are ready" (status-text)) (status-text))
    (open-file-path (merge-pathnames "rkt-demo/demo.rkt" *root*))))

(then 500
  (when *have-racket*
    (set-cursor 6 28)                   ; (fi|rst (list 2))
    (call-command 'cadre-ui::look-up-documentation)))

(then-when ((or (not *have-racket*) (find-if #'cadre-ui::hyperspec-buffer-p (buffer-list))) :timeout 30)
  (when *have-racket*
    (let ((docs (find-if #'cadre-ui::hyperspec-buffer-p (buffer-list))))
      (check "Look Up in Docs shows the identifier's signature"
             (and docs (search "(first lst)" (buffer-string docs)))
             (and docs (subseq (buffer-string docs) 0 (min 200 (length (buffer-string docs)))))))
    (screenshot "55-racket-docs")
    (sb-posix:unsetenv "XDG_DATA_HOME")
    (cadre-ui::reload-docsets)
    (call-command 'cadre-ui::raco-test)))

(then-when ((or (not *have-racket*) (and (null cadre-ui::*build*) (search "raco test" (status-text)))) :timeout 120)
  (when *have-racket*
    (check "raco test runs, and the error it reports goes to Problems"
           (find-if (lambda (s) (search "if: bad syntax" (compiler-note-message (cadre-ui::sn-note s)))) cadre-ui::*notes*)
           (status-text))
    ;; Claude's tools ask the current file's language: they wait on the GTK
    ;; thread, so they are called from another.
    (setf *racket-tool-results* nil)
    (sb-thread:make-thread
     (lambda ()
       (setf *racket-tool-results*
             (list (cadre:call-mcp-tool "describe_symbol" (cadre::jobj "symbol" "area"))
                   (cadre:call-mcp-tool "current_context" nil)
                   (nth-value 1 (cadre:call-mcp-tool "apropos" (cadre::jobj "pattern" "area")))))))))

(then-when ((or (not *have-racket*) *racket-tool-results*) :timeout 30)
  (when *have-racket*
    (destructuring-bind (describe context apropos-failed) *racket-tool-results*
      (check "Claude's describe_symbol asks Racket in a Racket file" (search "area: procedure" describe) describe)
      (check "current_context names the language and the Racket connection"
             (and (search "Language: Racket" context) (search "Racket: Racket 9" context)) context)
      (check "Swank-only tools say so in Racket" apropos-failed))
    (call-command 'cadre-ui::racket-stop)))

(then-when ((or (not *have-racket*) (null (cadre-ui::racket-connection))) :timeout 30)
  (when *have-racket*
    (check "stopping Racket disconnects" (null (cadre-ui::racket-connection)))))

;; The Tests page runs rackunit tests with raco test, a file at a time.
(then 300
  (when *have-racket*
    (write-root-file "rkt-demo/arith-test.rkt"
                     (format nil "#lang racket/base~%(module+ test~%  (require rackunit)~%  (test-case \"adds\"~%    (check-equal? (+ 1 1) 2))~%  (test-case \"broken\"~%    (check-equal? (+ 1 1) 3)))~%"))
    (open-file-path (merge-pathnames "rkt-demo/arith-test.rkt" *root*))))

(then-when ((or (not *have-racket*) (find-buffer "arith-test.rkt")) :timeout 10)
  (when *have-racket*
    (call-command 'cadre-ui::show-test-explorer)
    (let ((tests (remove :rackunit (cadre-ui::tsp-tests cadre-ui::*tests-page*)
                         :key #'source-test-framework :test-not #'eq)))
      (check "the Tests page finds rackunit's test-cases"
             (equal '("adds" "broken") (mapcar #'source-test-name tests)) (mapcar #'source-test-name tests))
      (cadre-ui::run-tests-of tests))))

(then-when ((or (not *have-racket*)
                (and (not (cadre-ui::tsp-running cadre-ui::*tests-page*))
                     (find :rackunit (cadre-ui::tsp-tests cadre-ui::*tests-page*) :key #'source-test-framework)))
            :timeout 90)
  (when *have-racket*
    (let ((tests (remove :rackunit (cadre-ui::tsp-tests cadre-ui::*tests-page*)
                         :key #'source-test-framework :test-not #'eq)))
      (check "and runs them with raco test, each passed or failed"
             (equal '(:passed :failed) (mapcar #'source-test-status tests))
             (mapcar (lambda (test) (list (source-test-name test) (source-test-status test) (source-test-messages test))) tests)))))

;;; The Project page: a Racket package and a Clojure project, with their dependencies
(section "projects")

(defun section-labels () (label-texts cadre-ui::*language-sections*))

(defvar *dep-result* nil)

(defun find-button (label)
  (find-if (lambda (w) (and (typep w 'gtk:button) (equal (gtk:button-get-label w) label)))
           (find-widgets cadre-ui::*language-sections* (lambda (w) (typep w 'gtk:button)))))

(then 300
  (when *have-racket*
    (write-root-file "rkt-pkg/info.rkt"
                     (format nil "#lang info~%(define collection \"greet\")~%(define version \"0.1\")~%(define deps '(\"base\" \"no-such-package-xyz\"))~%"))
    (write-root-file "rkt-pkg/main.rkt"
                     (format nil "#lang racket/base~%(provide hi)~%(define (hi) 'hi)~%(module+ main~%  (printf \"name? \")~%  (flush-output)~%  (printf \"Hello, ~~a!~~n\" (read-line)))~%"))
    (cadre-ui::set-window-project *window* (merge-pathnames "rkt-pkg/" *root*))
    (call-command 'cadre-ui::show-systems)
    (cadre-ui::refresh-systems)))

(then-when ((or (not *have-racket*) (find-if (lambda (l) (search "Missing" l)) (section-labels))) :timeout 30)
  (when *have-racket*
    (check "the Project page shows the Racket package" (member "RACKET PACKAGE · greet 0.1" (section-labels) :test #'equal)
           (section-labels))
    (check "with its missing dependency, and Install"
           (and (find-if (lambda (l) (search "Missing: no-such-package-xyz" l)) (section-labels))
                (gtk:widget-get-visible (find-button "Install"))))
    (check "and its modules" (member "main.rkt" (section-labels) :test #'equal))
    (screenshot "56-racket-project")
    (let ((button (gethash :run-button cadre-ui::*named-widgets*)))
      (check "the ▶ button runs the package's main module"
             (and (gtk:widget-get-visible button)
                  (equal "Run main.rkt (in the Racket REPL)" (gtk:widget-get-tooltip-text button)))
             (gtk:widget-get-tooltip-text button)))
    (call-command 'cadre-ui::run-app)))

(then-when ((or (not *have-racket*) (cadre-ui::repl-reading (cadre-ui::language-repl :racket))) :timeout 60)
  (when *have-racket*
    (check "it runs in the Racket REPL, which takes the line it reads"
           (cadre-ui::repl-reading (cadre-ui::language-repl :racket)))
    (cadre-ui::with-buffer-repl () (cadre-ui::set-repl-input "Pat"))
    (call-command 'cadre-ui::repl-return)))

(then-when ((or (not *have-racket*) (null cadre-ui::*language-app*)) :timeout 30)
  (when *have-racket*
    (let ((text (buffer-string (cadre-ui::repl-buffer (cadre-ui::language-repl :racket)))))
      (check "Racket app output and completion are retained in Runs"
             (let ((run (first cadre-ui::*project-runs*)))
               (and (eq :passed (cadre-ui::pr-status run))
                    (search "Hello, Pat!" (cadre-ui::project-run-output-text run)))))
      (check "and the program ends, with ▶ back"
             (and (search "Hello, Pat!" text) (search "finished" (status-text)))
             (status-text)))))

;; The package's own docs, apart from Racket's (in a data folder of the run's own).
(then 300
  (when *have-racket*
    (sb-posix:setenv "XDG_DATA_HOME" (namestring (merge-pathnames "xdg-data/" *root*)) 1)
    (gtk:widget-activate (find-button "Build Docs"))))

(then-when ((or (not *have-racket*) (search "docs are ready" (status-text))) :timeout 60)
  (when *have-racket*
    (check "the package's Build Docs makes its own docset" (search "rkt-pkg docs are ready: 1 entr" (status-text)) (status-text))
    (open-file-path (merge-pathnames "rkt-pkg/main.rkt" *root*))))

(then 600
  (when *have-racket*
    (set-cursor 2 9)                    ; (define (h|i) 'hi)
    (call-command 'cadre-ui::look-up-documentation)))

(then-when ((or (not *have-racket*) (find-buffer "rkt-pkg: hi")) :timeout 20)
  (when *have-racket*
    (check "Look Up in Docs finds the project's own entry first" (find-buffer "rkt-pkg: hi"))
    (call-command 'cadre-ui::settings)))

(then 800
  (when *have-racket*
    (check "Settings has the Racket docs, with Rebuild"
           (member "Racket docs" (label-texts cadre-ui::*settings-dialog*) :test #'equal)
           (remove-if-not (lambda (l) (search "docs" l)) (label-texts cadre-ui::*settings-dialog*)))
    (adw:dialog-close cadre-ui::*settings-dialog*)
    (sb-posix:unsetenv "XDG_DATA_HOME")
    (cadre-ui::reload-docsets)))

(then 300
  (when *have-clojure*
    (write-root-file "clj-proj/deps.edn"
                     "{:paths [\"src\"] :deps {org.clojure/clojure {:mvn/version \"1.12.0\"}} :aliases {:dev {}}}")
    (write-root-file "clj-proj/src/proj/core.clj"
                     (format nil "(ns proj.core)~%(defn hi [] :hi)~%(defn -main [& args] (println \"proj ran\") :done)~%"))
    (cadre-ui::set-window-project *window* (merge-pathnames "clj-proj/" *root*))
    (cadre-ui::refresh-systems)))

(then 500
  (when *have-clojure*
    (check "the Project page shows the Clojure project"
           (find-if (lambda (l) (search "CLOJURE PROJECT · clj-proj" l)) (section-labels)) (section-labels))
    (check "its aliases, dependencies and namespaces"
           (and (member ":dev" (section-labels) :test #'equal)
                (member "org.clojure/clojure" (section-labels) :test #'equal)
                (member "proj.core" (section-labels) :test #'equal)))
    (gtk:widget-activate (find-button "Jack In"))))

(then-when ((or (not *have-clojure*) (and (cadre-ui::clojure-connection) (find-button "Restart"))) :timeout 120)
  (when *have-clojure*
    (check "Jack In from the page starts the project's REPL" (find-button "Restart"))
    (open-file-path (merge-pathnames "clj-proj/deps.edn" *root*))))

(then 500
  (when *have-clojure*
    (let ((text (buffer-text (current-buffer))))
      (text-replace-contents text "{:paths [\"src\"] :deps {org.clojure/clojure {:mvn/version \"1.12.0\"} org.clojure/data.json {:mvn/version \"2.5.1\"}} :aliases {:dev {}}}"))
    ;; As if typed: replacing the text from code doesn't mark it changed.
    (setf (buffer-modified-p (current-buffer)) t)
    (call-command 'cadre-ui::save-buffer)))

(then-when ((or (not *have-clojure*) (search "dependencies changed" (status-text))) :timeout 20)
  (when *have-clojure*
    (check "saving deps.edn with a new library says the dependencies changed"
           (and (search "dependencies changed" (status-text))
                (member "Changed since the REPL started" (section-labels) :test #'equal))
           (status-text))
    (screenshot "57-clojure-project")
    (gtk:widget-activate (find-button "Load New Dependencies"))))

(then-when ((or (not *have-clojure*) (search "loaded in the REPL" (status-text))) :timeout 180)
  (when *have-clojure*
    (check "Load New Dependencies adds them to the running REPL" (search "loaded in the REPL" (status-text)) (status-text))
    (let ((result :waiting))
      (cadre-ui::request #'backend-eval (cadre-ui::clojure-connection) "(do (require 'clojure.data.json) (clojure.data.json/write-str [1]))"
               :package "user" :on-ok (lambda (v) (setf result v)) :on-abort (lambda (r) (setf result r)))
      (setf *dep-result* (lambda () result)))))


(then-when ((or (not *have-clojure*) (and *dep-result* (not (eq :waiting (funcall *dep-result*))))) :timeout 30)
  (when *have-clojure*
    (check "and the new library loads without a restart" (search "[1]" (funcall *dep-result*)) (funcall *dep-result*))
    (check "the ▶ button runs the project's -main"
           (equal "Run proj.core/-main (in the Clojure REPL)"
                  (gtk:widget-get-tooltip-text (gethash :run-button cadre-ui::*named-widgets*))))
    (call-command 'cadre-ui::run-app)))

(then-when ((or (not *have-clojure*) (search "returned" (status-text))) :timeout 60)
  (when *have-clojure*
    (check "it runs in the Clojure REPL, and says what -main returned"
           (and (search "proj ran" (buffer-string (cadre-ui::repl-buffer (cadre-ui::language-repl :clojure))))
                (search "proj.core/-main returned :done" (status-text)))
           (status-text))
    (check "Clojure app output and completion are retained in Runs"
           (let ((run (first cadre-ui::*project-runs*)))
             (and (eq :passed (cadre-ui::pr-status run))
                  (search "proj ran" (cadre-ui::project-run-output-text run)))))
    (write-root-file "clj-proj/test/proj/core_test.clj"
                     (format nil "(ns proj.core-test (:require [clojure.test :refer [deftest is]]))~%(deftest works (is (= 2 (+ 1 1))))~%"))
    (cadre-ui::scan-project-tests cadre-ui::*tests-page* (merge-pathnames "clj-proj/" *root*))
    (cadre-ui::run-tests-of (cadre-ui::tsp-tests cadre-ui::*tests-page*))))

(then-when ((or (not *have-clojure*) (not (cadre-ui::tsp-running cadre-ui::*tests-page*))) :timeout 60)
  (when *have-clojure*
    (check "Clojure Test Explorer results are retained in Runs"
           (let ((run (cadre-ui::tsp-run cadre-ui::*tests-page*)))
             (and (eq :passed (cadre-ui::pr-status run))
                  (search "works" (cadre-ui::project-run-output-text run))))
           (cadre-ui::project-run-output-text (cadre-ui::tsp-run cadre-ui::*tests-page*)))
    ;; A test file that refers to a function that isn't there.
    (write-root-file "clj-proj/test/proj/core_test.clj"
                     (format nil "(ns proj.core-test~%  (:require [clojure.test :refer [deftest is]]~%            [proj.core :refer [no-such-fn]]))~%(deftest t (is (no-such-fn)))~%"))
    (cadre-ui::run-clojure-project-tests (merge-pathnames "clj-proj/" *root*))))

(then-when ((or (not *have-clojure*) (search "Project tests" (status-text))) :timeout 90)
  (when *have-clojure*
    (check "Run All Tests says which namespaces didn't load, with why in Problems"
           (and (search "1 namespace didn't load" (status-text))
                (find-if (lambda (s) (search "no-such-fn" (compiler-note-message (cadre-ui::sn-note s)))) cadre-ui::*notes*))
           (status-text))
    (check "Clojure project test load failures are retained in Runs"
           (let ((run cadre-ui::*clojure-project-test-run*))
             (and run (eq :failed (cadre-ui::pr-status run))
                  (search "didn't load" (cadre-ui::project-run-output-text run)))))
    (call-command 'cadre-ui::clojure-disconnect))
  (cadre-ui::set-window-project *window* *root*))

;; Claude follows Open Folder: a conversation in one folder ends, and the
;; next starts in the folder opened.
(then 300
  (setf (cadre-ui::chat-started cadre-ui::*chat*) t
        (cadre-ui::chat-directory cadre-ui::*chat*) *root*)
  (cadre-ui::set-window-project *window* (merge-pathnames "clj-proj/" *root*)))

(then 300
  (let ((notes (label-texts (cadre-ui::chat-messages cadre-ui::*chat*))))
    (check "opening another folder starts Claude's next conversation there"
           (and (not (cadre-ui::chat-started cadre-ui::*chat*))
                (find-if (lambda (l) (search "Claude now works in" l)) notes))
           notes))
  (cadre-ui::set-window-project *window* *root*))

;; A folder of several projects: the page and ▶ follow the current file's.
(then 500
  (when (and *have-racket* *have-clojure*)
    (open-file-path (merge-pathnames "rkt-pkg/main.rkt" *root*))))

(then 800
  (when (and *have-racket* *have-clojure*)
    (check "in a folder of projects, the page shows the current file's project"
           (and (find-if (lambda (l) (search "RACKET PACKAGE" l)) (section-labels))
                (equal "Run main.rkt (in the Racket REPL)"
                       (gtk:widget-get-tooltip-text (gethash :run-button cadre-ui::*named-widgets*))))
           (section-labels))
    (open-file-path (merge-pathnames "clj-proj/src/proj/core.clj" *root*))))

(then 800
  (when (and *have-racket* *have-clojure*)
    (check "and switching to a file in another project switches to it"
           (and (find-if (lambda (l) (search "CLOJURE PROJECT · clj-proj" l)) (section-labels))
                (not (find-if (lambda (l) (search "RACKET PACKAGE" l)) (section-labels)))
                (equal "Run proj.core/-main (in the Clojure REPL)"
                       (gtk:widget-get-tooltip-text (gethash :run-button cadre-ui::*named-widgets*))))
           (section-labels))))

;;; Indentation learned from the Lisp: a macro with &body indents its body by 2
(section "indentation")
(connect-lisp)

(then 300
  (cadre-ui::repl-eval "(defmacro smoke-around (thing &body body) `(progn ,thing ,@body))"))

(then-when ((cadre:indentation-spec "smoke-around") :timeout 20)
  (check "a macro's &body position comes from the Lisp" (eql 1 (cadre:indentation-spec "smoke-around"))
         (cadre:indentation-spec "smoke-around")))

(then 100
  (let ((key (cadre-ui::event-key (gdk:keyval-from-name "Return") '(:super-mask :alt-mask) :super-as-control t)))
    (check "Cmd+Option+Return compiles and loads a Lisp file (Standard keys)"
           (eq 'cadre-ui::compile-and-load-file
               (cadre:keymap-lookup (cadre-ui::mode-profile-keymap 'cadre:lisp-mode :standard) (list key)))
           key)))

;;; Other languages, through tree-sitter (needs the grammars installed)
(section "tree-sitter")

(defvar *langs* (merge-pathnames "langs/" *new-parent*))
(defun write-lang-file (name text)
  (let ((path (merge-pathnames name *langs*)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :if-exists :supersede) (write-string text o))
    path))

(defvar *grammars* (every #'tree-sitter-language-installed-p '("javascript" "typescript" "json")))

(then 300
  (unless *grammars* (format t "~&(tree-sitter grammars not installed: skipping their checks)~%"))
  (write-lang-file "app.js" (format nil "// greet~%const MAX = 10;~%function greet(name) {~%  return `hi ${name}`;~%}~%"))
  (write-lang-file "types.ts" (format nil "interface Shape {~%  area(): number;~%}~%"))
  (write-lang-file "data.json" (format nil "{~%  \"name\": \"cadre\",~%  \"ok\": true~%}~%"))
  (open-file-path (merge-pathnames "app.js" *langs*)))

(then-when ((and (find-buffer "app.js") (eq (current-buffer) (find-buffer "app.js"))))
  (check "a .js file opens in JavaScript mode" (eq 'javascript-mode (buffer-major-mode (current-buffer)))))

(then 600
  (when *grammars*
    (check "tree-sitter colors comments" (has-face-p 0 3 :comment))
    (check "keywords" (has-face-p 2 1 :keyword))
    (check "function names" (has-face-p 2 10 :code-function))
    (check "constants" (has-face-p 1 7 :constant))
    (check "and strings" (has-face-p 3 10 :string))
    (screenshot "43-javascript")
    (let ((gtk-buffer (buffer-text (current-buffer))))
      (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) (format nil "let y = 1;~%") -1))))

(then 500
  (when *grammars*
    (check "an edit is colored after it's parsed again" (has-face-p 5 1 :keyword))
    (check "the outline lists the function" (equal '(("greet" :function 2 0)) (cadre-ui::outline-items (current-buffer)))
           (cadre-ui::outline-items (current-buffer)))
    (set-cursor 3 4)
    (call-command 'cadre-ui::toggle-fold)))

(then 300
  (when *grammars*
    (check "the syntax tree folds the function" (equal '(2) (fold-lines-now)) (fold-lines-now))
    (call-command 'cadre-ui::unfold-all)
    (set-cursor 1 0)
    (call-command 'cadre-ui::toggle-comment)))

(then 300
  (check "Toggle Comment uses //" (string= "// const MAX = 10;" (line-text 1)) (line-text 1))
  (call-command 'cadre-ui::toggle-comment))

(then 300
  (check "and takes it away again" (string= "const MAX = 10;" (line-text 1)) (line-text 1))
  (let ((gtk-buffer (buffer-text (current-buffer))))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) "if (y) {}" -1)
    (set-cursor 6 8)
    (call-command 'cadre-ui::code-newline)))

(then 300
  (check "Return between braces indents and puts the closing one on its own line"
         (and (string= "if (y) {" (line-text 6)) (string= "  " (line-text 7)) (string= "}" (line-text 8))
              (equal '(7 2) (cursor)))
         (list (line-text 6) (line-text 7) (line-text 8) (cursor)))
  (open-file-path (merge-pathnames "data.json" *langs*)))

(then-when ((and (find-buffer "data.json") (eq (current-buffer) (find-buffer "data.json"))))
  (check "a .json file opens in JSON mode" (eq 'json-mode (buffer-major-mode (current-buffer)))))

(then 600
  (when *grammars*
    (check "JSON keys are colored as keys" (has-face-p 1 4 :code-property))
    (check "and values as values" (has-face-p 1 12 :string)))
  (open-file-path (merge-pathnames "types.ts" *langs*)))

(then-when ((and (find-buffer "types.ts") (eq (current-buffer) (find-buffer "types.ts"))))
  (check "a .ts file opens in TypeScript mode" (eq 'typescript-mode (buffer-major-mode (current-buffer)))))

(then 600
  (when *grammars*
    (check "TypeScript types are colored" (has-face-p 0 12 :code-type))
    (check "and its outline lists the interface"
           (equal '(("Shape" :interface 0 0)) (cadre-ui::outline-items (current-buffer)))
           (cadre-ui::outline-items (current-buffer)))))

;;; HTML, CSS and Format Document
(section "html-css" :needs ("tree-sitter"))

(defvar *web-grammars* (every #'tree-sitter-language-installed-p '("html" "css" "javascript")))

(then 300
  (write-lang-file "page.html" (format nil "<html>~%<head>~%  <style>body { color: red; }</style>~%</head>~%<body class=\"main\">~%  <h1>Title</h1>~%  <script>const x = 1;</script>~%  <div></div>~%</body>~%</html>~%"))
  (write-lang-file "min.css" "a,b{color:red;margin:0}p{font-size:12px}")
  (write-lang-file "min.json" "{\"name\":\"cadre\",\"tags\":[\"lisp\",\"editor\"],\"ok\":true}")
  (write-lang-file "messy.lisp" (format nil "(defun f ()~%(+ 1~%2))~%"))
  (open-file-path (merge-pathnames "page.html" *langs*)))

(then-when ((and (find-buffer "page.html") (eq (current-buffer) (find-buffer "page.html"))))
  (check "a .html file opens in HTML mode" (eq 'html-mode (buffer-major-mode (current-buffer)))))

(then 700
  (when *web-grammars*
    (check "HTML tags are colored" (has-face-p 5 4 :code-type))
    (check "attributes" (has-face-p 4 7 :code-property))
    (check "the style element's CSS is colored as CSS" (has-face-p 2 17 :code-property))
    (check "and the script's JavaScript as JavaScript" (has-face-p 6 11 :keyword))
    (check "the outline lists the heading" (equal '(("Title" :heading 5 0)) (cadre-ui::outline-items (current-buffer)))
           (cadre-ui::outline-items (current-buffer)))
    (screenshot "44-html"))
  (set-cursor 5 4)
  (call-command 'cadre-ui::toggle-comment))

(then 300
  (check "Toggle Comment wraps HTML lines in <!-- -->" (string= "  <!-- <h1>Title</h1> -->" (line-text 5)) (line-text 5))
  (call-command 'cadre-ui::toggle-comment))

(then 300
  (check "and unwraps them" (string= "  <h1>Title</h1>" (line-text 5)) (line-text 5))
  (set-cursor 7 7)
  (call-command 'cadre-ui::code-newline))

(then 300
  (check "Return between tags puts the closing tag on its own line"
         (and (string= "  <div>" (line-text 7)) (string= "    " (line-text 8)) (string= "  </div>" (line-text 9)))
         (list (line-text 7) (line-text 8) (line-text 9)))
  (open-file-path (merge-pathnames "min.css" *langs*)))

(then-when ((and (find-buffer "min.css") (eq (current-buffer) (find-buffer "min.css"))))
  (check "a .css file opens in CSS mode" (eq 'css-mode (buffer-major-mode (current-buffer))))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "Format Document lays out CSS"
         (string= (format nil "a, b {~%  color: red;~%  margin: 0;~%}~%~%p {~%  font-size: 12px;~%}~%") (buffer-string (current-buffer)))
         (buffer-string (current-buffer)))
  (when *web-grammars* (check "and the result is colored" (has-face-p 1 3 :code-property)))
  (call-command 'cadre-ui::undo))

(then 300
  (check "one Undo takes the formatting back" (string= "a,b{color:red;margin:0}p{font-size:12px}" (buffer-string (current-buffer)))
         (buffer-string (current-buffer)))
  (open-file-path (merge-pathnames "min.json" *langs*)))

(then-when ((and (find-buffer "min.json") (eq (current-buffer) (find-buffer "min.json"))))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "Format Document lays out single-line JSON"
         (string= (format nil "{~%  \"name\": \"cadre\",~%  \"tags\": [~%    \"lisp\",~%    \"editor\"~%  ],~%  \"ok\": true~%}~%")
                  (buffer-string (current-buffer)))
         (buffer-string (current-buffer)))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "formatting it again changes nothing" (search "Already formatted" (status-text)) (status-text))
  (open-file-path (merge-pathnames "app.js" *langs*)))

(then-when ((and (find-buffer "app.js") (eq (current-buffer) (find-buffer "app.js"))))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "JavaScript needs Prettier, and says so" (search "prettier" (status-text)) (status-text))
  (open-file-path (merge-pathnames "messy.lisp" *langs*)))

(then-when ((and (find-buffer "messy.lisp") (eq (current-buffer) (find-buffer "messy.lisp"))))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "Format Document indents Lisp"
         (string= (format nil "(defun f ()~%  (+ 1~%     2))~%") (buffer-string (current-buffer)))
         (buffer-string (current-buffer))))

;;; XML: colors, outline, Return between tags, Format Document
(section "xml")

(defvar *xml-grammar* (tree-sitter-language-installed-p "xml"))

(then 300
  (unless *xml-grammar* (format t "~&(the XML grammar isn't installed: skipping its colors)~%"))
  (write-lang-file "feed.xml" (format nil "<?xml version=\"1.0\"?>~%<!-- items -->~%<feed lang=\"en\"><item id=\"1\">One</item><entry/></feed>~%"))
  (open-file-path (merge-pathnames "feed.xml" *langs*)))

(then-when ((and (find-buffer "feed.xml") (eq (current-buffer) (find-buffer "feed.xml"))))
  (check "a .xml file opens in XML mode" (eq 'xml-mode (buffer-major-mode (current-buffer)))))

(then 600
  (when *xml-grammar*
    (check "XML comments are colored" (has-face-p 1 6 :comment))
    (check "tag names" (has-face-p 2 2 :code-type))
    (check "attribute names" (has-face-p 2 7 :code-property))
    (check "and attribute values" (has-face-p 2 13 :string))
    (check "the outline lists the root's elements"
           (equal '("item" "entry") (mapcar #'first (cadre-ui::outline-items (current-buffer))))
           (cadre-ui::outline-items (current-buffer))))
  (call-command 'cadre-ui::format-document))

(then 300
  (check "Format Document lays XML out"
         (string= (format nil "<?xml version=\"1.0\"?>~%<!-- items -->~%<feed lang=\"en\">~%  <item id=\"1\">One</item>~%  <entry/>~%</feed>~%")
                  (buffer-string (current-buffer)))
         (buffer-string (current-buffer)))
  (screenshot "45-xml")
  (set-cursor 3 2)
  (insert-at-cursor "<br>")
  (call-command 'cadre-ui::code-newline))

(then 300
  (check "Return after an opening tag indents, even <br> (not HTML's empty element)"
         (string= "    <item id=\"1\">One</item>" (line-text 4)) (list (line-text 3) (line-text 4)))
  (setf (buffer-modified-p (current-buffer)) nil))

;;; A value from the REPL or the Inspector: copy it, or open it in a tab

(section "repl-values")
(defvar *title-before* nil)
(connect-lisp)

(then 100
  (cadre-ui::show-repl-page)
  (cadre-ui::repl-eval "(format nil \"<a><b>~a</b></a>\" \"x \\\"q\\\"\")"))

(then-when ((cadre-ui::repl-presentations) :timeout 20)
  (let* ((presentation (first (cadre-ui::repl-presentations)))
         (items (cadre-ui::presentation-menu-items (cadre-ui::repl-view) presentation)))
    (check "right-clicking a result offers Copy Value and Open in New Tab"
           (equal '("Inspect" "Copy Value" "Copy to Input" "Open in New Tab") (mapcar #'car items))
           (mapcar #'car items))
    (cadre-ui::show-presentation-menu (cadre-ui::repl-view) presentation 40 40)
    (funcall (cdr (assoc "Copy Value" items :test #'string=)))
    (funcall (cdr (assoc "Open in New Tab" items :test #'string=)))))

(then-when ((find-buffer "value") :timeout 20)
  (let ((buffer (find-buffer "value")))
    (check "Open in New Tab shows the string itself, without quotes or escapes"
           (string= "<a><b>x \"q\"</b></a>" (buffer-string buffer)) (buffer-string buffer))
    (check "in the mode it looks like" (eq 'xml-mode (buffer-major-mode buffer)))
    (check "Copy Value says what it copied" (search "Copied 19 characters" (status-text)) (status-text)))
  (setf *title-before* (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
  (cadre-ui::inspect-string "(list 1 \"two\")" "COMMON-LISP-USER"))

;; The title names the object (#<CONS …>), not its contents: wait for it to change.
(then-when ((string/= *title-before* (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))) :timeout 20)
  (let ((items (cadre-ui::inspector-menu-items nil)))
    (check "the Inspector offers the inspected value too"
           (and (assoc "Copy the Inspected Value" items :test #'string=)
                (assoc "Open in New Tab" items :test #'string=))
           (mapcar #'car items))
    (funcall (cdr (assoc "Open in New Tab" items :test #'string=)))))

(then-when ((find-buffer "value<2>") :timeout 20)
  (check "and opens it, printed in full" (string= "(1 \"two\")" (buffer-string (find-buffer "value<2>")))
         (buffer-string (find-buffer "value<2>")))
  (dolist (name '("value" "value<2>"))
    (setf (buffer-modified-p (find-buffer name)) nil)))

;;; The Profiler page
(section "profiler")
(defun profiler () cadre-ui::*profiler-page*)
(defun current-profile () (cadre-ui::pp-profile (profiler)))
(defun profiler-row-names ()
  (loop for row = (gtk:widget-get-first-child (cadre-ui::pp-list (profiler))) then (gtk:widget-get-next-sibling row)
        while row
        for function = (gethash row cadre-ui::*profiler-rows*)
        when function collect (profile-function-name function)))
(connect-lisp)

(then 100
  (call-command 'cadre-ui::clear-profile)
  (cadre-ui::profile-form-text "(progn (defun smoke-fib (n) (if (< n 2) n (+ (smoke-fib (- n 1)) (smoke-fib (- n 2)))))
  (loop repeat 600 sum (smoke-fib 22)))" "COMMON-LISP-USER"))

(then-when ((and (current-profile) (plusp (profile-samples (current-profile)))) :timeout 60)
  (check "Profile Form shows the Profiler page"
         (equal "profiler" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (check "with the samples' count" (search "samples of CPU" (gtk:label-get-text (cadre-ui::pp-title (profiler))))
         (gtk:label-get-text (cadre-ui::pp-title (profiler))))
  (check "each function a row, the busiest first" (equal "SMOKE-FIB" (first (profiler-row-names)))
         (subseq (profiler-row-names) 0 (min 3 (length (profiler-row-names)))))
  (check "only the form's own stacks" (equal '("SMOKE-FIB") (mapcar #'first (cddr (profile-tree (current-profile)))))
         (mapcar #'first (cddr (profile-tree (current-profile)))))
  (cadre-ui::select-profile-function (profiler) "SMOKE-FIB"))

(then 300
  (let ((texts (label-texts (cadre-ui::pp-details (profiler)))))
    (check "choosing a function shows its callers and callees"
           (and (member "Called by" texts :test #'equal) (member "Calls" texts :test #'equal)) texts))
  (check "and offers its definition"
         (find-if (lambda (b) (and (equal "Go to Definition" (gtk:button-get-label b)) (gtk:widget-get-sensitive b)))
                  (find-widgets (cadre-ui::pp-details (profiler)) (lambda (w) (typep w 'gtk:button)))))
  (setf (cadre-ui::pp-sort (profiler)) :name)
  (cadre-ui::fill-profiler-list (profiler))
  (let ((names (profiler-row-names)))
    (check "the table sorts by name" (equal names (sort (copy-list names) #'string-lessp))))
  (setf (cadre-ui::pp-sort (profiler)) :total)
  (gtk:editable-set-text (cadre-ui::pp-filter (profiler)) "fib"))

(then 300
  (check "and filters" (equal '("SMOKE-FIB") (profiler-row-names)) (profiler-row-names))
  (gtk:editable-set-text (cadre-ui::pp-filter (profiler)) ""))

(then 300
  (screenshot "profiler-table")
  (let ((flame (find-if (lambda (b) (equal "Flame Graph" (gtk:button-get-label b)))
                        (find-widgets (cadre-ui::pp-widget (profiler)) (lambda (w) (typep w 'gtk:toggle-button))))))
    (check "a toggle shows the flame graph" flame)
    (when flame (gtk:toggle-button-set-active flame t))))

(then 400
  (let ((boxes (cadre-ui::pp-boxes (profiler))))
    (check "the flame graph has a box for each call" (< 3 (length boxes)) (length boxes))
    (screenshot "profiler-flame")
    ;; Click the third row's widest box: it zooms in.
    (let* ((area (cadre-ui::pp-flame (profiler)))
           (box (find 2 boxes :key #'second)))
      (check "the flame graph is drawn" (plusp (gtk:widget-get-width area)))
      (when box
        (let ((path (flame-path-to (profile-tree (current-profile)) (first box))))
          (setf (cadre-ui::pp-path (profiler)) path)
          (cadre-ui::update-flame-boxes (profiler))
          (check "zooming in puts the box at full width"
                 (= 1.0 (fourth (find 2 (cadre-ui::pp-boxes (profiler)) :key #'second))))))))
  (call-command 'cadre-ui::profile-all-threads))

(then-when ((and (current-profile) (profile-running (current-profile))) :timeout 20)
  (check "Profile All Threads says it's running" (equal "Stop" (gtk:button-get-label (cadre-ui::pp-run-button (profiler)))))
  (cadre-ui::repl-eval "(loop repeat 800 sum (smoke-fib 25))"))

(then 1500
  (call-command 'cadre-ui::stop-profiling))

(then-when ((and (current-profile) (not (profile-running (current-profile)))) :timeout 20)
  (check "Stop shows what the threads did" (find "SMOKE-FIB" (profile-functions (current-profile))
                                                 :key #'profile-function-name :test #'string=)
         (list (profile-samples (current-profile))
               (mapcar #'profile-function-name (subseq (profile-functions (current-profile)) 0 (min 8 (length (profile-functions (current-profile))))))))
  (call-command 'cadre-ui::clear-profile))

(then 200
  (check "Clear forgets the samples" (equal "Nothing profiled" (gtk:label-get-text (cadre-ui::pp-title (profiler))))))

;;; The scratch buffer
(section "scratch")
(defun scratch-buffer () (find-if #'cadre-ui::scratch-buffer-p (buffer-list)))

(then 100
  (let ((button (gethash :scratch-button cadre-ui::*named-widgets*)))
    (check "Scratch is available in the header" (and button (gtk:widget-get-visible button)))
    (when button (gtk:widget-activate button))))

(then-when ((scratch-buffer))
  (let* ((buffer (scratch-buffer))
         (file (buffer-file buffer)))
    (check "Scratch Buffer opens *scratch*" (equal "*scratch*" (buffer-name buffer)) (buffer-name buffer))
    (check "in Lisp mode" (eq 'lisp-mode (buffer-major-mode buffer)))
    (check "kept in the config directory, not the project"
           (and (search "cadre/scratch/" (namestring file)) (not (uiop:subpathp file *root*))) file)
    (check "named after the project" (search (cadre::project-file-key (cadre-ui::window-project *window*)) (namestring file)) file)
    (check "starting with a note" (search ";;;; Scratch for" (buffer-string buffer)))
    (let ((gtk-buffer (buffer-text buffer)))
      (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) "(+ 1 2)" -1))
    (check "it never asks to be saved" (not (cadre-ui::buffer-needs-saving-p buffer)))))

(then-when ((not (buffer-modified-p (scratch-buffer))) :timeout 5)
  (check "it saves itself after a change"
         (search "(+ 1 2)" (uiop:read-file-string (buffer-file (scratch-buffer)))))
  (cadre-ui::show-buffer *window* (scratch-buffer))
  (let ((gtk-buffer (buffer-text (scratch-buffer))))
    (gtk:text-buffer-insert gtk-buffer (gtk:text-buffer-get-end-iter gtk-buffer) " ; more" -1))
  (call-command 'cadre-ui::close-tab))

(then-when ((null (scratch-buffer)))
  (check "closing it right after a change still saves it"
         (search "; more" (uiop:read-file-string (cadre-ui::scratch-file (cadre-ui::window-project *window*)))))
  (check "Ctrl+K Shift+S and C-x M-s open it"
         (and (eq 'cadre-ui::scratch (keymap-lookup cadre-ui::*standard-global-keymap* (parse-keys "C-k S")))
              (eq 'cadre-ui::scratch (keymap-lookup cadre-ui::*emacs-global-keymap* (parse-keys "C-x M-s")))))
  (call-command 'cadre-ui::scratch))

(then-when ((scratch-buffer))
  (check "and it opens again as it was" (search "(+ 1 2) ; more" (buffer-string (scratch-buffer)))
         (buffer-string (scratch-buffer))))

;;; The Libraries tab
(section "libraries")
(defvar *smoke-asd-text* nil)
(defun libraries-page () (let ((b (cadre-ui::libraries-buffer *root*))) (and b (buffer-local b :libraries))))
(defun smoke-asd () (merge-pathnames "smoke.asd" *root*))
(connect-lisp)
(load-quicklisp)

(then 100
  (setf *smoke-asd-text* (uiop:read-file-string (smoke-asd)))
  (cadre-ui::show-libraries *root*))

(then-when ((let ((page (libraries-page))) (and page (listp (cadre-ui::lp-libraries page)) (cadre-ui::lp-libraries page))) :timeout 30)
  (let ((page (libraries-page)))
    (check "the Libraries tab lists Quicklisp's systems" (< 1000 (length (cadre-ui::lp-libraries page))))
    (check "and the system's dependencies: none yet"
           (member "No dependencies" (label-texts (cadre-ui::lp-dependencies page)) :test #'equal))
    (gtk:editable-set-text (cadre-ui::lp-search page) "alexandria")))

(then 500
  (let* ((page (libraries-page))
         (titles (label-texts (cadre-ui::lp-results page))))
    (check "searching finds a library" (member "alexandria" titles :test #'equal) (subseq titles 0 (min 6 (length titles))))
    (screenshot "libraries")
    (cadre-ui::add-library page "alexandria")))

(then-when ((not (cadre-ui::repl-busy cadre-ui::*repl*)) :timeout 60)
  (let ((text (uiop:read-file-string (smoke-asd))))
    (check "Add puts it in the .asd's :depends-on"
           (search "(defsystem \"smoke\" :depends-on (\"alexandria\") :components" text) text))
  (check "and lists it" (member "alexandria" (label-texts (cadre-ui::lp-dependencies (libraries-page))) :test #'equal))
  (check "and loads it" (search "alexandria" (repl-text)))
  (cadre-ui::remove-library (libraries-page) "alexandria"))

(then 300
  (check "Remove takes it out again" (string= *smoke-asd-text* (uiop:read-file-string (smoke-asd)))
         (uiop:read-file-string (smoke-asd))))

;;; The Threads page
(section "clos-browser")
(defvar *clos-smoke-ready* nil)
(defun clos-smoke-page ()
  (let ((buffer (find-buffer "CLOS Browser")))
    (and buffer (first (buffer-local buffer :clos-pages)))))
(defun clos-smoke-description ()
  (and (clos-smoke-page) (cadre-ui::cb-description (clos-smoke-page))))
(connect-lisp)

(then 50
  (let ((file (merge-pathnames "src/clos-demo.lisp" *root*)))
    (with-open-file (out file :direction :output :if-exists :supersede)
      (write-string "(defpackage :clos-smoke (:use :cl))
(in-package :clos-smoke)
(defclass document () ((path :initarg :path)))
(defclass text-document (document) ((text :initform \"\")))
(defgeneric save-document (document))
(defmethod save-document ((document document)) document)
(defmethod save-document :before ((document text-document)) nil)
(make-instance 'text-document :path \"sample.lisp\")" out))
    (cadre-ui::rex cadre-ui::*connection* (swank-call "swank:load-file" (namestring file))
                   :on-ok (lambda (reply) (declare (ignore reply)) (setf *clos-smoke-ready* t)))))

(then-when (*clos-smoke-ready* :timeout 30)
  (cadre-ui::open-clos-browser :name "text-document" :package "CLOS-SMOKE"))

(then-when ((getf (clos-smoke-description) :slots) :timeout 30)
  (let ((page (clos-smoke-page)) (description (clos-smoke-description)))
    (check "CLOS browser opens an editor tab" (member "CLOS Browser" (tab-titles) :test #'equal))
    (check "CLOS browser lists loaded classes and generic functions"
           (= 3 (length (getf (cadre-ui::cb-catalog page) :entries))))
    (check "CLOS browser includes inherited slots" (= 2 (length (getf description :slots))))
    (check "CLOS sidebar highlights the selected class"
           (equal "CLOS-SMOKE::TEXT-DOCUMENT"
                  (getf (gethash (gtk:list-box-get-selected-row (cadre-ui::cb-list page))
                                (cadre-ui::cb-rows page)) :name)))
    (check "CLOS browser resolves definition source"
           (equal (namestring (merge-pathnames "src/clos-demo.lisp" *root*))
                  (getf (parse-location (getf description :source)) :file)))
    (check "CLOS slots use compact table columns"
           (every (lambda (name) (member name (label-texts (cadre-ui::cb-details page)) :test #'equal))
                  '("Effective slot" "Type" "Defined by")))))

(then 300
  (screenshot "clos-browser")
  (let ((page (clos-smoke-page)))
    (setf (cadre-ui::cb-section page) "Methods")
    (cadre-ui::clos-render-detail page)
    (check "CLOS browser renders method qualifiers"
           (some (lambda (text) (search ":BEFORE" text :test #'char-equal)) (label-texts (cadre-ui::cb-details page))))
    (check "CLOS methods use compact table columns"
           (every (lambda (name) (member name (label-texts (cadre-ui::cb-details page)) :test #'equal))
                  '("Generic function" "Qualifier" "Specializers")))))

(then 300
  (screenshot "clos-browser-methods")
  (let ((page (clos-smoke-page)))
    (setf (cadre-ui::cb-section page) "Hierarchy")
    (cadre-ui::clos-render-detail page)
    (check "CLOS browser renders superclass links"
           (find "CLOS-SMOKE::DOCUMENT"
                 (find-widgets (cadre-ui::cb-details page) (lambda (w) (typep w 'gtk:button)))
                 :key #'gtk:widget-get-tooltip-text :test #'equal))))

(then 300
  (screenshot "clos-browser-hierarchy")
  (cadre-ui::clos-select-entry (clos-smoke-page) '(:kind :generic :name "CLOS-SMOKE::SAVE-DOCUMENT")))

(then-when ((eq :generic (getf (clos-smoke-description) :kind)) :timeout 20)
  (check "CLOS browser navigates to generic functions"
         (= 2 (length (getf (clos-smoke-description) :methods))))
  (check "CLOS Back is enabled after following a link"
         (gtk:widget-get-sensitive (cadre-ui::cb-back (clos-smoke-page))))
  (gtk:editable-set-text (cadre-ui::cb-search (clos-smoke-page)) "save")
  (gtk:widget-activate (cadre-ui::cb-back (clos-smoke-page))))

(then-when ((equal "CLOS-SMOKE::TEXT-DOCUMENT" (getf (clos-smoke-description) :name)))
  (check "CLOS Back restores the class and its detail tab"
         (and (equal "CLOS-SMOKE::TEXT-DOCUMENT" (getf (clos-smoke-description) :name))
              (equal "Hierarchy" (cadre-ui::cb-section (clos-smoke-page)))))
  (check "CLOS Back restores the filter"
         (equal "" (gtk:editable-get-text (cadre-ui::cb-search (clos-smoke-page)))))
  (check "CLOS Back is disabled at the beginning of history"
         (not (gtk:widget-get-sensitive (cadre-ui::cb-back (clos-smoke-page)))))
  (let* ((page (clos-smoke-page))
         (index (position "COMMON-LISP" (getf (cadre-ui::cb-catalog page) :packages) :test #'equal)))
    (gtk:drop-down-set-selected (cadre-ui::cb-packages page) index)))

(then-when ((and (equal "COMMON-LISP" (cadre-ui::cb-package (clos-smoke-page)))
                 (null (cadre-ui::cb-selected (clos-smoke-page)))))
  (gtk:widget-activate (cadre-ui::cb-back (clos-smoke-page))))

(then-when ((and (equal "CLOS-SMOKE" (cadre-ui::cb-package (clos-smoke-page)))
                 (equal "CLOS-SMOKE::TEXT-DOCUMENT" (getf (clos-smoke-description) :name))))
  (check "CLOS Back restores a previous package and definition"
         (and (equal "CLOS-SMOKE" (cadre-ui::cb-package (clos-smoke-page)))
              (equal "CLOS-SMOKE::TEXT-DOCUMENT" (getf (clos-smoke-description) :name))))
  (cadre-ui::clos-select-entry (clos-smoke-page) '(:kind :generic :name "CLOS-SMOKE::SAVE-DOCUMENT")))

(then-when ((eq :generic (getf (clos-smoke-description) :kind)))
  (cadre-ui::clos-inspect-selected (clos-smoke-page)))

(then-when ((search "SAVE-DOCUMENT" (text-string (cadre-ui::inspector-buffer)) :test #'char-equal) :timeout 20)
  (check "CLOS browser opens a live object in the Inspector"
         (and (search "GENERIC-FUNCTION" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
              (search "SAVE-DOCUMENT" (text-string (cadre-ui::inspector-buffer)) :test #'char-equal))
         (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*)))
  (cadre-ui::clos-open-class (clos-smoke-page) "CLOS-SMOKE::TEXT-DOCUMENT"))

(then-when ((eq :class (getf (clos-smoke-description) :kind)) :timeout 20)
  (cadre-ui::clos-goto-source (getf (clos-smoke-description) :source)))

(then-when ((equal "clos-demo.lisp" (buffer-name (current-buffer))) :timeout 20)
  (check "CLOS source navigation opens the definition" (equal "clos-demo.lisp" (buffer-name (current-buffer))))
  (cadre-ui::rex cadre-ui::*connection*
                 (swank-call "swank:interactive-eval-region"
                             "(defclass clos-smoke::text-document (clos-smoke::document)
 ((text :initform \"\") (encoding :initform :utf-8)))
(make-instance 'clos-smoke::text-document :path \"sample.lisp\")" 3 120)
                 :on-ok (lambda (reply) (declare (ignore reply)) (cadre-ui::image-changed))))

(then-when ((= 3 (length (getf (clos-smoke-description) :slots))) :timeout 20)
  (check "CLOS browser refreshes after class redefinition" (= 3 (length (getf (clos-smoke-description) :slots))))
  (call-command 'disconnect))

(then-when ((not (cadre-ui::connected-p)) :timeout 20)
  (check "CLOS browser clears stale image data on disconnect" (null (clos-smoke-description)))
  (check "CLOS browser clears navigation history on disconnect"
         (and (null (cadre-ui::cb-history (clos-smoke-page)))
              (not (gtk:widget-get-sensitive (cadre-ui::cb-back (clos-smoke-page)))))))

(section "threads")
(defun threads-page () cadre-ui::*threads-page*)
(defun shown-thread-names () (mapcar #'lisp-thread-name (cadre-ui::shown-threads (threads-page))))
(defun smoke-thread () (find "smoke-worker" (cadre-ui::thp-threads (threads-page)) :key #'lisp-thread-name :test #'string=))
(connect-lisp)

(then 100
  (cadre-ui::repl-eval "(sb-thread:make-thread (lambda () (loop (sleep 0.1))) :name \"smoke-worker\")")
  (call-command 'cadre-ui::show-threads))

(then-when ((smoke-thread) :timeout 20)
  (check "the Threads page lists the Lisp's threads"
         (and (member "smoke-worker" (shown-thread-names) :test #'string=)
              (member "repl-thread" (shown-thread-names) :test #'string=))
         (shown-thread-names))
  (check "but not Swank's own" (not (member "reader-thread" (shown-thread-names) :test #'string=)))
  (gtk:check-button-set-active (cadre-ui::thp-show-swank (threads-page)) t)
  (check "unless asked" (member "reader-thread" (shown-thread-names) :test #'string=))
  (gtk:check-button-set-active (cadre-ui::thp-show-swank (threads-page)) nil))

(then 300
  (screenshot "threads")
  (cadre-ui::debug-thread (smoke-thread)))

(then-when (cadre-ui::*debug-levels* :timeout 20)
  (check "Debug opens the debugger in the thread"
         (eql (lisp-thread-id (smoke-thread)) (cadre-ui::dl-thread (first cadre-ui::*debug-levels*)))
         (list (lisp-thread-id (smoke-thread)) (cadre-ui::dl-thread (first cadre-ui::*debug-levels*))))
  (check "saying it was interrupted" (equal "Interrupted" (first (cadre-ui::dl-condition (first cadre-ui::*debug-levels*)))))
  (call-command 'cadre-ui::debugger-continue))

(then-when ((null cadre-ui::*debug-levels*) :timeout 20)
  (check "and Continue lets it go on" (smoke-thread))
  (cadre-ui::kill-thread (smoke-thread)))

(then-when ((null (smoke-thread)) :timeout 20)
  (check "Kill ends it" t))

;;; The Tests page (the test explorer)
(section "test-explorer")
(defun tests-page () cadre-ui::*tests-page*)
(defun explorer-test (name) (find name (cadre-ui::tsp-tests (tests-page)) :key #'source-test-name :test #'string=))
(defun test-project () (merge-pathnames "smoke-tests/" *new-parent*))
(defun write-test-file (expected)
  (with-open-file (o (merge-pathnames "tests.lisp" (test-project)) :direction :output :if-exists :supersede)
    (write-string (test-file-text expected) o)))
(defun test-file-text (expected)
  (format nil "(defpackage #:smoke-tests (:use #:cl #:parachute))
(in-package #:smoke-tests)

(define-test arithmetic
  (is = 2 (+ 1 1)))

(define-test broken
  (is = ~d (+ 1 1)))

(define-test outer)

(define-test inner :parent outer
  (true t))
" expected))
(connect-lisp)
(load-quicklisp)

(use-root-project)

(then 100
  (ensure-directories-exist (test-project))
  (with-open-file (o (merge-pathnames "smoke-tests.asd" (test-project)) :direction :output :if-exists :supersede)
    (format o "(defsystem \"smoke-tests\" :depends-on (\"parachute\") :components ((:file \"tests\")))~%"))
  (write-test-file 3)
  (open-file-path (merge-pathnames "tests.lisp" (test-project))))

(then-when ((find-buffer "tests.lisp") :timeout 10)
  (call-command 'cadre-ui::show-test-explorer))

(then 300
  (check "the Tests page lists the tests in the source"
         (equal '("arithmetic" "broken" "outer" "inner") (mapcar #'source-test-name (cadre-ui::tsp-tests (tests-page))))
         (mapcar #'source-test-name (cadre-ui::tsp-tests (tests-page))))
  (check "before anything runs" (notany #'source-test-status (cadre-ui::tsp-tests (tests-page))))
  (call-command 'cadre-ui::run-all-tests))

(then-when ((and (not (cadre-ui::tsp-running (tests-page))) (source-test-status (explorer-test "arithmetic"))) :timeout 120)
  (check "Run All shows each test's result"
         (equal '(:passed :failed :passed :passed) (mapcar #'source-test-status (cadre-ui::tsp-tests (tests-page))))
         (mapcar #'source-test-status (cadre-ui::tsp-tests (tests-page))))
  (let ((run (cadre-ui::tsp-run (tests-page))))
    (check "Test Explorer records its failing selection in Runs"
           (and (eq :failed (cadre-ui::pr-status run))
                (search "broken" (cadre-ui::project-run-output-text run))
                (search "evaluated to" (cadre-ui::project-run-output-text run)))))
  (check "with what failed" (search "evaluated to" (or (first (source-test-messages (explorer-test "broken"))) ""))
         (source-test-messages (explorer-test "broken")))
  (check "and the tab counts the failures" (equal "Tests (1 ✗)" (panel-page-title "tests")) (panel-page-title "tests")))

(then 1100                              ; file times are to the second: let ASDF see the change
  (screenshot "test-explorer")
  ;; Fix the test in its tab, unsaved: Run Failed saves it first.
  (let ((buffer (find-buffer "tests.lisp")))
    (cadre-ui::replace-text-between (buffer-text buffer) 0 (length (buffer-string buffer)) (test-file-text 2)))
  (call-command 'cadre-ui::run-failed-tests))

(then-when ((and (not (cadre-ui::tsp-running (tests-page))) (eq :passed (source-test-status (explorer-test "broken")))) :timeout 60)
  (check "Run Failed saves and runs the fixed test again, loading the change"
         (eq :passed (source-test-status (explorer-test "broken")))
         (source-test-messages (explorer-test "broken")))
  (check "Run Failed records only its new successful selection"
         (eq :passed (cadre-ui::pr-status (cadre-ui::tsp-run (tests-page)))))
  (let ((row (loop for row being the hash-keys of cadre-ui::*test-rows* using (hash-value test)
                   when (equal "broken" (source-test-name test)) return row)))
    (when row (gtk:widget-activate row))))

(then-when ((find-buffer "tests.lisp") :timeout 10)
  (check "clicking a test goes to it" (= 6 (cadre-ui::cursor-line-column (current-view)))
         (cadre-ui::cursor-line-column (current-view))))

(section "runs-backends" :needs ("runs"))
(defvar *racket-explorer-run* nil)
(defvar *racket-explorer-process* nil)
(connect-lisp)
(then 100
  (write-root-file "repl-run/smoke-repl-run.asd"
                   "(defsystem \"smoke-repl-run\" :perform (test-op (op system) (format t \"repl-run-output-marker~%\")))")
  (cadre-ui::run-repl-test-system (merge-pathnames "repl-run/" *root*) "smoke-repl-run"))
(then-when ((not (cadre-ui::repl-busy cadre-ui::*repl*)) :timeout 60)
  (let ((run (first cadre-ui::*project-runs*)))
    (check "REPL test-system runs retain completion and output"
           (and (eq :passed (cadre-ui::pr-status run))
                (search "repl-run-output-marker" (cadre-ui::project-run-output-text run)))))
  (when *have-racket*
    (write-root-file "racket-explorer/main.rkt"
                     (format nil "#lang racket~%(require rackunit)~%(test-case \"works\" (check-equal? 2 2))~%(test-case \"broken\" (check-equal? 2 3))~%"))
    (cadre-ui::scan-project-tests cadre-ui::*tests-page* (merge-pathnames "racket-explorer/" *root*))
    (cadre-ui::run-tests-of (cadre-ui::tsp-tests cadre-ui::*tests-page*))))
(then-when ((or (not *have-racket*) (not (cadre-ui::tsp-running cadre-ui::*tests-page*))) :timeout 60)
  (when *have-racket*
    (setf *racket-explorer-run* (cadre-ui::tsp-run cadre-ui::*tests-page*))
    (check "Racket Test Explorer failures are retained in Runs"
           (and (eq :failed (cadre-ui::pr-status *racket-explorer-run*))
                (search "broken" (cadre-ui::project-run-output-text *racket-explorer-run*)))
           (list (cadre-ui::pr-status *racket-explorer-run*)
                 (cadre-ui::project-run-output-text *racket-explorer-run*)))
    (write-root-file "racket-explorer/main.rkt"
                     (format nil "#lang racket~%(require rackunit)~%(test-case \"works\" (check-equal? 2 2))~%(test-case \"broken\" (check-equal? 2 2))~%"))
    (cadre-ui::rerun-project-run *racket-explorer-run*)))
(then-when ((or (not *have-racket*) (not (cadre-ui::tsp-running cadre-ui::*tests-page*))) :timeout 60)
  (when *have-racket*
    (let ((run (cadre-ui::tsp-run cadre-ui::*tests-page*)))
      (check "Rerun finds the original Racket test selection in current source"
             (and (not (eq run *racket-explorer-run*)) (eq :passed (cadre-ui::pr-status run)))))
    (write-root-file "racket-explorer/main.rkt"
                     (format nil "#lang racket~%(require rackunit)~%(test-case \"wait\" (sleep 30) (check-true #t))~%"))
    (cadre-ui::scan-project-tests cadre-ui::*tests-page* (merge-pathnames "racket-explorer/" *root*))
    (cadre-ui::run-tests-of (cadre-ui::tsp-tests cadre-ui::*tests-page*))))
(then-when ((or (not *have-racket*) (cadre-ui::tsp-process cadre-ui::*tests-page*)) :timeout 10)
  (when *have-racket*
    (setf *racket-explorer-process* (cadre-ui::tsp-process cadre-ui::*tests-page*))
    (cadre-ui::stop-project-run (cadre-ui::tsp-run cadre-ui::*tests-page*))))
(then-when ((or (not *have-racket*) (not (sb-ext:process-alive-p *racket-explorer-process*))) :timeout 10)
  (when *have-racket*
    (check "Stop terminates a Racket test process and retains stopped status"
           (and (not (cadre-ui::tsp-running cadre-ui::*tests-page*))
                (eq :stopped (cadre-ui::pr-status (cadre-ui::tsp-run cadre-ui::*tests-page*)))))))

;;; Expanding macros in place
(section "macrostep")
(defun steps-buffer () (find-buffer "steps.lisp"))
(defun steps-count () (length (cadre-ui::macrosteps (steps-buffer))))
(use-root-project)
(connect-lisp)

(then 100
  (write-root-file "src/steps.lisp" (format nil "(defmacro step-outer (x) `(step-inner ,x))~%(defmacro step-inner (x) `(list ,x ,x))~%~%(step-outer 5)~%"))
  (open-file-path (merge-pathnames "src/steps.lisp" *root*)))

(then-when ((steps-buffer) :timeout 10)
  (show-buffer-named "steps.lisp")
  (call-command 'cadre-ui::load-file))

(then-when ((search "Loaded steps.lisp" (status-text)) :timeout 30)
  ;; An edit of the user's, to undo afterwards
  (let ((text (buffer-text (steps-buffer))))
    (gtk:text-buffer-insert text (gtk:text-buffer-get-end-iter text) "; edited" -1))
  (set-cursor 3 0)
  (call-command 'cadre-ui::macrostep-expand-in-place))

(then-when ((plusp (steps-count)) :timeout 20)
  (check "Expand in Place puts the expansion in the source" (search "(step-inner 5)" (string-downcase (line-text 3))) (line-text 3))
  (check "read-only while expanded" (not (gtk:text-view-get-editable (cadre-ui::view-text-view (current-view)))))
  (set-cursor 3 0)
  (call-command 'cadre-ui::macrostep-expand-in-place))

(then-when ((= 2 (steps-count)) :timeout 20)
  (check "expanding inside it steps further in" (search "(list 5 5)" (string-downcase (line-text 3))) (line-text 3))
  (screenshot "macrostep")
  (call-command 'cadre-ui::undo))

(then 300
  (check "Undo collapses the last expansion"
         (and (= 1 (steps-count)) (search "(step-inner 5)" (string-downcase (line-text 3)))) (line-text 3))
  (call-command 'cadre-ui::macrostep-collapse-all-expansions))

(then 300
  (check "Collapse All puts the form back" (string= "(step-outer 5)" (line-text 3)) (line-text 3))
  (check "and the buffer is editable again" (gtk:text-view-get-editable (cadre-ui::view-text-view (current-view))))
  (call-command 'cadre-ui::undo))

(then 300
  (check "the undo history from before is still there" (not (search "; edited" (buffer-string (steps-buffer)))))
  (set-cursor 3 0)
  (call-command 'cadre-ui::macrostep-expand-in-place))

(then-when ((plusp (steps-count)) :timeout 20)
  (call-command 'cadre-ui::save-buffer))

(then-when ((and (zerop (steps-count)) (not (buffer-modified-p (steps-buffer)))) :timeout 10)
  (check "saving collapses first: the file has the form, not its expansion"
         (search "(step-outer 5)" (uiop:read-file-string (merge-pathnames "src/steps.lisp" *root*)))))

;;; The Terminal page (VTE)
(section "terminal")

(defun term () cadre-ui::*current-terminal*)
(defun term-text () (if (term) (or (cadre-ui::terminal-text (term)) "") ""))
(defun terminal-count () (length (cadre-ui::live-terminals)))
(defvar *vte* nil)

(then 100
  (setf *vte* (cadre-ui::vte-available-p))
  (check "VTE is installed for the Terminal" *vte* (and (not *vte*) (cadre-ui::vte-missing-message)))
  ;; A plain shell, so the user's prompt and profile don't matter here.
  (setf cadre:*terminal-shell* "/bin/sh")
  ;; Earlier steps opened other projects; terminals start in the project's folder.
  (cadre-ui::open-project *root*)
  (call-command 'cadre-ui::show-terminal))

(then-when ((and *vte* (term) (cadre-ui::term-pid (term))) :timeout 10)
  (check "Show Terminal opens the Terminal page with a shell"
         (and (term) (string= "terminal" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*)))))
  (check "the terminal has the keyboard focus" (eq (term) (cadre-ui::focused-terminal *window*)))
  (check "it starts in the project's folder"
         (equal (truename *root*) (truename (cadre-ui::term-directory (term)))))
  (check "Ctrl+C goes to the terminal, not to Cadre" (null (press "C-c")))
  (check "Ctrl+P too (the shell's previous command)" (null (press "C-p")))
  (check "Escape too" (null (press "ESC")))
  (cadre-ui::feed-terminal (term) (format nil "echo cadre-$((6*7)) $TERM~c" #\Return)))

(then-when ((search "cadre-42 xterm-256color" (term-text)) :timeout 10)
  (check "commands run in the terminal, with TERM set" (search "cadre-42 xterm-256color" (term-text)) (term-text))
  (screenshot "44-terminal")
  (cadre-ui::feed-terminal (term) (format nil "printf 'src/hello.lisp:2:3\\n'~c" #\Return)))

(then-when ((search "src/hello.lisp:2:3" (term-text)) :timeout 10)
  (cadre-ui::open-terminal-link (term) "src/hello.lisp:2:3"))

(then-when ((and (find-buffer "hello.lisp") (eq (current-buffer) (find-buffer "hello.lisp"))) :timeout 10)
  (check "a file:line:column in the terminal opens there" (equal '(1 2) (cursor)) (cursor))
  (call-command 'cadre-ui::new-terminal))

(then-when ((= 2 (terminal-count)) :timeout 10)
  (check "New Terminal opens a second one, and selects it" (eq (term) (second cadre-ui::*terminals*)))
  (check "each has a tab" (= 2 (length (find-widgets cadre-ui::*terminal-tabs* (lambda (w) (typep w 'gtk:toggle-button))))))
  (cadre-ui::feed-terminal (term) (format nil "exit 3~c" #\Return)))

(then-when ((cadre-ui::term-exited (second cadre-ui::*terminals*)) :timeout 10)
  (check "a shell that fails stays, saying how it ended"
         (search "exited with code 3" (term-text)) (term-text))
  (check "and its tab says it ended" (search "(ended)" (gtk:button-get-label (cadre-ui::term-button (term)))))
  (call-command 'cadre-ui::kill-terminal))

(then 300
  (check "Kill Terminal closes it" (= 1 (length cadre-ui::*terminals*)))
  (check "the first terminal is current again" (eq (term) (first cadre-ui::*terminals*)))
  (with-open-file (o (merge-pathnames "run.sh" *root*) :direction :output :if-exists :supersede)
    (format o "echo sent-$((40+2))~%"))
  (open-file-path (merge-pathnames "run.sh" *root*)))

(then-when ((and (find-buffer "run.sh") (eq (current-buffer) (find-buffer "run.sh"))) :timeout 10)
  (set-cursor 0 0)
  (call-command 'cadre-ui::send-to-terminal))

(then-when ((search "sent-42" (term-text)) :timeout 10)
  (check "Send to Terminal runs the current line there" (search "sent-42" (term-text)))
  (call-command 'cadre-ui::claude-code-in-terminal))

(then-when ((= 2 (length cadre-ui::*terminals*)) :timeout 10)
  (check "Claude Code in a Terminal runs claude with Cadre's MCP tools"
         (and (string= "Claude Code" (cadre-ui::term-title (term)))
              (member "--mcp-config" (cadre-ui::term-command (term)) :test #'string=))
         (cadre-ui::term-command (term)))
  (call-command 'cadre-ui::kill-terminal))

(then 300
  (cadre-ui::feed-terminal (term) (format nil "exit~c" #\Return)))

(then-when ((null cadre-ui::*terminals*) :timeout 10)
  (check "a shell that exits cleanly closes its terminal" (null cadre-ui::*terminals*))
  (check "and the page offers a new one"
         (string= "empty" (gtk:stack-get-visible-child-name cadre-ui::*terminal-stack*))))

;;; Themes: no paren color may look like a token's, or a ) next to a string
;;; or keyword seems part of it.
(section "theme-colors")
(defun rgb-distance (a b)
  (flet ((rgb (hex) (loop for i from 1 below 7 by 2 collect (parse-integer hex :start i :end (+ i 2) :radix 16))))
    (sqrt (reduce #'+ (mapcar (lambda (x y) (expt (- x y) 2)) (rgb a) (rgb b))))))

(then 100
  (dolist (theme (cadre-ui::list-themes))
    (let ((collisions
            (loop for depth below cadre::*paren-face-count*
                  for paren = (getf (cadre-ui::theme-face (list :paren depth) theme) :foreground)
                  append (loop for face in '(:string :keyword :number :character :builtin :macro :definer
                                             :definition-name :special-variable :constant :lambda-keyword
                                             :quote :comment)
                               for color = (getf (cadre-ui::theme-face face theme) :foreground)
                               when (and paren color (< (rgb-distance paren color) 30))
                                 collect (list depth face)))))
      (check (format nil "~a: paren colors differ from token colors" (cadre-ui::theme-title theme))
             (null collisions) collisions))))

;;; Databases (databases.lisp): a SQLite file's connection, its tables in tabs, editing, and SQL
(section "databases")
(defvar *db-file* (merge-pathnames "data/app.db" *root*))
(defun db-page () (buffer-local (current-buffer) :db-table))
(defun db-query (sql)
  (let ((db (open-sqlite *db-file*)))
    (unwind-protect (db-result-rows (db-run db sql)) (db-close db))))
(defun db-connection-named (name)
  (find name cadre-ui::*db-connections* :key #'db-connection-name :test #'string=))
(defun results-label ()
  (gtk:label-get-text (cadre-ui::rp-label (cadre-ui::results-page))))

(then 50
  (ensure-directories-exist *db-file*)
  (let ((db (open-sqlite *db-file* :create t)))
    (db-run db "CREATE TABLE people (id INTEGER PRIMARY KEY, name TEXT NOT NULL, age INTEGER, notes TEXT);
INSERT INTO people (name, age, notes) VALUES ('Ann', 34, 'first'), ('Bo', 12, NULL), ('Cy', 51, 'two
lines');
CREATE TABLE log (message TEXT);
CREATE VIEW adults AS SELECT name, age FROM people WHERE age >= 18;")
    (db-close db))
  (call-command 'db-add-connection))

(then 300
  (check "Add Database Connection shows the Databases page"
         (string= "databases" (cadre-ui::sidebar-page *window*)))
  (let ((dialog cadre-ui::*db-connection-dialog*))
    (check "the dialog can't save without a file" (not (gtk:widget-get-sensitive (getf dialog :save))))
    (funcall (getf dialog :use-file) *db-file*)
    (check "choosing a file names the connection after it"
           (string= "app" (gtk:editable-get-text (getf dialog :name))))
    (check "and lets it save" (gtk:widget-get-sensitive (getf dialog :save)))
    (check "a connection is saved with the project when a folder is open"
           (adw:switch-row-get-active (getf dialog :in-project)))
    (gobject:emit (getf dialog :test) :clicked)))

(then-when ((plusp (length (gtk:label-get-text (getf cadre-ui::*db-connection-dialog* :note)))) :timeout 5)
  (let ((dialog cadre-ui::*db-connection-dialog*))
    (check "Test Connection opens it" (search "Connected: 3 tables" (gtk:label-get-text (getf dialog :note)))
           (gtk:label-get-text (getf dialog :note)))
    (screenshot "db-01-dialog")
    (funcall (getf dialog :save-it))))

(then 300
  (let ((connection (db-connection-named "app")))
    (check "the connection is added" connection)
    (check "kept in the project's .cadre/databases.sexp, relative to it"
           (let ((saved (read-db-connections (project-db-connections-file *root*) :project)))
             (and saved (string= "data/app.db" (db-connection-path (first saved)))))
           (uiop:read-file-string (project-db-connections-file *root*)))
    (cadre-ui::open-db-connection connection)))

(then 300
  (let ((texts (label-texts cadre-ui::*databases-list*)))
    (check "an open connection lists its tables and views"
           (every (lambda (name) (member name texts :test #'string=)) '("people" "log" "adults" "SQL Console"))
           texts))
  (screenshot "db-02-page")
  (cadre-ui::open-db-table (db-connection-named "app") "people"))

(then 300
  (check "a table opens in a tab" (member "people — app" (tab-titles) :test #'string=) (tab-titles))
  (let ((page (db-page)))
    (check "showing its rows" (= 3 (length (cadre-ui::dbt-rows page))))
    (check "and how many" (search "Rows 1–3 of 3" (gtk:label-get-text (cadre-ui::dbt-info page)))
           (gtk:label-get-text (cadre-ui::dbt-info page)))
    (check "a table with a key can be edited" (cadre-ui::dbt-editable-p page))
    (screenshot "db-03-table")
    ;; The age column's filter.
    (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 3 1) ">20")))

(then 700
  (let ((page (db-page)))
    (check "typing a filter filters the rows" (= 2 (cadre-ui::dbt-total page)) (cadre-ui::dbt-total page))
    (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 3 1) "")
    (gtk:widget-activate (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 3 1))
    (cadre-ui::sort-db-table page "age")
    (cadre-ui::sort-db-table page "age")
    (check "clicking a column's name twice sorts by it, descending"
           (equal '("Cy" "Ann" "Bo") (mapcar (lambda (row) (aref row 2)) (cadre-ui::dbt-rows page)))
           (cadre-ui::dbt-rows page))
    (cadre-ui::sort-db-table page "age")
    (cadre-ui::sort-db-table page "id")
    ;; Ann's age; a new row; Bo deleted.
    (cadre-ui::set-db-cell page 0 2 "35")
    (cadre-ui::add-db-row page)
    (cadre-ui::set-db-cell page 3 1 "Dee")
    (setf (gethash 1 (cadre-ui::dbt-checked page)) t)
    (cadre-ui::delete-checked-db-rows page)
    (check "changes mark the tab unsaved" (member "people — app ●" (tab-titles) :test #'string=) (tab-titles))
    (check "and show Save" (gtk:widget-get-visible (cadre-ui::dbt-save-button page)))
    (check "paging is refused until they're saved"
           (handler-case (progn (cadre-ui::load-db-table page) nil) (editor-error () t)))
    (screenshot "db-04-changes")
    (call-command 'save-buffer)))

(then 300
  (check "Save writes the changes"
         (equalp '(#("Ann" 35) #("Cy" 51) #("Dee" :null)) (db-query "SELECT name, age FROM people ORDER BY id"))
         (db-query "SELECT name, age FROM people ORDER BY id"))
  (check "and the tab is saved" (member "people — app" (tab-titles) :test #'string=) (tab-titles))
  (let ((page (db-page)))
    (cadre-ui::set-db-cell page 0 1 :null)
    (check "a NOT NULL column refuses NULL on Save"
           (not (cadre-ui::save-db-table page (lambda (ok) (declare (ignore ok))))))
    (check "leaving the database as it was"
           (equalp '(#("Ann")) (db-query "SELECT name FROM people WHERE id = 1")))
    (cadre-ui::discard-db-changes page)
    (check "Discard drops the changes" (not (buffer-modified-p (current-buffer))))
    (gtk:toggle-button-set-active
     (second (find-widgets (cadre-ui::view-widget (current-view))
                           (lambda (w) (and (typep w 'gtk:toggle-button) (equal "Structure" (gtk:button-get-label w))))))
     t)
    (screenshot "db-05-structure")
    (check "Structure shows the columns and the definition"
           (let ((texts (label-texts (cadre-ui::dbt-structure page))))
             (and (member "COLUMNS" texts :test #'string=) (find-if (lambda (s) (search "CREATE TABLE people" s)) texts)))))
  (cadre-ui::open-db-table (db-connection-named "app") "adults"))

(then 300
  (check "a view can't be edited" (not (cadre-ui::dbt-editable-p (db-page))))
  (cadre-ui::open-db-table (db-connection-named "app") "log"))

(then 300
  (check "a table without a key is edited by rowid" (equal '(:rowid) (cadre-ui::dbt-key (db-page))))
  (cadre-ui::new-sql-console (db-connection-named "app")))

(then 300
  (check "a SQL console is a SQL buffer" (eq 'sql-mode (buffer-major-mode (current-buffer))))
  (let ((names (mapcar #'first (cadre-ui::sql-completions "peo" (current-buffer)))))
    (check "SQL completion offers the connection's tables" (member "people" names :test #'string=) names))
  (check "a table's columns after its name and a dot"
         (equal '("people.id" "people.name" "people.age" "people.notes")
                (mapcar #'first (cadre-ui::sql-completions "people." (current-buffer)))))
  (check "and keywords in the case typed"
         (member "select" (mapcar #'first (cadre-ui::sql-completions "sel" (current-buffer))) :test #'string=))
  (insert-at-cursor "select name, age from people order by name;
select * from nowhere;
create table later (x)")
  (set-cursor 2 3)
  (call-command 'db-run-statement))

(then-when ((search "row" (results-label)) :timeout 10)
  (check "Run Statement runs the statement at the cursor" (search "3 rows" (results-label)) (results-label))
  (check "into the Results page" (string= "results" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
  (check "SQL is highlighted (with the SQL grammar installed)"
         (or (not (tree-sitter-language-installed-p "sql")) (buffer-local (current-buffer) :ts-document)))
  (screenshot "db-06-results")
  (set-cursor 1 3)
  (call-command 'db-run-statement))

(then-when ((search "row" (results-label)) :timeout 10)
  (check "a query says how long it took" (ppcre:scan "3 rows · \\d+ ms" (results-label)) (results-label))
  (set-cursor 3 3)
  (call-command 'db-run-statement))

(then-when ((search "nowhere" (results-label)) :timeout 10)
  (check "a SQL error shows there" (gtk:widget-has-css-class (cadre-ui::rp-label (cadre-ui::results-page)) "error"))
  (set-cursor 4 3)
  (call-command 'db-run-statement))

(then-when ((search "changed" (results-label)) :timeout 10)
  (check "a statement making a table lists it on the page"
         (member "later" (label-texts cadre-ui::*databases-list*) :test #'string=))
  (let ((connection (db-connection-named "app")))
    (cadre-ui::duplicate-db-connection connection)
    (check "Duplicate copies a connection" (db-connection-named "app copy"))
    (cadre-ui::close-db-connection connection)
    (check "Close closes its tables' tabs"
           (notany (lambda (title) (search "— app" title)) (tab-titles)) (tab-titles))))

;;; API requests (api.lisp): .http files, environments, secrets, curl, and responses by type
(section "api")
(defvar *api-port* nil)
(defun api-serve (socket)
  "Answer each request: /xml with XML, /html with HTML, anything else with JSON of what came."
  (loop
    (let ((client (handler-case (sb-bsd-sockets:socket-accept socket) (error () (return)))))
      (let ((stream (sb-bsd-sockets:socket-make-stream client :input t :output t :element-type :default
                                                              :external-format :utf-8 :buffering :full)))
        (ignore-errors
         (unwind-protect
              (let* ((line (string-right-trim '(#\Return) (read-line stream)))
                     (headers (loop for h = (string-right-trim '(#\Return) (read-line stream))
                                    until (string= h "")
                                    collect (let ((c (position #\: h)))
                                              (cons (string-downcase (subseq h 0 c)) (string-trim " " (subseq h (1+ c)))))))
                     (length (let ((l (cdr (assoc "content-length" headers :test #'string=)))) (if l (parse-integer l) 0)))
                     (body (let ((b (make-string length))) (read-sequence b stream) b))
                     (path (second (uiop:split-string line :separator " "))))
                (multiple-value-bind (type reply)
                    (cond ((search "/login" path) (values "application/json" "{\"token\": \"fresh123\", \"user\": {\"id\": 9}}"))
                          ((search "/xml" path) (values "application/xml" "<people><person id=\"1\"><name>Ann</name></person></people>"))
                          ((search "/html" path) (values "text/html" "<!doctype html><html><body><p>Hi</p></body></html>"))
                          (t (values "application/json"
                                     (json-string (jobj "path" path "auth" (or (cdr (assoc "authorization" headers :test #'string=)) "")
                                                        "cookie" (or (cdr (assoc "cookie" headers :test #'string=)) "")
                                                        "body" body)))))
                  (let ((octets (sb-ext:string-to-octets reply :external-format :utf-8)))
                    (format stream "HTTP/1.1 200 OK~c~cContent-Type: ~a~c~c~aContent-Length: ~d~c~c~c~c~a"
                            #\Return #\Newline type #\Return #\Newline
                            (if (search "/set-cookie" path) (format nil "Set-Cookie: visit=v42; Path=/~c~c" #\Return #\Newline) "")
                            (length octets) #\Return #\Newline #\Return #\Newline reply)))
                (finish-output stream))
           (close stream)))))))
(defun api-tab-page () (buffer-local (current-buffer) :api-page))
(defun api-status-text () (gtk:label-get-text (cadre-ui::ap-status (api-tab-page))))
(defun api-response-buffer () (view-buffer (cadre-ui::ap-response-view (api-tab-page))))

(then 50
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 5)
    (setf *api-port* (nth-value 1 (sb-bsd-sockets:socket-name socket)))
    (sb-thread:make-thread (lambda () (api-serve socket)) :name "smoke api server"))
  (with-open-file (o (merge-pathnames "http-client.env.json" *root*) :direction :output :if-exists :supersede)
    (format o "{\"dev\": {\"baseUrl\": \"http://127.0.0.1:~d\"}}~%" *api-port*))
  (ensure-directories-exist (merge-pathnames "api/" *root*))
  (with-open-file (o (merge-pathnames "api/people.http" *root*) :direction :output :if-exists :supersede)
    (format o "### Who am I~%POST {{baseUrl}}/me~%Authorization: Bearer {{token}}~%Content-Type: application/json~%~%{\"a\": [1, 2]}~%~%### People as XML~%GET {{baseUrl}}/xml~%~%### A page~%GET {{baseUrl}}/html~%~%### Log in~%# @capture token = $.token~%POST {{baseUrl}}/login~%"))
  (setf cadre-ui::*api-environment* "dev")
  (write-http-environments (cadre-ui::api-secrets-file) '(("dev" ("token" . "s3cret"))) :private t)
  (call-command 'show-api))

(then 300
  (check "the API page lists the project's .http files and their requests"
         (let ((texts (label-texts cadre-ui::*api-list*)))
           (and (member "api/people.http" texts :test #'string=) (member "Who am I" texts :test #'string=)))
         (label-texts cadre-ui::*api-list*))
  (check "and the environments" (string= "dev" (gtk:string-object-get-string
                                                  (gtk:drop-down-get-selected-item cadre-ui::*api-environment-dropdown*))))
  (check "secrets are kept outside the project, readable only by you"
         (and (not (uiop:string-prefix-p (namestring *root*) (namestring (cadre-ui::api-secrets-file))))
              (= #o600 (logand #o777 (sb-posix:stat-mode (sb-posix:stat (namestring (cadre-ui::api-secrets-file))))))))
  (let ((file (merge-pathnames "api/people.http" *root*)))
    (cadre-ui::open-api-request (first (parse-http-file (uiop:read-file-string file))) :file file :send t)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "a request opens in a tab" (member "Who am I" (tab-titles) :test #'string=) (tab-titles))
  (check "Send fills in the environment's variables and secrets"
         (search "Bearer s3cret" (buffer-string (api-response-buffer))) (buffer-string (api-response-buffer)))
  (check "a JSON response is laid out, as JSON" (and (eq 'json-mode (buffer-major-mode (api-response-buffer)))
                                                    (search (format nil "{~%  \"path\"") (buffer-string (api-response-buffer)))))
  (check "the status says how long it took" (ppcre:scan "^200 OK · \\d+ ms · \\d+ B$" (api-status-text)) (api-status-text))
  (check "and the request is still saved with {{variables}}"
         (search "{{token}}" (uiop:read-file-string (merge-pathnames "api/people.http" *root*))))
  (screenshot "api-01-json")
  (let ((file (merge-pathnames "api/people.http" *root*)))
    (cadre-ui::open-api-request (second (parse-http-file (uiop:read-file-string file))) :file file :send t)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "an XML response is laid out, as XML"
         (and (eq 'xml-mode (buffer-major-mode (api-response-buffer)))
              (search (format nil "<people>~%  <person") (buffer-string (api-response-buffer))))
         (buffer-string (api-response-buffer)))
  (gtk:toggle-button-set-active (cadre-ui::ap-pretty-button (api-tab-page)) nil)
  (check "Pretty off shows it as it came"
         (search "<people><person" (buffer-string (api-response-buffer))))
  (let ((file (merge-pathnames "api/people.http" *root*)))
    (cadre-ui::open-api-request (third (parse-http-file (uiop:read-file-string file))) :file file :send t)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "an HTML response is shown as HTML" (eq 'html-mode (buffer-major-mode (api-response-buffer))))
  (screenshot "api-02-html")
  ;; A new request with a token typed in.
  (cadre-ui::open-api-request
   (parse-curl-command (format nil "curl http://127.0.0.1:~d/new -H 'Authorization: Bearer abc123'" *api-port*))))

(then 300
  (check "a pasted curl command opens as a request, unsaved"
         (and (buffer-modified-p (current-buffer))
              (string= "Bearer abc123" (http-request-header (cadre-ui::page-request (api-tab-page)) "Authorization"))))
  (call-command 'save-buffer))

(then 300
  (let ((dialog cadre-ui::*api-save-dialog*))
    (check "Save asks where in the project it goes"
           (member (truename (merge-pathnames "api/people.http" *root*)) (getf dialog :paths)
                   :test (lambda (a b) (equal a (probe-file b))))
           (getf dialog :paths))
    (gtk:editable-set-text (getf dialog :name) "New one")
    (funcall (getf dialog :save-it))))

(then 300
  (check "and offers to move the token out of it" cadre-ui::*api-secrets-dialog*)
  (funcall (getf cadre-ui::*api-secrets-dialog* :answer) "move")
  (adw:dialog-close (getf cadre-ui::*api-secrets-dialog* :dialog)))

(then 300
  (let ((text (uiop:read-file-string (merge-pathnames "api/people.http" *root*))))
    (check "Move to Secrets leaves a {{variable}} in the file"
           (and (search "### New one" text) (search "Bearer {{token}}" text) (not (search "abc123" text))) text))
  (check "and puts the token in the secrets"
         (string= "abc123" (cdr (assoc "token" (http-environment-variables (read-http-environments (cadre-ui::api-secrets-file)) "dev")
                                       :test #'string=))))
  (check "the tab is saved, and named" (member "New one" (tab-titles) :test #'string=) (tab-titles))
  (check "the page lists it" (member "New one" (label-texts cadre-ui::*api-list*) :test #'string=)))

(then 100
  (call-command 'api-new-request))

(then 300
  (let* ((page (api-tab-page))
         (url (cadre-ui::ap-url page)))
    (gtk:editable-set-text url "{{baseUrl}}/search?q=two%20words&page=1")
    (check "typing a URL with a query fills in Params"
           (equal '(("q" . "two words") ("page" . "1")) (cadre-ui::box-pairs (cadre-ui::ap-params-box page)))
           (cadre-ui::box-pairs (cadre-ui::ap-params-box page)))
    (let ((row (cadre-ui::add-param-row page)))
      (gtk:editable-set-text (gtk:widget-get-first-child row) "tag")
      (gtk:editable-set-text (gtk:widget-get-next-sibling (gtk:widget-get-first-child row)) "a&b"))
    (check "a parameter added in Params goes into the URL, encoded"
           (string= "{{baseUrl}}/search?q=two%20words&page=1&tag=a%26b" (gtk:editable-get-text url))
           (gtk:editable-get-text url))
    (let ((stack (loop for w = (cadre-ui::ap-params-box page) then (gtk:widget-get-parent w)
                       until (typep w 'gtk:stack) finally (return w))))
      (gtk:stack-set-visible-child-name stack "params"))))

(then 300
  (gtk:widget-activate (gtk:widget-get-last-child (first (cadre-ui::box-rows (cadre-ui::ap-params-box (api-tab-page)))))))

(then 400
  (let ((url (cadre-ui::ap-url (api-tab-page))))
    (check "removing one takes it out of the URL"
           (string= "{{baseUrl}}/search?page=1&tag=a%26b" (gtk:editable-get-text url)) (gtk:editable-get-text url)))
  (screenshot "api-03-params")
  (let ((file (merge-pathnames "api/people.http" *root*)))
    (cadre-ui::open-api-request (find "Log in" (parse-http-file (uiop:read-file-string file)) :key #'http-request-name :test #'string=)
                                :file file :send t)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "a request's captures take values from its response"
         (string= "fresh123" (cdr (assoc "token" cadre-ui::*api-captured* :test #'string=))) cadre-ui::*api-captured*)
  (check "and say so" (search "captured {{token}}" (api-status-text)) (api-status-text))
  (check "the page lists what was captured" (member "{{token}}" (label-texts cadre-ui::*api-list*) :test #'string=))
  (let ((file (merge-pathnames "api/people.http" *root*)))
    (cadre-ui::open-api-request (first (parse-http-file (uiop:read-file-string file))) :file file)
    (cadre-ui::send-api-request (api-tab-page))))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "a captured value wins over the secret of the same name"
         (search "Bearer fresh123" (buffer-string (api-response-buffer))) (buffer-string (api-response-buffer))))

(then 100
  (let ((auth (cadre-ui::ap-auth (api-tab-page))))
    (check "the Auth tab sees a request's Bearer {{token}}"
           (and (eq :bearer (funcall (getf auth :chosen)))
                (string= "token" (gtk:editable-get-text (getf auth :variable))))))
  (cadre-ui::open-api-request (make-http-request :method "POST" :url (format nil "http://127.0.0.1:~d/auth" *api-port*))))

(then 300
  (let* ((page (api-tab-page))
         (auth (cadre-ui::ap-auth page)))
    (adw:combo-row-set-selected (getf auth :kind) 1)
    (gtk:editable-set-text (getf auth :variable) "api_token")
    (gtk:editable-set-text (getf auth :secret) "tok999")
    (cadre-ui::use-auth page)
    (check "Use sets the header with a {{variable}}"
           (equal '(("Authorization" . "Bearer {{api_token}}")) (cadre-ui::box-pairs (cadre-ui::ap-headers-box page)))
           (cadre-ui::box-pairs (cadre-ui::ap-headers-box page)))
    (check "and keeps the token in the secrets, not the request"
           (string= "tok999" (cdr (assoc "api_token" (http-environment-variables
                                                      (read-http-environments (cadre-ui::api-secrets-file)) "dev")
                                         :test #'string=))))
    (check "clearing the field" (string= "" (gtk:editable-get-text (getf auth :secret))))
    (adw:combo-row-set-selected (getf auth :kind) 2)
    (gtk:editable-set-text (getf auth :variable) "basic_auth")
    (gtk:editable-set-text (getf auth :user) "ann")
    (gtk:editable-set-text (getf auth :secret) "pw")
    (cadre-ui::use-auth page)
    (check "Basic replaces it, keeping user:password encoded"
           (and (equal '(("Authorization" . "Basic {{basic_auth}}")) (cadre-ui::box-pairs (cadre-ui::ap-headers-box page)))
                (string= (base64-string "ann:pw")
                         (cdr (assoc "basic_auth" (http-environment-variables
                                                   (read-http-environments (cadre-ui::api-secrets-file)) "dev")
                                     :test #'string=)))))
    (cadre-ui::send-api-request page)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "and the request sends it" (search (format nil "Basic ~a" (base64-string "ann:pw")) (buffer-string (api-response-buffer)))
         (buffer-string (api-response-buffer)))
  (let ((stack (loop for w = (cadre-ui::ap-headers-box (api-tab-page)) then (gtk:widget-get-parent w)
                     until (typep w 'gtk:stack) finally (return w))))
    (gtk:stack-set-visible-child-name stack "auth"))
  (screenshot "api-04-auth"))

(then 100
  (cadre-ui::open-api-request (make-http-request :method "POST" :url (format nil "http://127.0.0.1:~d/form" *api-port*))))

(then 300
  (let ((page (api-tab-page)))
    (gtk:drop-down-set-selected (cadre-ui::ap-body-mode-dropdown page) 1)
    (cadre-ui::add-form-row page "name" "Ann Bo")
    (cadre-ui::add-form-row page "x" "1&2")
    (check "a Form body is its fields, encoded, with its Content-Type"
           (and (string= "name=Ann%20Bo&x=1%262" (http-request-body (cadre-ui::page-request page)))
                (string= "application/x-www-form-urlencoded"
                         (http-request-header (cadre-ui::page-request page) "Content-Type")))
           (cadre-ui::page-request page))
    (cadre-ui::send-api-request page)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "and is sent so" (search "name=Ann%20Bo&x=1%262" (buffer-string (api-response-buffer))))
  (with-open-file (o (merge-pathnames "api/upload.txt" *root*) :direction :output :if-exists :supersede)
    (write-string "uploaded text" o))
  (cadre-ui::open-api-request (make-http-request :name "Upload" :method "POST"
                                                 :url (format nil "http://127.0.0.1:~d/upload" *api-port*))))

(then 300
  (let ((page (api-tab-page)))
    (gtk:drop-down-set-selected (cadre-ui::ap-body-mode-dropdown page) 2)
    (let ((first-row (first (cadre-ui::box-rows (cadre-ui::ap-multipart-box page)))))
      (destructuring-bind (name value file) (cdr (assoc first-row (cadre-ui::ap-multipart-rows page)))
        (declare (ignore file))
        (gtk:editable-set-text name "note")
        (gtk:editable-set-text value "hello")))
    (cadre-ui::add-multipart-row page :name "doc" :value (namestring (merge-pathnames "api/upload.txt" *root*)) :file t)
    (check "Multipart sets its Content-Type"
           (search "multipart/form-data" (http-request-header (cadre-ui::page-request page) "Content-Type")))
    (cadre-ui::send-api-request page)))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (let ((echoed (buffer-string (api-response-buffer))))
    (check "a multipart request sends its text parts and files"
           (and (search "name=\\\"note\\\"" echoed) (search "hello" echoed)
                (search "filename=\\\"upload.txt\\\"" echoed) (search "uploaded text" echoed))
           echoed))
  (let ((page (api-tab-page)))
    (setf (cadre-ui::ap-name page) "Upload")
    (cadre-ui::write-api-request page (merge-pathnames "api/people.http" *root*) (cadre-ui::page-request page)))
  (let ((text (uiop:read-file-string (merge-pathnames "api/people.http" *root*))))
    (check "saved, its file is beside the .http file: < ./upload.txt" (search "< ./upload.txt" text) text)))

(then 100
  (cadre-ui::open-api-request (make-http-request :url (format nil "http://127.0.0.1:~d/set-cookie" *api-port*)) :send t))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "a cookie the server sets is kept for the environment"
         (equal '("visit") (mapcar (lambda (c) (getf c :name)) (read-cookie-jar (cadre-ui::api-cookie-jar)))))
  (check "outside the project" (not (search (namestring *root*) (namestring (cadre-ui::api-cookie-jar)))))
  (check "and listed on the page" (find-if (lambda (s) (search "visit ·" s)) (label-texts cadre-ui::*api-list*)))
  (cadre-ui::open-api-request (make-http-request :url (format nil "http://127.0.0.1:~d/after" *api-port*)) :send t))

(then-when ((search "200" (api-status-text)) :timeout 15)
  (check "and sent back with the next request" (search "visit=v42" (buffer-string (api-response-buffer)))
         (buffer-string (api-response-buffer)))
  (uiop:delete-file-if-exists (cadre-ui::api-cookie-jar))
  (cadre-ui::refresh-api-page)
  (check "Clear forgets them" (notany (lambda (s) (search "visit ·" s)) (label-texts cadre-ui::*api-list*))))

;;; PostgreSQL on the Databases page, against a server started here (skipped
;;; where PostgreSQL isn't installed)
(section "postgres" :needs ("databases"))
(defvar *pg-bin* (let ((d (probe-file "/opt/homebrew/opt/postgresql@17/bin/"))) (and d (namestring d))))
(defvar *pg-port* nil)
(defvar *pg-data* nil)

(then 50
  (when *pg-bin*
    (let* ((dir (merge-pathnames "pg/" *root*))
           (data (merge-pathnames "data/" dir))
           (pw (merge-pathnames "pw" dir))
           (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
      (setf *pg-port* (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket)
      (ensure-directories-exist dir)
      (with-open-file (o pw :direction :output) (write-line "secret" o))
      (uiop:run-program (list (concatenate 'string *pg-bin* "initdb") "-D" (namestring data) "-U" "cadre"
                              "--auth=scram-sha-256" (format nil "--pwfile=~a" (namestring pw)) "-E" "UTF8" "--no-locale")
                        :output nil :error-output nil)
      (sb-posix:setenv "LC_ALL" "en_US.UTF-8" 1)
      (uiop:run-program (list (concatenate 'string *pg-bin* "pg_ctl") "-D" (namestring data) "-w" "-l" (namestring (merge-pathnames "log" dir))
                              "-o" (format nil "-p ~d -k /tmp -c listen_addresses=127.0.0.1" *pg-port*) "start")
                        :output nil :error-output nil)
      (setf *pg-data* data)
      (push (lambda () (ignore-errors (uiop:run-program (list (concatenate 'string *pg-bin* "pg_ctl") "-D" (namestring data)
                                                              "-m" "immediate" "stop")
                                                        :output nil :error-output nil)))
            sb-ext:*exit-hooks*)
      (let ((db (open-postgres :host "127.0.0.1" :port *pg-port* :user "cadre" :password "secret")))
        (db-run db "CREATE TABLE orders (id serial PRIMARY KEY, item text NOT NULL, qty integer, placed date DEFAULT current_date);
INSERT INTO orders (item, qty) VALUES ('Apple', 3), ('banana', 12), ('Cherry', 7);")
        (db-close db))
      (call-command 'db-add-connection))))

(then 300
  (when *pg-bin*
    (let ((dialog cadre-ui::*db-connection-dialog*))
      (adw:combo-row-set-selected (getf dialog :kind) 1)
      (check "choosing PostgreSQL asks for a server, not a file"
             (and (gtk:widget-get-visible (getf dialog :host)) (gtk:widget-get-visible (getf dialog :password))))
      (gtk:editable-set-text (getf dialog :name) "shop")
      (gtk:editable-set-text (getf dialog :host) "127.0.0.1")
      (gtk:editable-set-text (getf dialog :port) (princ-to-string *pg-port*))
      (gtk:editable-set-text (getf dialog :user) "cadre")
      (gtk:editable-set-text (getf dialog :password) "secret")
      (adw:switch-row-set-active (getf dialog :in-project) t)
      (gobject:emit (getf dialog :test) :clicked))))

(then-when ((or (null *pg-bin*) (plusp (length (gtk:label-get-text (getf cadre-ui::*db-connection-dialog* :note))))) :timeout 10)
  (when *pg-bin*
    (let ((dialog cadre-ui::*db-connection-dialog*))
      (check "Test Connection reaches the server" (search "PostgreSQL" (gtk:label-get-text (getf dialog :note)))
             (gtk:label-get-text (getf dialog :note)))
      (screenshot "pg-01-dialog")
      (funcall (getf dialog :save-it)))))

(then 300
  (when *pg-bin*
    (let ((saved (uiop:read-file-string (project-db-connections-file *root*))))
      (check "the project's connection file has the server, not the password"
             (and (search "127.0.0.1" saved) (not (search "secret" saved))) saved))
    (check "the password is kept outside the project"
           (string= "secret" (read-db-password (cadre-ui::db-passwords-file)
                                               (db-connection-id (db-connection-named "shop")))))
    (cadre-ui::open-db-connection (db-connection-named "shop"))
    (cadre-ui::open-db-table (db-connection-named "shop") "orders")))

(then 400
  (when *pg-bin*
    (let ((page (db-page)))
      (check "a PostgreSQL table opens in a tab" (= 3 (length (cadre-ui::dbt-rows page))))
      (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 2 1) "an"))))

(then 700
  (when *pg-bin*
    (let ((page (db-page)))
      (check "a filter finds text in any case" (= 1 (cadre-ui::dbt-total page)) (cadre-ui::dbt-rows page))
      (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 2 1) "")
      (cadre-ui::filter-db-table page)
      (check "rows come in key order" (equal '(1 2 3) (mapcar (lambda (r) (aref r 0)) (cadre-ui::dbt-rows page))))
      (cadre-ui::set-db-cell page 0 2 "4")
      (call-command 'save-buffer))))

(then 400
  (when *pg-bin*
    (let ((db (open-postgres :host "127.0.0.1" :port *pg-port* :user "cadre" :password "secret")))
      (check "editing a cell and saving writes it to the server"
             (equalp '(#(4)) (db-result-rows (db-run db "SELECT qty FROM orders WHERE item = 'Apple'")))
             (list (db-result-rows (db-run db "SELECT item, qty FROM orders ORDER BY id"))
                   (gtk:label-get-text (cadre-ui::window-status-message *window*))))
      (db-close db))
    (screenshot "pg-02-table")
    (cadre-ui::close-db-connection (db-connection-named "shop"))
    ;; Stop the server now: the run deletes its folder on the way out.
    (uiop:run-program (list (concatenate 'string *pg-bin* "pg_ctl") "-D" (namestring *pg-data*) "-m" "fast" "stop")
                      :output nil :error-output nil :ignore-error-status t)))

;;; MySQL / MariaDB on the Databases page, against a MariaDB server started
;;; here (skipped where MariaDB isn't installed)
(section "mysql" :needs ("databases"))
(defvar *mdb-bin* (let ((d (probe-file "/opt/homebrew/opt/mariadb/bin/"))) (and d (namestring d))))
(defvar *mdb-port* nil)
(defvar *mdb-process* nil)

(then 50
  (when *mdb-bin*
    (let* ((dir (uiop:ensure-directory-pathname (format nil "/tmp/cmdb-~d/" (random 1000000))))
           (data (merge-pathnames "data/" dir))
           (sock (namestring (merge-pathnames "s.sock" dir)))
           (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
      (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
      (setf *mdb-port* (nth-value 1 (sb-bsd-sockets:socket-name socket)))
      (sb-bsd-sockets:socket-close socket)
      (ensure-directories-exist dir)
      (uiop:run-program (list (concatenate 'string *mdb-bin* "mariadb-install-db") (format nil "--datadir=~a" (namestring data))
                              "--auth-root-authentication-method=socket" "--skip-test-db")
                        :output nil :error-output nil)
      (setf *mdb-process* (uiop:launch-program (list (concatenate 'string *mdb-bin* "mariadbd") (format nil "--datadir=~a" (namestring data))
                                                     (format nil "--port=~d" *mdb-port*) "--bind-address=127.0.0.1"
                                                     (format nil "--socket=~a" sock) "--skip-name-resolve")
                                               :output nil :error-output nil))
      (let ((process *mdb-process*))
        (push (lambda () (ignore-errors (uiop:terminate-process process))
                (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore))
              sb-ext:*exit-hooks*))
      (loop repeat 100 until (probe-file sock) do (sleep 0.1))
      (uiop:run-program (list (concatenate 'string *mdb-bin* "mariadb") "-S" sock "-u" (uiop:getenv "USER") "-e"
                              "CREATE USER 'cadre'@'%' IDENTIFIED BY 'secret'; GRANT ALL ON *.* TO 'cadre'@'%'; CREATE DATABASE shop;
CREATE TABLE shop.items (id int AUTO_INCREMENT PRIMARY KEY, item varchar(20) NOT NULL, qty int, added date);
INSERT INTO shop.items (item, qty, added) VALUES ('Apple', 3, '2026-01-02'), ('banana', 12, NULL), ('Cherry', 7, '2026-03-04');")
                        :output nil :error-output nil)
      (call-command 'db-add-connection))))

(then 300
  (when *mdb-bin*
    (let ((dialog cadre-ui::*db-connection-dialog*))
      (adw:combo-row-set-selected (getf dialog :kind) 2)
      (check "MySQL / MariaDB takes its usual port" (string= "3306" (gtk:editable-get-text (getf dialog :port))))
      (gtk:editable-set-text (getf dialog :name) "maria")
      (gtk:editable-set-text (getf dialog :host) "127.0.0.1")
      (gtk:editable-set-text (getf dialog :port) (princ-to-string *mdb-port*))
      (gtk:editable-set-text (getf dialog :database) "shop")
      (gtk:editable-set-text (getf dialog :user) "cadre")
      (gtk:editable-set-text (getf dialog :password) "secret")
      (gobject:emit (getf dialog :test) :clicked))))

(then-when ((or (null *mdb-bin*) (plusp (length (gtk:label-get-text (getf cadre-ui::*db-connection-dialog* :note))))) :timeout 10)
  (when *mdb-bin*
    (let ((dialog cadre-ui::*db-connection-dialog*))
      (check "Test Connection reaches MariaDB" (search "MariaDB" (gtk:label-get-text (getf dialog :note)))
             (gtk:label-get-text (getf dialog :note)))
      (funcall (getf dialog :save-it)))))

(then 300
  (when *mdb-bin*
    (cadre-ui::open-db-connection (db-connection-named "maria"))
    (cadre-ui::open-db-table (db-connection-named "maria") "items")))

(then 400
  (when *mdb-bin*
    (let ((page (db-page)))
      (check "a MariaDB table opens in a tab, dates as text"
             (and (= 3 (length (cadre-ui::dbt-rows page)))
                  (string= "2026-01-02" (aref (first (cadre-ui::dbt-rows page)) 4)))
             (cadre-ui::dbt-rows page))
      (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 2 1) "AN")
      (cadre-ui::filter-db-table page)
      (check "a filter finds text in any case" (= 1 (cadre-ui::dbt-total page)) (cadre-ui::dbt-rows page))
      (gtk:editable-set-text (gtk:grid-get-child-at (cadre-ui::dbt-grid page) 2 1) "")
      (cadre-ui::filter-db-table page)
      (cadre-ui::set-db-cell page 0 1 "Apple's")
      (call-command 'save-buffer))))

(then 400
  (when *mdb-bin*
    (let ((db (open-mysql :host "127.0.0.1" :port *mdb-port* :user "cadre" :password "secret" :database "shop")))
      (check "editing a cell (with a quote in it) and saving writes it to the server"
             (equalp '(#("Apple's")) (db-result-rows (db-run db "SELECT item FROM items WHERE id = 1")))
             (db-result-rows (db-run db "SELECT item FROM items ORDER BY id")))
      (db-close db))
    (screenshot "mysql-01-table")
    (cadre-ui::close-db-connection (db-connection-named "maria"))
    (ignore-errors (uiop:terminate-process *mdb-process*))))

(let* ((only (or (uiop:getenv "ONLY") ""))
       (sections (selected-sections only)))
  (when (uiop:getenv "SMOKE_LIST")
    (loop for (name . needs) in *sections*
          do (format t "~&~a~@[  (needs ~{~a~^, ~})~]~%" name needs))
    (uiop:quit 0))
  (unless (string= only "")
    (format t "~&Smoke sections: ~{~a~^ ~}~%" sections))
  (setf *steps* (mapcar #'cdr (remove-if-not (lambda (step) (member (car step) sections :test #'string=))
                                             (reverse *steps*)))))

;;; Run, with a fresh config directory so first-run questions are skipped.
(let ((config (merge-pathnames (format nil "cadre-smoke-config-~d-~d/" (get-universal-time) (sb-posix:getpid))
                                (uiop:temporary-directory))))
  (ensure-directories-exist (merge-pathnames "cadre/" config))
  (with-open-file (o (merge-pathnames "cadre/settings.sexp" config) :direction :output)
    (prin1 '(:keybinding-profile :standard) o))
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config) 1)
  (sb-posix:setenv "XDG_STATE_HOME" (namestring (merge-pathnames "state/" config)) 1))

(glib:timeout-add glib:+priority-default+ 100 (lambda () (run-steps *steps*) nil))
(cadre-ui:main :project *root* :init-file nil :quit-after 900)
(format t "~&Timed out.~%")
(uiop:quit 1)
