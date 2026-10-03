;;;; smoke.lisp — drive a real Cadre window through the M0 features:
;;;;   make smoke
;;;; Uses a throwaway project and config directory, checks each step, saves
;;;; screenshots to build/smoke/, prints a report, and exits 0 if all passed.

(push (truename ".") asdf:*central-registry*)
(push (truename "../gtk4/") asdf:*central-registry*)
(ql:quickload :cadre :silent t)

(defpackage #:cadre-smoke (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-smoke)

(defvar *out* (merge-pathnames "build/smoke/" (truename ".")))
(defvar *root* (merge-pathnames (format nil "cadre-smoke-~d/" (get-universal-time))
                                (uiop:temporary-directory)))
(defvar *results* '())
(defvar *steps* '())

(defun check (name ok &optional detail)
  (push (list name (and ok t) detail) *results*)
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok name detail))

(defvar *paintable* nil)

(defun screenshot (name)
  "Render the window to build/smoke/NAME.png."
  (let* ((window (cadre-ui::window-gtk-window *window*))
         (w (gtk:widget-get-width window))
         (h (gtk:widget-get-height window))
         (paintable (or *paintable* (setf *paintable* (gtk:widget-paintable-new window))))
         (snapshot (gtk:snapshot-new)))
    (gdk:paintable-snapshot paintable snapshot (float w 1d0) (float h 1d0))
    (let* ((node (gtk:snapshot-to-node snapshot))
           (renderer (gtk:native-get-renderer window))
           (texture (gsk:renderer-render-texture renderer node nil))
           (path (merge-pathnames (format nil "~a.png" name) *out*)))
      (ensure-directories-exist path)
      (gdk:texture-save-to-png texture (namestring path))
      path)))

(defun press (keys)
  "Type KEYS (such as \"C-s\") through Cadre's key handling."
  (dolist (key (parse-keys keys))
    (let* ((mods (cadre-ui::key-modifiers key))
           (name (subseq key (* 2 (length mods))))
           (keyval (gdk:keyval-from-name (cond ((string= name "TAB") "Tab")
                                               ((string= name "RET") "Return")
                                               ((string= name "ESC") "Escape")
                                               (t name))))
           (state (loop for m in mods
                        collect (ecase m (#\C :control-mask) (#\M :alt-mask)
                                  (#\s :super-mask) (#\S :shift-mask)))))
      (cadre-ui::handle-key *window* keyval state))))

(defun has-face-p (line column face)
  "True if the current buffer's text at (LINE, COLUMN) has FACE's tag."
  (let* ((gtk-buffer (buffer-text (current-buffer)))
         (tag (cadre-ui::face-tag gtk-buffer face)))
    (gtk:text-iter-has-tag (cadre-ui::line-iter gtk-buffer line column) tag)))

(defun cursor ()
  (multiple-value-list (cadre-ui::cursor-line-column (current-view))))

(defun set-cursor (line column)
  (cadre-ui::goto-line-column (current-view) line column :extend nil))

(defun line-text (line)
  (text-line-string (buffer-text (current-buffer)) line))

(defun picker () (cadre-ui::window-picker-object *window*))

(defmacro timed (&body body)
  `(let ((start (get-internal-real-time)))
     ,@body
     (round (* 1000 (- (get-internal-real-time) start)) internal-time-units-per-second)))

(defun insert-at-cursor (string)
  (gtk:text-buffer-insert-at-cursor (buffer-text (current-buffer)) string -1))

(defun cadre-ensure-all ()
  (let ((syntax (cadre-ui::buffer-syntax (current-buffer))))
    (ensure-lexed syntax (1- (syntax-line-count syntax)))))

(defun show-buffer-named (name)
  (cadre-ui::show-buffer *window* (find-buffer name)))

(defun repl-text ()
  (buffer-string (cadre-ui::repl-buffer cadre-ui::*repl*)))

(defun tab-titles ()
  (mapcar #'adw:tab-page-get-title (cadre-ui::window-pages *window*)))

(defun find-widgets (root predicate)
  "Every widget under ROOT (inclusive) satisfying PREDICATE."
  (let ((found '()))
    (labels ((walk (w)
               (when (funcall predicate w) (push w found))
               (loop for c = (gtk:widget-get-first-child w) then (gtk:widget-get-next-sibling c)
                     while c do (walk c))))
      (walk root))
    (nreverse found)))

(defun label-texts (root)
  (mapcar #'gtk:label-get-text (find-widgets root (lambda (w) (typep w 'gtk:label)))))

(defun panel-page-title (name)
  (let ((stack (cadre-ui::panel-stack (cadre-ui::window-panel *window*))))
    (gtk:stack-page-get-title (gtk:stack-get-page stack (gtk:stack-get-child-by-name stack name)))))

(defmacro then (delay &body body)
  `(push (cons ,delay (lambda () ,@body)) *steps*))

(defun finish ()
  (format t "~&--- Output panel ---~%~a~%---~%" (cadre-ui::panel-output-string (cadre-ui::window-panel *window*)))
  (let ((failed (count nil *results* :key #'second)))
    (format t "~&~%~d checks, ~d failed. Screenshots in ~a~%" (length *results*) failed *out*)
    (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if (zerop failed) 0 1))))

(defmacro then-when ((condition &key (timeout 30)) &body body)
  "A step that runs once CONDITION is true (checked every 100 ms), or after TIMEOUT seconds."
  `(push (list :wait (lambda () ,condition) ,timeout (lambda () ,@body)) *steps*))

(defun run-step (fn)
  (handler-case (funcall fn)
    (error (e) (check "step ran without error" nil (princ-to-string e)))))

(defun run-steps (steps)
  (if (null steps)
      (finish)
      (let ((step (first steps)))
        (if (eq (car step) :wait)
            (destructuring-bind (condition timeout fn) (rest step)
              (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
                (glib:timeout-add glib:+priority-default+ 100
                                  (lambda ()
                                    (cond ((or (ignore-errors (funcall condition))
                                               (> (get-internal-real-time) deadline))
                                           (run-step fn)
                                           (run-steps (rest steps))
                                           nil)
                                          (t t))))))
            (glib:timeout-add glib:+priority-default+ (car step)
                              (lambda ()
                                (run-step (cdr step))
                                (run-steps (rest steps))
                                nil))))))

;;; A throwaway project
(ensure-directories-exist (merge-pathnames "src/" *root*))
(with-open-file (o (merge-pathnames "src/hello.lisp" *root*) :direction :output)
  (format o "(defun hello (name)~%  (format t \"Hello, ~~a!~~%\" name))~%"))
(with-open-file (o (merge-pathnames "notes.txt" *root*) :direction :output)
  (format o "Some notes.~%"))
(with-open-file (o (merge-pathnames "src/m1.lisp" *root*) :direction :output)
  (format o "(defun area (w h)~%  (* w h))~%~%(defvar *x* 1)~%; done~%"))
(uiop:copy-file (merge-pathnames "../gtk4/src/generated/gtk-functions-1.lisp" (truename "."))
                (merge-pathnames "src/big.lisp" *root*))
(with-open-file (o (merge-pathnames "src/m2.lisp" *root*) :direction :output)
  (format o "(defun twice (x) (* 2 x))~%(defun bad (y) (+ y undefined-thing))~%(twice 21)~%~%"))
(with-open-file (o (merge-pathnames "src/m3.lisp" *root*) :direction :output)
  (format o "(defmacro my-mac (x) `(list ,x ,x))~%(defvar special-thing 5)~%(defun caller () (my-mac (twice 3)))~%(defun get-thing () special-thing)~%(defun uses-undefined () (no-such-function 1))~%(defun deep (n) (if (zerop n) (error \"deep ~~a\" n) (deep (1- n))))~%(defun binder () (let ((not-a-call 1)) not-a-call))~%"))
(with-open-file (o (merge-pathnames "src/lib.lisp" *root*) :direction :output)
  (format o "(defun smoke-lib-fn () :loaded)~%"))
(with-open-file (o (merge-pathnames "smoke.asd" *root*) :direction :output)
  (format o "(defsystem \"smoke\" :components ((:file \"src/lib\")))~%"))
(setf *lisp-command* '("sbcl" "--noinform" "--no-userinit"))
(with-open-file (o (merge-pathnames "cache.fasl" *root*) :direction :output)
  (format o "hidden"))

;;; The steps
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

(then-when ((search "CONS" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))))
  (check "Back returns to the previous object" t)
  (cadre-ui::repl-eval "(deep 3)"))

(defun deep-frame ()
  (find-if (lambda (f) (search "(DEEP 0" (frame-description f)))
           (cadre-ui::dl-frames (first cadre-ui::*debug-levels*))))

(then-when (cadre-ui::*debug-levels*)
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
(defun chat-texts () (label-texts (cadre-ui::chat-messages cadre-ui::*chat*)))
(defun chat-says (text) (some (lambda (s) (search text s)) (chat-texts)))
(defun chat-type (text)
  (text-replace-contents (gtk:text-view-get-buffer (cadre-ui::chat-input cadre-ui::*chat*)) text)
  (call-command 'cadre-ui::chat-send))

(defvar *signed-out* (merge-pathnames "fake-claude-signed-out" *root*))

(then 300
  (with-open-file (o *signed-out* :direction :output :if-exists :supersede) (write-line "out" o))
  (sb-posix:setenv "FAKE_CLAUDE_SIGNED_OUT" (namestring *signed-out*) 1)
  (setf *claude-program* (namestring (truename "scripts/fake-claude")))
  (call-command 'cadre-ui::claude))

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
  (gtk:widget-activate (chat-button "Allow Once")))

(then-when ((chat-says "Permission:"))
  (check "allowing tells Claude Code to go ahead" (chat-says "Permission: allow"))
  (chat-type "what is my context"))

(then-when ((chat-says "Context:"))
  (check "current_context describes what the user is looking at" (chat-says "Buffer: m2.lisp")
         (find-if (lambda (s) (search "Context:" s)) (chat-texts)))
  (call-command 'cadre-ui::claude-new-chat)
  (check "a new conversation starts empty" (not (chat-says "Context:"))))

(then 500
  (screenshot "10-repl")
  (call-command 'disconnect))

(then-when ((not (cadre-ui::connected-p)))
  (check "disconnect closes the connection" (not (cadre-ui::connected-p))))

(setf *steps* (reverse *steps*))

;;; Run, with a fresh config directory so first-run questions are skipped.
(let ((config (merge-pathnames (format nil "cadre-smoke-config-~d/" (get-universal-time))
                                (uiop:temporary-directory))))
  (ensure-directories-exist (merge-pathnames "cadre/" config))
  (with-open-file (o (merge-pathnames "cadre/settings.sexp" config) :direction :output)
    (prin1 '(:keybinding-profile :standard) o))
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config) 1))

(glib:timeout-add glib:+priority-default+ 100 (lambda () (run-steps *steps*) nil))
(cadre-ui:main :project *root* :init-file nil :quit-after 480)
(format t "~&Timed out.~%")
(uiop:quit 1)
