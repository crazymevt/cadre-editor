;;;; smoke.lisp — drive a real Cadre window through the M0 features:
;;;;   make smoke
;;;; Uses a throwaway project and config directory, checks each step, saves
;;;; screenshots to build/smoke/, prints a report, and exits 0 if all passed.

(push (truename ".") asdf:*central-registry*)
;; The gtk4 bindings: a checkout next to Cadre if there is one, otherwise
;; wherever Quicklisp finds them (~/quicklisp/local-projects/).
(let ((gtk4 (probe-file "../gtk4/gtk4.asd")))
  (when gtk4 (push (uiop:pathname-directory-pathname gtk4) asdf:*central-registry*)))
(ql:quickload :cadre :silent t)

(defpackage #:cadre-smoke (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-smoke)

(defvar *out* (merge-pathnames "build/smoke/" (truename ".")))
(defvar *root* (merge-pathnames (format nil "cadre-smoke-~d-~d/" (get-universal-time) (sb-posix:getpid))
                                (uiop:temporary-directory)))
(defvar *results* '())
(defvar *steps* '())

(defun check (name ok &optional detail)
  (push (list name (and ok t) detail) *results*)
  (format t "~&~:[FAIL~;ok  ~] ~a~@[ — ~a~]~%" ok name detail))

(defvar *paintable* nil)

(defun screenshot (name &optional widget)
  "Render the window (or WIDGET, such as a popover) to build/smoke/NAME.png."
  (let* ((window (or widget (cadre-ui::window-gtk-window *window*)))
         (w (gtk:widget-get-width window))
         (h (gtk:widget-get-height window))
         (paintable (if widget
                        (gtk:widget-paintable-new window)
                        (or *paintable* (setf *paintable* (gtk:widget-paintable-new window)))))
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
           (keyval (if (and (= 1 (length name)) (not (alphanumericp (char name 0))))
                       (gdk:unicode-to-keyval (char-code (char name 0))) ; punctuation: - = + …
                       (gdk:keyval-from-name (cond ((string= name "TAB") "Tab")
                                                   ((string= name "RET") "Return")
                                                   ((string= name "ESC") "Escape")
                                                   (t name)))))
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

;;; Sections: `make smoke ONLY=run-app` runs that section, and the ones it
;;; needs, instead of everything (`make smoke-list` names them). Each
;;; section starts with (section "name" :needs (...)); its steps are tagged
;;; with it.

(defvar *sections* '() "(name . needs), in order.")
(defvar *current-section* nil)

(defmacro section (name &key needs)
  `(progn (setf *current-section* ,name)
          (setf *sections* (append *sections* (list (cons ,name ',needs))))))

(defun push-step (step)
  (push (cons *current-section* step) *steps*))

(defun selected-sections (only)
  "The sections to run for ONLY (names separated by commas or spaces), with
everything they need, in file order; all of them if ONLY is empty."
  (let ((names (remove "" (uiop:split-string only :separator ", ") :test #'string=))
        (chosen '()))
    (labels ((add (name)
               (let ((entry (or (assoc name *sections* :test #'string=)
                                (progn (format t "~&No smoke section ~s. Sections: ~{~a~^ ~}~%"
                                               name (mapcar #'car *sections*))
                                       (uiop:quit 2)))))
                 (unless (member name chosen :test #'string=)
                   (push name chosen)
                   (mapc #'add (cdr entry))))))
      (if names
          (progn (mapc #'add names)
                 (remove-if-not (lambda (s) (member s chosen :test #'string=)) (mapcar #'car *sections*)))
          (mapcar #'car *sections*)))))

(defmacro then (delay &body body)
  `(push-step (cons ,delay (lambda () ,@body))))

(defun finish ()
  (format t "~&--- Output panel ---~%~a~%---~%" (cadre-ui::panel-output-string (cadre-ui::window-panel *window*)))
  (let ((failed (count nil *results* :key #'second)))
    (format t "~&~%~d checks, ~d failed. Screenshots in ~a~%" (length *results*) failed *out*)
    (uiop:delete-directory-tree *root* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if (zerop failed) 0 1))))

(defmacro then-when ((condition &key (timeout 30)) &body body)
  "A step that runs once CONDITION is true (checked every 100 ms), or after TIMEOUT seconds."
  `(push-step (list :wait (lambda () ,condition) ,timeout (lambda () ,@body))))

(defun open-files (&rest files)
  "Steps that open FILES (relative to *root*) unless they are open, and show
the first: so a section can run without the ones before it."
  (then 50
    (dolist (file files)
      (unless (find-buffer (file-namestring file))
        (open-file-path (merge-pathnames file *root*)))))
  (then-when ((every (lambda (file) (find-buffer (file-namestring file))) files) :timeout 10)
    (show-buffer-named (file-namestring (first files)))))

(defvar *quicklisp-loaded* nil)

(defun load-quicklisp ()
  "Steps that load Quicklisp into the Lisp, as a usual init file would (the
smoke test starts it without one), and wait for it."
  (then 50
    (with-connection (connection)
      (cadre-ui::rex connection (swank-call "swank:interactive-eval"
                                            "(load (merge-pathnames \"quicklisp/setup.lisp\" (user-homedir-pathname)))" 3 120)
                     :on-ok (lambda (v) (declare (ignore v)) (setf *quicklisp-loaded* t)))))
  (then-when (*quicklisp-loaded* :timeout 120)))

(defun connect-lisp ()
  "Steps that start a Lisp unless one is connected, and wait for it."
  (then 50
    (unless (or (cadre-ui::connected-p) cadre-ui::*connecting*) (call-command 'lisp)))
  (then-when ((cadre-ui::connected-p) :timeout 120)))

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
  (setf (gethash "compile" cadre-ui::*session-allowed*) t)
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
  (let ((menu (gtk:text-view-get-extra-menu (view-text-view (current-view)))))
    (check "Lisp editors have a right-click menu with Go to Definition"
           (and menu (plusp (gio:menu-model-get-n-items menu))
                (search "Go to Definition" (prin1-to-string (loop for i below (gio:menu-model-get-n-items (gio:menu-model-get-item-link menu 0 "section"))
                                                                     collect (glib:variant-get-string
                                                                              (gio:menu-model-get-item-attribute-value
                                                                               (gio:menu-model-get-item-link menu 0 "section") i "label" nil))))))))
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
                 (search "result" (gtk:label-get-text (cadre-ui::ps-status cadre-ui::*project-search*)))))
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
        (check "and restores it" (and (preview-buffer) (cadre-ui::buffer-views *window* (preview-buffer)))))))
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
  (call-command 'close-tab))

(then 300
  (check "closing that preview lets the file go too" (and (null (preview-buffer)) (null (find-buffer "guide.md")))))

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

;;; Claude edit
(section "claude-edit")
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

(then-when ((search "(* w h 2)" (m1-disk)) :timeout 10)
  (call-command 'cadre-ui::git-stage-file))

(then-when ((null (find :conflict cadre-ui::*git-entries* :key (lambda (e) (git-status-kind (first e) (second e))))) :timeout 10)
  (git-run-continue))

(defun git-run-continue ()
  (let ((root (cadre-ui::project-git-root)))
    (cadre-ui::git-run root (lambda () (git-continue root :merge)))))

(then-when ((null cadre-ui::*git-operation*) :timeout 15)
  (check "after staging, Commit finishes the merge" (search "Merge branch" (git-ok *root* "log" "-1" "--format=%s")))
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

;;; A new Lisp project
(section "new-project")

(defun np (key) (getf cadre-ui::*new-project-dialog* key))
(defvar *new-parent* (merge-pathnames "new-projects/" *root*))

(then 300
  (ensure-directories-exist *new-parent*)
  (setf (cadre-ui::setting :new-project-folder) (namestring *new-parent*))
  (call-command 'cadre-ui::new-lisp-project))

(then 600
  (check "New Lisp Project opens its dialog" (np :dialog))
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
  (check "and shows its output" (equal "output" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*)))))

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

(then-when ((search "Passed:" (repl-text)) :timeout 120)
  (check "Run Tests runs the project's tests in the REPL" (search "Testing system smoke-app" (repl-text)))
  (check "and their results show there" (search "Passed:" (repl-text))
         (let ((level (first cadre-ui::*debug-levels*)))
           (and level (cadre-ui::dl-condition level))))
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
  (check "and shows the Output page" (equal "output" (cadre-ui::panel-visible-name (cadre-ui::window-panel *window*))))
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
              (string= "(in-package :foo)" (cadre-ui::gtk-thread-source "(in-package :foo)")))))

(defun gtk-title-form ()
  "(let ((app (gio:application-get-default))) (and app (gtk:application-get-active-window app) (gtk:window-get-title (gtk:application-get-active-window app))))")

(then 3000
  (cadre-ui::repl-eval (gtk-title-form)))

(then-when ((search "\"smoke-gtk\"" (repl-text)) :timeout 180)
  (check "the app's window is up, and the REPL reaches it on the GTK thread" (search "\"smoke-gtk\"" (repl-text)))
  (screenshot "42-gtk-app")
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

(then 300
  (check "▶ is shown for a project with an :entry-point" (gtk:widget-get-visible (run-button)))
  (call-command 'cadre-ui::run-app))

(then-when ((cadre-ui::gtk-app-running-p) :timeout 30)
  (check "▶ runs a gtk4 app as Run GTK App does" (cadre-ui::gtk-app-running-p))
  (check "and becomes a stop button" (equal "Stop the app" (gtk:widget-get-tooltip-text (run-button))))
  (call-command 'cadre-ui::stop-app))

(then-when ((not (cadre-ui::gtk-app-running-p)) :timeout 30)
  (check "Stop App quits it" (not (cadre-ui::gtk-app-running-p)))
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
  (check "RET sends the line, and the app's output follows" (search "Hello, Pat!" (repl-text)) (repl-text))
  (check "▶ is back" (equal "Run smoke-cli:main (in the REPL)" (gtk:widget-get-tooltip-text (run-button)))
         (gtk:widget-get-tooltip-text (run-button)))
  (call-command 'cadre-ui::run-app))

(then-when ((and cadre-ui::*console-app* (cadre-ui::repl-reading cadre-ui::*repl*)) :timeout 30)
  (call-command 'cadre-ui::stop-app))

(then-when ((null cadre-ui::*console-app*) :timeout 30)
  (check "Stop App aborts a console app waiting for input" (null cadre-ui::*console-app*))
  (check "and the REPL takes forms again" (not (cadre-ui::repl-busy cadre-ui::*repl*))))

(then-when ((search "Stopped" (status-text)) :timeout 10)
  (check "the status bar names what stopped" (search "Stopped smoke-cli:main" (status-text)) (status-text)))

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
  (cadre-ui::inspect-string "(list 1 \"two\")" "COMMON-LISP-USER"))

(then-when ((search "two" (gtk:label-get-text (cadre-ui::ins-title cadre-ui::*inspector*))) :timeout 20)
  (let ((items (cadre-ui::inspector-menu-items nil)))
    (check "the Inspector offers the inspected value too"
           (equal '("Copy the Inspected Value" "Open in New Tab") (mapcar #'car items)) (mapcar #'car items))
    (funcall (cdr (assoc "Open in New Tab" items :test #'string=)))))

(then-when ((find-buffer "value<2>") :timeout 20)
  (check "and opens it, printed in full" (string= "(1 \"two\")" (buffer-string (find-buffer "value<2>")))
         (buffer-string (find-buffer "value<2>")))
  (dolist (name '("value" "value<2>"))
    (setf (buffer-modified-p (find-buffer name)) nil)))

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
