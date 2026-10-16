;;;; gallery.lisp — the screenshots in docs/gallery/, taken from a real window:
;;;;   make gallery                       (dark; and editing in light, for Themes)
;;;;   make gallery ONLY=profiler         (one scene, and what it needs)
;;;; Opens a small demo project, stages each scene (a Lisp running, files
;;;; open, a page showing) and saves the window at twice its size to
;;;; docs/gallery/SCHEME/NAME.png, SCHEME being GALLERY_SCHEME (light or
;;;; dark). Like the smoke test, it uses a throwaway config directory, so
;;;; your settings don't show.

(push (truename ".") asdf:*central-registry*)
(dolist (asd '("../gtk4/gtk4.asd" "../docset/docset.asd"))
  (let ((found (probe-file asd)))
    (when found (push (uiop:pathname-directory-pathname found) asdf:*central-registry*))))
(ql:quickload :cadre :silent t)

(defpackage #:cadre-gallery (:use #:cl #:cadre #:cadre-ui))
(in-package #:cadre-gallery)

(defvar *scheme* (let ((s (uiop:getenv "GALLERY_SCHEME")))
                   (if (member s '("light" "dark") :test #'equal) s "light")))
(defvar *scale* (let ((s (uiop:getenv "GALLERY_SCALE"))) (if (plusp (length s)) (parse-integer s) 2)))
(defvar *out* (merge-pathnames (format nil "docs/gallery/~a/" *scheme*) (truename ".")))
(defvar *base* (merge-pathnames (format nil "cadre-gallery-~d/" (sb-posix:getpid)) (uiop:temporary-directory)))
;; The project is named geometry, and the window says it's in ~/projects.
(defvar *root* (merge-pathnames "geometry/" *base*))
(load (merge-pathnames "scripts/drive.lisp" (truename ".")))

(defun shoot (name)
  "Save the window, *SCALE* times its size, to *OUT*/NAME.png."
  (let* ((window (cadre-ui::window-gtk-window *window*))
         (w (gtk:widget-get-width window))
         (h (gtk:widget-get-height window))
         (paintable (gtk:widget-paintable-new window))
         (snapshot (gtk:snapshot-new)))
    (gtk:snapshot-scale snapshot (float *scale*) (float *scale*))
    (gdk:paintable-snapshot paintable snapshot (float w 1d0) (float h 1d0))
    (let* ((node (gtk:snapshot-to-node snapshot))
           (texture (gsk:renderer-render-texture (gtk:native-get-renderer window) node nil))
           (path (merge-pathnames (format nil "~a.png" name) *out*)))
      (ensure-directories-exist path)
      (gdk:texture-save-to-png texture (namestring path))
      (format t "~&Saved ~a~%" (enough-namestring path (truename "."))))))

(defun finish ()
  (let ((failed (count nil *results* :key #'second)))
    (format t "~&~%~d checks, ~d failed. Pictures in ~a~%" (length *results*) failed *out*)
    (uiop:delete-directory-tree *base* :validate t :if-does-not-exist :ignore)
    (uiop:quit (if (zerop failed) 0 1))))

;;; Staging

(defun clear-message ()
  (gtk:label-set-text (cadre-ui::window-status-message *window*) ""))

(defun clear-stage (&key panel (sidebar "explorer"))
  "Close every tab and group, show the sidebar's SIDEBAR page (none if nil),
and the panel if PANEL: T, or its height in pixels."
  (ignore-errors (call-command 'cadre-ui::delete-other-groups))
  (dolist (page (cadre-ui::window-pages *window*))
    (ignore-errors
     (let ((view (cadre-ui::page-view *window* page)))
       (setf (buffer-modified-p (view-buffer view)) nil)
       (cadre-ui::close-view *window* view))))
  (when (integerp panel)
    (setf (getf (cadre-ui::window-panel-sizes *window*) (cadre-ui::window-layout *window*)) panel))
  (cadre-ui::set-panel-visible *window* (and panel t))
  (when panel (cadre-ui::restore-panel-size *window*))
  (clear-message)
  (if sidebar
      (cadre-ui::show-sidebar-page *window* sidebar :toggle nil)
      (cadre-ui::set-sidebar-visible *window* nil)))

(defun cursor-after (text &key (before 0))
  "Put the cursor just after the first TEXT in the current buffer, or BEFORE
characters back from there."
  (let* ((string (buffer-string (current-buffer)))
         (at (search text string)))
    (when at
      (let* ((point (- (+ at (length text)) before))
             (line (count #\Newline string :end point))
             (start (let ((nl (position #\Newline string :end point :from-end t))) (if nl (1+ nl) 0))))
        (set-cursor line (- point start))))))

(defun open-scene-file (file &key at)
  "Steps that open FILE (relative to the project) and, with AT, put the cursor after it."
  (then 50 (open-file-path (merge-pathnames file *root*)))
  (then-when ((find-buffer (file-namestring file)) :timeout 10)
    (show-buffer-named (file-namestring file))
    (when at (cursor-after at))))

(defun close-in-this-group (name)
  "Close the tab of buffer NAME in the current tab's group, when it isn't the current tab."
  (let ((group (cadre-ui::view-group (current-view))))
    (dolist (view (cadre-ui::buffer-views *window* (find-buffer name)))
      (when (and (eq group (cadre-ui::view-group view)) (not (eq view (current-view))))
        (cadre-ui::close-view *window* view)))))

(defvar *geometry-loaded* nil)

(defun load-geometry ()
  "Steps that start a Lisp with Quicklisp and load the geometry system into it."
  (connect-lisp)
  (load-quicklisp)
  (then 50
    (unless *geometry-loaded*
      (with-connection (connection)
        (cadre-ui::rex connection
                       (swank-call "swank:interactive-eval"
                                   (format nil "(progn (asdf:load-asd ~s) (asdf:load-system \"geometry\") t)"
                                           (namestring (merge-pathnames "geometry.asd" *root*)))
                                   3 120)
                       :on-ok (lambda (v) (declare (ignore v)) (setf *geometry-loaded* t))))))
  (then-when (*geometry-loaded* :timeout 120)
    (check "geometry loads" *geometry-loaded*)))

;;; The demo project

(defun write-root-file (file text)
  (let ((path (merge-pathnames file *root*)))
    (ensure-directories-exist path)
    (with-open-file (o path :direction :output :if-exists :supersede)
      (write-string text o))))

(write-root-file "geometry.asd" "(defsystem \"geometry\"
  :description \"Shapes, their areas, and the Mandelbrot set as text.\"
  :license \"MIT\"
  :components ((:module \"src\"
                :serial t
                :components ((:file \"package\")
                             (:file \"shapes\")
                             (:file \"render\")))))

(defsystem \"geometry/tests\"
  :depends-on (\"geometry\" \"parachute\")
  :components ((:file \"tests/shapes\")))
")

(write-root-file "src/package.lisp" "(defpackage #:geometry
  (:use #:cl)
  (:export #:shape #:circle #:rect #:radius #:width #:height
           #:area #:perimeter #:scale #:with-scaling #:total-area
           #:mandelbrot))
")

(write-root-file "src/shapes.lisp" "(in-package #:geometry)

(defclass shape ()
  ((name :initarg :name :initform \"shape\" :reader shape-name))
  (:documentation \"Something with an area.\"))

(defclass circle (shape)
  ((radius :initarg :radius :accessor radius)))

(defclass rect (shape)
  ((width :initarg :width :accessor width)
   (height :initarg :height :accessor height)))

(defgeneric area (shape)
  (:documentation \"The area SHAPE covers.\"))

(defmethod area ((c circle))
  (* pi (expt (radius c) 2)))

(defmethod area ((r rect))
  (* (width r) (height r)))

(defgeneric perimeter (shape))

(defmethod perimeter ((c circle))
  (* 2 pi (radius c)))

(defmethod perimeter ((r rect))
  (* 2 (+ (width r) (height r))))

(defun copy-shape (shape)
  (etypecase shape
    (circle (make-instance 'circle :name (shape-name shape) :radius (radius shape)))
    (rect (make-instance 'rect :name (shape-name shape)
                               :width (width shape) :height (height shape)))))

(defun scale (shape factor &key (copy t))
  \"Scale SHAPE by FACTOR: a copy of it, unless COPY is false.\"
  (let ((s (if copy (copy-shape shape) shape)))
    (etypecase s
      (circle (setf (radius s) (* factor (radius s))))
      (rect (setf (width s) (* factor (width s))
                  (height s) (* factor (height s)))))
    s))

(defmacro with-scaling ((var shape factor) &body body)
  \"Run BODY with VAR bound to a copy of SHAPE scaled by FACTOR.\"
  `(let ((,var (scale ,shape ,factor)))
     ,@body))

(defun total-area (shapes)
  (reduce #'+ shapes :key #'area))

(defun doubled-areas (shapes)
  (mapcar (lambda (s)
            (with-scaling (big s 2)
              (area big)))
          shapes))
")

(write-root-file "src/render.lisp" "(in-package #:geometry)

(defun escape-time (c &key (limit 200))
  \"How many steps z := z² + C takes to leave the circle of radius 2.\"
  (loop for z = c then (+ (* z z) c)
        for n below limit
        while (< (abs z) 2)
        finally (return n)))

(defun mandelbrot (&key (width 72) (height 28) (limit 200))
  \"The Mandelbrot set as text, WIDTH characters by HEIGHT lines.\"
  (with-output-to-string (out)
    (dotimes (row height)
      (dotimes (col width)
        (let* ((c (complex (- (* 3.0 (/ col width)) 2.2)
                           (- (* 2.4 (/ row height)) 1.2)))
               (n (escape-time c :limit limit)))
          (write-char (char \" .:-=+*#%@\" (floor (* 9 n) limit)) out)))
      (terpri out))))
")

(write-root-file "tests/shapes.lisp" "(defpackage #:geometry-tests
  (:use #:cl #:parachute #:geometry))
(in-package #:geometry-tests)

(define-test areas)

(define-test circle-area :parent areas
  (is = 3 (round (area (make-instance 'circle :radius 1)))))

(define-test rect-area :parent areas
  (is = 6 (area (make-instance 'rect :width 2 :height 3))))

(define-test scaling
  (is = 3 (radius (scale (make-instance 'circle :radius 1) 3)))
  (let ((c (make-instance 'circle :radius 1)))
    (scale c 3)
    (is = 1 (radius c))))

(define-test perimeters
  (is = 12 (perimeter (make-instance 'rect :width 2 :height 3))))
")

(write-root-file "README.md" "# Geometry

Shapes, their areas, and the **Mandelbrot set** as text.

```lisp
(ql:quickload \"geometry\")
(geometry:total-area (list (make-instance 'geometry:circle :radius 2)
                           (make-instance 'geometry:rect :width 3 :height 4)))
;; => 24.566370614359172d0
```

## Shapes

| Class    | Slots            | Area      |
|----------|------------------|-----------|
| `circle` | radius           | π r²      |
| `rect`   | width, height    | w × h     |

- `scale` makes a scaled copy; `with-scaling` binds one.
- `mandelbrot` draws the set in characters, for a terminal.

## Tests

Run them with `(asdf:test-system \"geometry/tests\")`, or the Tests page.
")

(write-root-file "ports/shapes.rkt" "#lang racket
;; The shapes, in Racket
(provide (struct-out circle) (struct-out rect) area)

(struct circle (radius))
(struct rect (width height))

(define (area shape)
  (match shape
    [(circle r) (* pi r r)]
    [(rect w h) (* w h)]))

(define (total-area shapes)
  (for/sum ([s (in-list shapes)])
    (area s)))
")

(write-root-file "ports/shapes.clj" "(ns geometry.shapes
  \"The shapes, in Clojure.\")

(defprotocol Shape
  (area [shape] \"The area it covers.\"))

(defrecord Circle [radius]
  Shape
  (area [_] (* Math/PI radius radius)))

(defrecord Rect [width height]
  Shape
  (area [_] (* width height)))

(defn total-area [shapes]
  (transduce (map area) + shapes))

(comment
  (total-area [(->Circle 2) (->Rect 3 4)]))
")

(ensure-directories-exist (merge-pathnames "data/" *root*))
(let ((db (open-sqlite (merge-pathnames "data/shapes.db" *root*) :create t)))
  (db-run db "CREATE TABLE shapes (id INTEGER PRIMARY KEY, kind TEXT NOT NULL, name TEXT,
  width REAL, height REAL, radius REAL, color TEXT);
INSERT INTO shapes (kind, name, width, height, radius, color) VALUES
  ('circle', 'sun', NULL, NULL, 40, 'gold'),
  ('rect', 'door', 0.9, 2.1, NULL, 'oak'),
  ('rect', 'window', 1.2, 1.4, NULL, 'white'),
  ('circle', 'coin', NULL, NULL, 1.2, 'copper'),
  ('rect', 'table', 1.6, 0.9, NULL, 'walnut'),
  ('circle', 'plate', NULL, NULL, 13, 'white'),
  ('rect', 'phone', 0.07, 0.15, NULL, 'black'),
  ('circle', 'wheel', NULL, NULL, 33, 'black');
CREATE TABLE drawings (id INTEGER PRIMARY KEY, title TEXT, shape_ids TEXT);
CREATE VIEW circles AS SELECT name, radius FROM shapes WHERE kind = 'circle';")
  (db-close db))

;; A server for the API scene, and its requests.
(defvar *api-port* nil)
(defparameter *shapes-json*
  "[{\"id\": 1, \"kind\": \"circle\", \"name\": \"sun\", \"radius\": 40, \"area\": 5026.55},
{\"id\": 2, \"kind\": \"rect\", \"name\": \"door\", \"width\": 0.9, \"height\": 2.1, \"area\": 1.89},
{\"id\": 3, \"kind\": \"rect\", \"name\": \"window\", \"width\": 1.2, \"height\": 1.4, \"area\": 1.68}]")
(defun api-serve (socket)
  "Answer every request with *SHAPES-JSON*."
  (loop
    (let ((client (handler-case (sb-bsd-sockets:socket-accept socket) (error () (return)))))
      (let ((stream (sb-bsd-sockets:socket-make-stream client :input t :output t :element-type :default
                                                              :external-format :utf-8 :buffering :full)))
        (ignore-errors
         (unwind-protect
              (progn
                (loop for line = (string-right-trim '(#\Return) (read-line stream))
                      until (string= line ""))
                (let ((octets (sb-ext:string-to-octets *shapes-json* :external-format :utf-8)))
                  (format stream "HTTP/1.1 200 OK~c~cContent-Type: application/json~c~cContent-Length: ~d~c~c~c~c~a"
                          #\Return #\Newline #\Return #\Newline (length octets)
                          #\Return #\Newline #\Return #\Newline *shapes-json*))
                (finish-output stream))
           (close stream)))))))
(let ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp)))
  (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
  (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
  (sb-bsd-sockets:socket-listen socket 5)
  (setf *api-port* (nth-value 1 (sb-bsd-sockets:socket-name socket)))
  (sb-thread:make-thread (lambda () (api-serve socket)) :name "gallery api server"))
(write-root-file "http-client.env.json" (format nil "{\"dev\": {\"baseUrl\": \"http://localhost:~d\"}}~%" *api-port*))
(write-root-file "api/shapes.http" "### List shapes
GET {{baseUrl}}/shapes?kind=all
Accept: application/json
Authorization: Bearer {{token}}

### Add a shape
POST {{baseUrl}}/shapes
Content-Type: application/json

{\"kind\": \"circle\", \"name\": \"moon\", \"radius\": 17}
")
(write-root-file ".gitignore" ".cadre/
")

;; A repository with one commit, so the Git scene has a change to show.
(flet ((git* (&rest args)
         (uiop:run-program (list* "git" "-C" (namestring *root*) "-c" "user.name=Cadre" "-c" "user.email=cadre@example.com"
                                  "-c" "commit.gpgsign=false" args)
                           :ignore-error-status t)))
  (git* "init" "-q" "-b" "main")
  (git* "add" "-A")
  (git* "commit" "-q" "-m" "Shapes, areas and the Mandelbrot set"))

(setf *lisp-command* '("sbcl" "--noinform" "--no-userinit"))
(setf *claude-program* (namestring (truename "scripts/fake-claude")))

;;; Scenes

(section "start")
(then 1500
  ;; The window shows as unfocused when another one has the focus (as it
  ;; may, while this runs): keep it looking focused.
  (glib:timeout-add glib:+priority-default+ 100
                    (lambda ()
                      (gtk:widget-unset-state-flags (cadre-ui::window-gtk-window *window*) '(:backdrop))
                      t))
  (adw:window-title-set-subtitle (cadre-ui::window-title *window*) "~/projects/geometry/"))

(section "editing" :needs ("start"))
(load-geometry)
(then 50 (clear-stage :panel 300))
(open-scene-file "src/render.lisp")
(open-scene-file "src/shapes.lisp")
(then 100
  (cadre-ui::show-repl-page)
  (cadre-ui::repl-eval "(in-package :geometry)"))
(then 1500
  (cadre-ui::repl-eval "(total-area (list (make-instance 'circle :radius 2) (make-instance 'rect :width 3 :height 4)))"))
(then 1500
  (cadre-ui::repl-eval "(mapcar #'area (list (scale (make-instance 'rect :width 1 :height 2) 3)))"))
(then 1500
  (show-buffer-named "shapes.lisp")
  (cursor-after "(scale ,shape "))
(then 1200 (shoot "editing"))

(section "debugger" :needs ("start"))
(load-geometry)
(then 50
  (clear-stage :panel 440)
  (cadre-ui::repl-eval "(geometry:total-area (list (make-instance 'geometry:circle :radius 1) (make-instance 'geometry:shape)))"))
(open-scene-file "src/shapes.lisp" :at "(reduce #'+ shapes")
(then-when (cadre-ui::*debug-levels* :timeout 20)
  (check "the debugger opens" cadre-ui::*debug-levels*))
(then 1200
  (shoot "debugger")
  (call-command 'cadre-ui::debugger-abort))
(then-when ((null cadre-ui::*debug-levels*) :timeout 10))

(section "profiler" :needs ("start"))
(load-geometry)
(then 50 (clear-stage :panel 330))
(open-scene-file "src/render.lisp" :at "(defun escape-time")
(then 100
  (call-command 'cadre-ui::clear-profile)
  (cadre-ui::profile-form-text "(loop repeat 25 do (geometry:mandelbrot :width 160 :height 60))" "COMMON-LISP-USER"))
(then-when ((let ((p (cadre-ui::pp-profile cadre-ui::*profiler-page*))) (and p (plusp (profile-samples p)))) :timeout 90)
  (check "the profiler has samples" (cadre-ui::pp-profile cadre-ui::*profiler-page*))
  (let ((flame (find-if (lambda (b) (equal "Flame Graph" (gtk:button-get-label b)))
                        (find-widgets (cadre-ui::pp-widget cadre-ui::*profiler-page*)
                                      (lambda (w) (typep w 'gtk:toggle-button))))))
    (when flame (gtk:toggle-button-set-active flame t))))
(then 1200 (shoot "profiler"))

(section "tests" :needs ("start"))
(load-geometry)
(then 50 (clear-stage :panel 440))
(open-scene-file "tests/shapes.lisp")
(then 300 (call-command 'cadre-ui::show-test-explorer))
(then 300 (call-command 'cadre-ui::run-all-tests))
(then-when ((let ((page cadre-ui::*tests-page*))
              (and (not (cadre-ui::tsp-running page)) (some #'source-test-status (cadre-ui::tsp-tests page))))
            :timeout 120)
  (check "the tests ran" (some #'source-test-status (cadre-ui::tsp-tests cadre-ui::*tests-page*))))
(then 1200 (shoot "tests"))

(section "macrostep" :needs ("start"))
(load-geometry)
(then 50 (clear-stage))
(open-scene-file "src/shapes.lisp" :at "(with-scaling (big" )
(then 100
  (cursor-after "(with-scaling (big" :before (length "(with-scaling (big"))
  (call-command 'cadre-ui::macrostep-expand-in-place))
(then-when ((plusp (length (cadre-ui::macrosteps (current-buffer)))) :timeout 20)
  (cursor-after "(scale s 2)" :before (length "(scale s 2)")))
(then 1200
  (shoot "expand-in-place")
  (call-command 'cadre-ui::macrostep-collapse-all-expansions))

(section "outline" :needs ("start"))
(then 50 (clear-stage :sidebar "outline"))
(open-scene-file "src/shapes.lisp" :at "(defun scale")
(then 500
  (let ((project (find-if (lambda (w) (and (typep w 'gtk:toggle-button) (equal "Project" (gtk:button-get-label w))))
                          (find-widgets (cadre-ui::ol-widget cadre-ui::*outline*) (constantly t)))))
    (when project (gtk:toggle-button-set-active project t))))
(then 2000 (shoot "outline"))

(section "hyperspec" :needs ("start"))
(then 50 (clear-stage))
(open-scene-file "src/render.lisp" :at "(with-output-to")
(then 100 (call-command 'cadre-ui::hyperspec-look-up))
(then 1500 (shoot "hyperspec"))

(section "racket-clojure" :needs ("start"))
(then 50 (clear-stage))
(open-scene-file "ports/shapes.rkt" :at "(match shape")
(then 300 (call-command 'cadre-ui::split-right))
(open-scene-file "ports/shapes.clj" :at "(transduce")
(then 300 (close-in-this-group "shapes.rkt"))
(then 1500 (shoot "racket-clojure"))

(section "markdown" :needs ("start"))
(then 50 (clear-stage))
(open-scene-file "README.md")
(then 300 (call-command 'cadre-ui::split-right))
(then 300 (cadre-ui::open-markdown-preview (merge-pathnames "README.md" *root*)))
(then 300 (close-in-this-group "README.md"))
(then 1500 (shoot "markdown"))

(section "databases" :needs ("start"))
(then 50
  (clear-stage)
  (call-command 'db-add-connection))
(then 300
  (let ((dialog cadre-ui::*db-connection-dialog*))
    (funcall (getf dialog :use-file) (merge-pathnames "data/shapes.db" *root*))
    (funcall (getf dialog :save-it))))
(then 300
  (let ((connection (find "shapes" cadre-ui::*db-connections* :key #'db-connection-name :test #'string=)))
    (check "the database connection is added" connection)
    (cadre-ui::open-db-connection connection)
    (cadre-ui::open-db-table connection "shapes")))
(then 1500 (shoot "databases"))

(section "api" :needs ("start"))
(then 50
  (clear-stage)
  (setf cadre-ui::*api-environment* "dev")
  (write-http-environments (cadre-ui::api-secrets-file) '(("dev" ("token" . "s3cret"))) :private t)
  (call-command 'show-api))
(then 300
  (let ((file (merge-pathnames "api/shapes.http" *root*)))
    (cadre-ui::open-api-request (first (parse-http-file (uiop:read-file-string file))) :file file :send t)))
(then-when ((search "200" (gtk:label-get-text (cadre-ui::ap-status (buffer-local (current-buffer) :api-page)))) :timeout 15)
  (check "the API request is answered" t))
(then 1200 (shoot "api"))

(section "git" :needs ("start"))
(then 50 (clear-stage :sidebar "git"))
(open-scene-file "src/shapes.lisp")
(then 300
  (let ((text (buffer-text (current-buffer))))
    (gtk:text-buffer-insert text (gtk:text-buffer-get-end-iter text)
                            (format nil "~%(defun largest (shapes)~%  \"The shape with the largest area.\"~%  (first (sort (copy-list shapes) #'> :key #'area)))~%")
                            -1))
  (let* ((old "(* 2 (+ (width r) (height r))))")
         (at (search old (buffer-string (current-buffer)))))
    (when at
      (cadre-ui::replace-text-between (buffer-text (current-buffer)) at (+ at (length old))
                                      "(+ (* 2 (width r)) (* 2 (height r))))")))
  (call-command 'cadre-ui::save-buffer))
(then 1000
  (cadre-ui::show-sidebar-page *window* "git" :toggle nil)
  (cursor-after "(defun largest")
  (clear-message))
(then 2000 (shoot "git"))

;;; Run

(let* ((only (or (uiop:getenv "ONLY") ""))
       (sections (selected-sections only)))
  (setf *steps* (mapcar #'cdr (remove-if-not (lambda (step) (member (car step) sections :test #'string=))
                                             (reverse *steps*)))))

(let ((config (merge-pathnames "config/" *base*)))
  (ensure-directories-exist (merge-pathnames "cadre/" config))
  (with-open-file (o (merge-pathnames "cadre/settings.sexp" config) :direction :output)
    (prin1 '(:keybinding-profile :standard) o))
  (sb-posix:setenv "XDG_CONFIG_HOME" (namestring config) 1)
  (sb-posix:setenv "XDG_STATE_HOME" (namestring (merge-pathnames "state/" config)) 1))
(setf cadre-ui::*color-scheme* (if (string= *scheme* "dark") :dark :light))

(glib:timeout-add glib:+priority-default+ 100 (lambda () (run-steps *steps*) nil))
(cadre-ui:main :project *root* :init-file nil :quit-after 900)
(format t "~&Timed out.~%")
(uiop:quit 1)
