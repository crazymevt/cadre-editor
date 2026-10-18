;;;; project-search.lisp — find (and replace) in every file of the project,
;;;; and renaming a symbol everywhere
;;;;
;;;; The Search page of the sidebar. Searching runs on a thread over the
;;;; project's files (what the explorer shows), using open buffers' text for
;;;; open files, so unsaved changes count. Results are grouped by file;
;;;; clicking one opens it there. Replacing shows a check box on each match:
;;;; Replace All changes the checked ones, in open buffers as one undo step
;;;; each, and in other files on disk.
;;;;
;;;; Renaming a symbol uses the same page. A local variable or function (one
;;;; bound around the cursor) is renamed in :lexical mode: the places in its
;;;; form that refer to that binding (lisp/rename.lisp). Any other symbol is
;;;; renamed in :symbol mode across the project's Lisp files: each
;;;; occurrence of the name (strings and comments aside) is put in its
;;;; package and resolved, by the connected Lisp or else guessed from the
;;;; IN-PACKAGE forms, so the page can say which are the symbol, which are
;;;; another symbol or a local binding of that name, and which it can't tell.
;;;; Only the first kind start checked.

(in-package #:cadre-ui)

(defstruct (project-search (:conc-name ps-))
  widget entry replace-entry replace-revealer replace-toggle case-button word-button regex-button
  status results apply-button
  (mode :text)                          ; :text, :symbol or :lexical for renaming
  (symbol nil)
  ;; :symbol mode: the package the renamed symbol is read in at the cursor.
  (target-package nil)
  ;; :lexical mode: the buffer, and a mark on the binding's use there.
  (lexical-buffer nil) (lexical-mark nil)
  (results-data '())                    ; (path match-rows), match-rows: (line start end check)
  (rename-contexts (make-hash-table :test 'equal)) ; Per-place context fingerprints.
  (generation 0))

(defvar *project-search* nil)

(defparameter *max-project-matches* 5000)

(defun make-project-search-widget ()
  (let* ((entry (make-instance 'gtk:search-entry :placeholder-text "Search the project" :hexpand t :width-chars 8))
         (replace-entry (make-instance 'gtk:entry :placeholder-text "Replace" :hexpand t :width-chars 6))
         (case-button (make-instance 'gtk:toggle-button :label "Aa" :tooltip-text "Match case" :css-classes '("flat")))
         (word-button (make-instance 'gtk:toggle-button :label "W" :tooltip-text "Whole words only" :css-classes '("flat")))
         (regex-button (make-instance 'gtk:toggle-button :label ".*" :tooltip-text "Regular expression (\\1 in the replacement is the first group)"
                                                         :css-classes '("flat")))
         (replace-toggle (make-instance 'gtk:toggle-button :icon-name "cadre-replace-symbolic"
                                                           :tooltip-text "Replace" :css-classes '("flat")))
         (apply-button (make-instance 'gtk:button :label "Replace All" :css-classes '("suggested-action") :sensitive nil))
         (replace-revealer (gtk:build
                             (gtk:revealer :reveal-child nil :transition-type :slide-down
                               (gtk:box :spacing 4 :margin-top 4 replace-entry apply-button))))
         (status (make-instance 'gtk:label :xalign 0.0 :wrap t :css-classes '("dim-label" "caption")))
         (results (make-instance 'gtk:box :orientation :vertical :spacing 2 :margin-end 4))
         (ps (make-project-search :entry entry :replace-entry replace-entry :replace-revealer replace-revealer
                                  :replace-toggle replace-toggle :case-button case-button :word-button word-button
                                  :regex-button regex-button
                                  :status status :results results :apply-button apply-button)))
    (setf *project-search* ps)
    (gobject:connect entry :activate (lambda (e) (declare (ignore e)) (setf (ps-mode ps) :text) (run-project-search)))
    (gobject:connect entry :search-changed
                     (lambda (e) (declare (ignore e))
                       (when (eq (ps-mode ps) :text) (schedule-project-search))))
    (dolist (b (list case-button word-button regex-button))
      (gobject:connect b :toggled (lambda (b) (declare (ignore b)) (run-project-search))))
    (gobject:connect replace-toggle :toggled
                     (lambda (b) (show-replace-row (gtk:toggle-button-get-active b))))
    (gobject:connect replace-entry :activate (lambda (e) (declare (ignore e)) (confirm-project-replace)))
    (gobject:connect apply-button :clicked (lambda (b) (declare (ignore b)) (confirm-project-replace)))
    (setf (ps-widget ps)
          (gtk:build
            (gtk:box :orientation :vertical :spacing 4 :margin-start 8 :margin-end 6
              (gtk:label :label "SEARCH" :xalign 0.0 :margin-start 4 :margin-top 8 :margin-bottom 4
                         :css-classes '("caption-heading" "dim-label"))
              entry
              (gtk:box :spacing 0 case-button word-button regex-button
                (gtk:box :hexpand t)
                replace-toggle)
              replace-revealer
              status
              (gtk:scrolled-window :vexpand t :hscrollbar-policy :never :child results))))))

(defun show-replace-row (on)
  (let ((ps *project-search*))
    (gtk:revealer-set-reveal-child (ps-replace-revealer ps) on)
    (unless (eq (gtk:toggle-button-get-active (ps-replace-toggle ps)) on)
      (gtk:toggle-button-set-active (ps-replace-toggle ps) on))
    (when (eq (ps-mode ps) :text)
      (gtk:button-set-label (ps-apply-button ps) "Replace All"))
    ;; Check boxes show only while replacing.
    (loop for (nil rows) in (ps-results-data ps)
          do (loop for row in rows do (gtk:widget-set-visible (fourth row) on)))))

(defvar *project-search-timer* nil)

(defun schedule-project-search ()
  (when *project-search-timer* (glib:source-remove *project-search-timer*))
  (setf *project-search-timer*
        (glib:timeout-add glib:+priority-default+ 350
                          (lambda () (setf *project-search-timer* nil) (run-project-search) nil))))

(defun relative-name (path)
  (let ((project (window-project *window*)))
    (if project
        (or (ignore-errors (enough-namestring path project)) (namestring path))
        (namestring path))))

(defun lisp-file-p (path) (eq (major-mode-for-file path) 'lisp-mode))

(defun run-project-search ()
  "Search the project for the entry's text (or, in symbol mode, the symbol), on a thread."
  (let* ((ps *project-search*)
         (pattern (gtk:editable-get-text (ps-entry ps)))
         (project (window-project *window*))
         (mode (ps-mode ps))
         (case-sensitive (gtk:toggle-button-get-active (ps-case-button ps)))
         (whole-word (gtk:toggle-button-get-active (ps-word-button ps)))
         (regex (gtk:toggle-button-get-active (ps-regex-button ps)))
         (bad-regex (and regex (eq mode :text)
                         (handler-case (progn (regex-scanner pattern) nil)
                           (editor-error (e) (editor-error-message e)))))
         (generation (incf (ps-generation ps)))
         ;; Open buffers' text, read here on the GUI thread.
         (open-texts (loop for buffer in (buffer-list)
                           when (buffer-file buffer)
                             collect (cons (namestring (or (probe-file (buffer-file buffer)) (buffer-file buffer)))
                                           (buffer-string buffer)))))
    (cond
      ((eq mode :lexical) (show-lexical-rename ps))
      ((null project) (gtk:label-set-text (ps-status ps) "Open a folder to search it."))
      ((string= pattern "") (show-project-results ps '() 0) (gtk:label-set-text (ps-status ps) ""))
      (bad-regex (show-project-results ps '() 0) (gtk:label-set-text (ps-status ps) bad-regex))
      (t
       (gtk:label-set-text (ps-status ps) "Searching…")
       (sb-thread:make-thread
        (lambda ()
          (let ((results '()) (count 0))
            (dolist (path (project-files project :hidden-names *explorer-hidden-names*
                                                 :hidden-types *explorer-hidden-types*))
              (when (< count *max-project-matches*)
                (let* ((text (or (cdr (assoc (namestring path) open-texts :test #'string=))
                                 (read-text-file path)))
                       (matches (and text
                                     (if (eq mode :symbol)
                                         (and (lisp-file-p path) (ignore-errors (symbol-contexts text pattern)))
                                         (text-matches text pattern :case-sensitive case-sensitive
                                                                    :whole-word whole-word :regex regex)))))
                  (when matches
                    (incf count (length matches))
                    (push (list path (coerce (split-text-lines text) 'vector) matches
                                (when (eq mode :symbol)
                                  (safe-rename-contexts text
                                                   (mapcar (lambda (context)
                                                             (list (sc-line context) (sc-column context)
                                                                   (+ (sc-column context) (- (sc-end context) (sc-start context)))))
                                                           matches))))
                          results)))))
            (deliver-to-gui (lambda ()
                              (when (= generation (ps-generation ps))
                                (if (eq mode :symbol)
                                    (classify-symbol-results ps (nreverse results) count generation)
                                    (show-project-results ps (nreverse results) count)))))))
        :name "project search")))))

;;; Which occurrences are the symbol (rename.lisp classifies them)

(defparameter *rename-kind-notes*
  '((:export . "DEFPACKAGE clause") (:local . "local binding") (:other . "another symbol")
    (:unknown . "can't tell") (:other-binding . "another binding"))
  "What a match's kind says beside it, when its own note says nothing more.")

(defun rename-checked-kind-p (kind)
  "Whether a match of KIND starts checked: the symbol itself, or the binding."
  (member kind '(nil :same :export :binding)))

(defun symbol-name-for-lookup (name)
  "NAME as the reader interns it: upper case, unless written with bars."
  (if (find #\| name) (remove #\| name) (string-upcase name)))

(defun symbol-homes-source (name packages)
  "Source giving, for each of PACKAGES, the home package of the symbol NAME
read there (the package itself if it would be interned there), or nil if
there is no such package."
  (format nil "(cl:let ((name ~a)) (cl:mapcar (cl:lambda (p) (cl:let ((pkg (cl:find-package p))) (cl:and pkg (cl:multiple-value-bind (s status) (cl:find-symbol name pkg) (cl:if status (cl:let ((home (cl:symbol-package s))) (cl:and home (cl:package-name home))) (cl:package-name pkg)))))) '~a))"
          (cadre::lisp-string (symbol-name-for-lookup name))
          (with-standard-io-syntax (prin1-to-string packages))))

(defun parse-symbol-homes (reply packages)
  "A table of package → home package from the image's REPLY, or nil."
  (let ((homes (ignore-errors
                (with-standard-io-syntax
                  ;; Read as the image printed it, so NIL is nil; CL is locked,
                  ;; so a reply that isn't strings and NIL can't intern into it.
                  (let ((*read-eval* nil) (*package* (find-package :common-lisp)))
                    (read-from-string (second reply)))))))
    (when (and (listp homes) (= (length homes) (length packages)) (every (lambda (h) (or (null h) (stringp h))) homes))
      (let ((table (make-hash-table :test 'equal)))
        (loop for package in packages for home in homes do (setf (gethash package table) home))
        table))))

(defun classified-matches (contexts target-home resolve)
  "CONTEXTS as matches (line start end kind note), columns in the line."
  (loop for context in contexts
        collect (multiple-value-bind (kind note) (classify-symbol-context context target-home resolve)
                  (list (sc-line context) (sc-column context)
                        (+ (sc-column context) (- (sc-end context) (sc-start context)))
                        kind note))))

(defun classify-symbol-results (ps results count generation)
  "Show RESULTS, each file's symbol-contexts, with what each occurrence is:
asking the connected Lisp which symbol each package's occurrences read as,
or guessing from the packages' names without one."
  (let* ((target (ps-target-package ps))
         (packages (remove-duplicates (append (and target (list target))
                                              (loop for (nil nil contexts) in results append (context-packages contexts)))
                                      :test #'string=)))
    (labels ((show (resolve how)
               (when (= generation (ps-generation ps))
                 (let ((target-home (and target (funcall resolve target))))
                   (show-project-results ps (loop for (path lines contexts fingerprints) in results
                                                  collect (list path lines (classified-matches contexts target-home resolve) fingerprints))
                                         count)
                   (show-rename-summary ps how))))
             (guess ()
               ;; Without the image: an occurrence read in the target's own
               ;; package is the symbol; elsewhere it can't be told.
               (show (lambda (package) (and target (string= package target) target)) :guessed)))
      (if (and (connected-p) packages (ps-symbol ps))
          (rex *connection* (swank-call "swank:eval-and-grab-output" (symbol-homes-source (ps-symbol ps) packages))
               :package "COMMON-LISP-USER"
               :on-ok (lambda (reply)
                        (let ((table (parse-symbol-homes reply packages)))
                          (if table
                              ;; A package the image doesn't have (the project
                              ;; isn't loaded) is guessed as without the image.
                              (let ((target-home (or (and target (gethash target table)) target)))
                                (show (lambda (package)
                                        (or (gethash package table)
                                            (and target (string= package target) target-home)))
                                      :image))
                              (guess))))
               :on-abort (lambda (reason) (declare (ignore reason)) (guess)))
          (guess)))))

(defun show-rename-summary (ps how)
  "Say how many of the matches are the symbol, and how they were told apart."
  (let ((counts (make-hash-table)))
    (loop for (nil rows) in (ps-results-data ps)
          do (loop for row in rows do (incf (gethash (fifth row) counts 0))))
    (let ((same (+ (gethash :same counts 0) (gethash :export counts 0) (gethash nil counts 0)))
          (unknown (gethash :unknown counts 0))
          (others (+ (gethash :other counts 0) (gethash :local counts 0))))
      (gtk:label-set-text
       (ps-status ps)
       (format nil "~d place~:p of ~a~[~:;, ~:*~d it can't tell (unchecked: review them)~]~[~:;, ~:*~d other symbol~:p or local binding~:p (unchecked)~]. ~a"
               same (ps-symbol ps) unknown others
               (if (eq how :image)
                   "Packages from the running Lisp."
                   "Not connected: packages guessed from IN-PACKAGE forms."))))))

;;; Renaming a local variable or function

(defun offset-line-column (starts offset)
  "OFFSET as (values line column), STARTS the text's line-starts vector."
  (let ((line (1- (or (position-if (lambda (s) (> s offset)) starts) (length starts)))))
    (values line (- offset (aref starts line)))))

(defun show-lexical-rename (ps)
  "Show the places of the local binding at the lexical mark: the ones that
refer to it checked, others of the same name in its form unchecked."
  (let* ((buffer (ps-lexical-buffer ps))
         (gtk-buffer (and buffer (buffer-text buffer)))
         (offset (and gtk-buffer (member buffer (buffer-list))
                      (gtk:text-iter-get-offset (gtk:text-buffer-get-iter-at-mark gtk-buffer (ps-lexical-mark ps)))))
         (text (and offset (buffer-string buffer))))
    (multiple-value-bind (places others name certain) (and text (lexical-rename-places text offset))
      (if (null places)
          (progn (show-project-results ps '() 0)
                 (gtk:label-set-text (ps-status ps) "No local binding there any more."))
          (let ((starts (coerce (cadre::line-starts text) 'vector)))
            (flet ((match (place kind note)
                     (multiple-value-bind (line column) (offset-line-column starts (first place))
                       (list line column (+ column (- (second place) (first place))) kind note))))
              (setf (ps-symbol ps) name)
              (show-project-results ps (list (list (buffer-file buffer) (coerce (split-text-lines text) 'vector)
                                                   (sort (append (mapcar (lambda (p) (match p :binding nil)) places)
                                                                 (mapcar (lambda (p) (match p :other-binding nil)) others))
                                                         (lambda (a b) (or (< (first a) (first b))
                                                                           (and (= (first a) (first b)) (< (second a) (second b))))))))
                                    (+ (length places) (length others)))
              (gtk:label-set-text (ps-status ps)
                                  (format nil "Local ~a: ~d place~:p in its form~[~:;; ~:*~d other~:p of that name left unchecked~].~:[ (Its binding is guessed from a WITH- macro.)~;~]"
                                          name (length places) (length others) certain))))))))

(defun match-markup (line start end)
  (let* ((from (max 0 (- start 40)))
         (to (min (length line) (+ end 80))))
    (format nil "~a<b>~a</b>~a"
            (glib:markup-escape-text (string-left-trim " 	" (subseq line from start)) -1)
            (glib:markup-escape-text (subseq line start end) -1)
            (glib:markup-escape-text (subseq line end to) -1))))

(defun show-project-results (ps results count)
  (clear-box (ps-results ps))
  (clrhash (ps-rename-contexts ps))
  (when (member (ps-mode ps) '(:symbol :lexical))
    (dolist (result results)
      (setf (gethash (first result) (ps-rename-contexts ps))
            (or (fourth result)
                (safe-rename-contexts (format nil "~{~a~^~%~}" (coerce (second result) 'list))
                                 (mapcar (lambda (match) (subseq match 0 3)) (third result)))))))
  (let ((replacing (gtk:revealer-get-reveal-child (ps-replace-revealer ps))))
    (setf (ps-results-data ps)
          (loop for (path lines matches) in results
                collect (let ((rows '())
                              (box (make-instance 'gtk:box :orientation :vertical)))
                          (dolist (m matches)
                            ;; A rename's match has a KIND (rename.lisp) and a NOTE
                            ;; saying why it isn't the symbol; only the symbol's start checked.
                            (destructuring-bind (line start end &optional kind note) m
                              (let* ((failure (rename-validation-note (gethash path (ps-rename-contexts ps))))
                                     (kind (if failure :unknown kind))
                                     (note (or failure note))
                                     (check (make-instance 'gtk:check-button :active (and (not failure) (rename-checked-kind-p kind) t)
                                                                             :sensitive (not failure) :visible replacing))
                                     (label (make-instance 'gtk:label :xalign 0.0 :ellipsize :end :hexpand t
                                                                      :use-markup t
                                                                      :label (match-markup (aref lines line) start end)))
                                     (number (make-instance 'gtk:label :label (princ-to-string (1+ line))
                                                                       :width-chars 4 :xalign 1.0
                                                                       :css-classes '("dim-label" "caption")))
                                     (tag (let ((text (or note (cdr (assoc kind *rename-kind-notes*)))))
                                            (and text (not (rename-checked-kind-p kind))
                                                 (make-instance 'gtk:label :label text :xalign 1.0
                                                                           :css-classes (if (eq kind :unknown)
                                                                                            '("warning" "caption")
                                                                                            '("dim-label" "caption"))))))
                                     (button (make-instance 'gtk:button :css-classes '("flat" "cadre-search-match")
                                                                        :tooltip-text (or note (cdr (assoc kind *rename-kind-notes*)))
                                                                        :child (let ((row (gtk:build (gtk:box :spacing 6 number label))))
                                                                                 (when tag (gtk:box-append row tag))
                                                                                 row))))
                                (let ((path path) (line line) (start start) (end end))
                                  (gobject:connect button :clicked
                                                   (lambda (b) (declare (ignore b)) (show-match path line start end))))
                                (gtk:box-append box (gtk:build (gtk:box check button)))
                                (push (list line start end check kind) rows))))
                          (gtk:box-append (ps-results ps)
                                          (make-instance 'gtk:expander :expanded t :child box
                                                                       :label (format nil "~a  (~d)" (relative-name path)
                                                                                      (length matches))))
                          (list path (nreverse rows)))))
    (gtk:label-set-text (ps-status ps)
                        (cond ((zerop count) "No results.")
                              (t (format nil "~d result~:p in ~d file~:p~:[~;, stopped at ~d~]"
                                         count (length results) (>= count *max-project-matches*)
                                         *max-project-matches*))))
    (gtk:widget-set-sensitive (ps-apply-button ps) (plusp count))))

(defun show-match (path line start end)
  (open-file-path path :then (lambda (view)
                               (let ((gtk-buffer (view-gtk-buffer view)))
                                 (gtk:text-buffer-select-range gtk-buffer (line-iter gtk-buffer line start)
                                                               (line-iter gtk-buffer line end))
                                 (scroll-to-cursor view)))))

;;; Replacing

(defun checked-matches (rows)
  (loop for (line start end check) in rows
        when (gtk:check-button-get-active check) collect (list line start end)))

(defun replace-buffer-text (buffer text)
  "Make BUFFER's text TEXT, changing only the part that differs, as one undo step."
  (let* ((gtk-buffer (buffer-text buffer))
         (old (text-string gtk-buffer))
         (prefix (or (mismatch old text) (length old)))
         (suffix (let ((m (mismatch old text :from-end t))) (if m (- (length old) m) 0)))
         (suffix (min suffix (- (length old) prefix) (- (length text) prefix))))
    (with-user-action (gtk-buffer)
      (gtk:text-buffer-delete gtk-buffer (iter-at gtk-buffer prefix) (iter-at gtk-buffer (- (length old) suffix)))
      (gtk:text-buffer-insert gtk-buffer (iter-at gtk-buffer prefix) (subseq text prefix (- (length text) suffix)) -1))))

(defun write-text-file (path text)
  (with-open-file (out path :direction :output :if-exists :supersede :external-format :utf-8)
    (write-string text out)))

(defun rename-form-spans (tree)
  "Top-level source spans, including reader prefixes and feature expressions."
  (labels ((one (nodes)
             (when nodes
               (let* ((node (first nodes))
                      (token (and (eq (cadre::node-kind node) :atom) (cadre::node-text node)))
                      (rest (rest nodes))
                      (end (cadre::node-end node)))
                 (cond
                   ((member token '("#+" "#-") :test #'equal)
                    (multiple-value-bind (tail feature-end) (one rest)
                      (when feature-end
                        (multiple-value-setq (rest end) (one tail)))))
                   ((member token '("'" "`" "#'" "," ",@" ",." "#.") :test #'equal)
                    (multiple-value-setq (rest end) (one rest))))
                 (values rest end)))))
    (loop while tree
          for start = (cadre::node-start (first tree))
          for end = (multiple-value-bind (rest end) (one tree) (setf tree rest) end)
          when end collect (list start end))))

(defun rename-contexts (text matches)
  "Map each (line start end) to its enclosing form and package context.
Only fingerprints are retained. Unrelated top-level forms may change, but
changed forms, reader prefixes, package declarations and moved places need
another preview. Malformed or missing context is never accepted."
  (let ((contexts (make-hash-table :test 'equal)))
    (when text
      (let* ((tree (cadre::read-form-tree text))
             (spans (rename-form-spans tree))
             (starts (coerce (cadre::line-starts text) 'vector))
             (packages (cadre::in-package-positions tree))
             (pending (sort (loop for match in matches
                                  for line = (first match)
                                  when (and (<= 0 line) (< line (length starts))
                                            (<= 0 (second match) (third match)
                                                (- (if (< (1+ line) (length starts))
                                                       (1- (aref starts (1+ line)))
                                                       (length text))
                                                   (aref starts line))))
                                    collect (cons (+ (aref starts (first match)) (second match)) match))
                            #'< :key #'car))
             (declarations
               (loop for node in tree
                     for head = (first (cadre::child-list node))
                     when (and head (member (cadre::atom-name head)
                                            (cons "in-package" cadre::*defpackage-heads*) :test #'equal))
                       collect (let ((span (find-if (lambda (span)
                                                     (and (<= (first span) (cadre::node-start node))
                                                          (< (cadre::node-start node) (second span)))) spans)))
                                 (coerce (sb-md5:md5sum-string text :start (first span) :end (second span)) 'list)))))
        (loop for (start end) in spans
              do (let ((fingerprint nil))
                   (loop while (and pending (< (caar pending) end))
                         for place = (pop pending)
                         for offset = (car place)
                         for match = (cdr place)
                         when (and (<= start offset)
                                   (<= (+ offset (- (third match) (second match))) end))
                           do (unless fingerprint
                                (setf fingerprint
                                      (list (coerce (sb-md5:md5sum-string text :start start :end end) 'list)
                                            (or (cdr (find-if (lambda (p) (< (car p) start)) packages :from-end t))
                                                "COMMON-LISP-USER")
                                            declarations)))
                              (setf (gethash match contexts) (cons (- offset start) fingerprint)))))))
    contexts))

(defun safe-rename-contexts (text matches)
  "Retain a diagnostic when one file's context cannot be analyzed."
  (handler-case (rename-contexts text matches)
    (error (condition)
      (let ((contexts (make-hash-table :test 'equal)))
        (setf (gethash :error contexts) (format nil "~a" condition))
        contexts))))

(defun rename-validation-note (contexts)
  (let ((error (and contexts (gethash :error contexts))))
    (when error
      (format nil "Context validation failed: ~a. Refresh the preview to retry." error))))

(defun apply-project-replace ()
  "Replace the checked matches. Returns the number of matches and files changed."
  (let* ((ps *project-search*)
         (replacement (gtk:editable-get-text (ps-replace-entry ps)))
         (pattern (gtk:editable-get-text (ps-entry ps)))
         (renaming (member (ps-mode ps) '(:symbol :lexical)))
         (case-sensitive (or renaming (gtk:toggle-button-get-active (ps-case-button ps))))
         (regex (and (eq (ps-mode ps) :text) (gtk:toggle-button-get-active (ps-regex-button ps))))
         (matches 0) (files 0) (skipped 0))
    (loop for (path rows) in (ps-results-data ps)
          for found = (checked-matches rows)
          when found
            do (let* ((buffer (find-file-buffer path))
                      (text (if buffer (buffer-string buffer) (read-text-file path)))
                      (current (and renaming (safe-rename-contexts text found)))
                      (preview (gethash path (ps-rename-contexts ps)))
                      (checked (if renaming
                                   (remove-if-not
                                    (lambda (match)
                                      (let ((context (gethash match current)))
                                        (if (and context preview (equal context (gethash match preview)))
                                            t
                                            (progn (incf skipped) nil))))
                                    found)
                                   found)))
                 (when (and text checked)
                   ;; Keep each match's case when matching ignored it.
                   (let* ((lines (coerce (split-text-lines text) 'vector))
                          (new (with-output-to-string (out)
                                 (let ((pieces (sort (copy-list checked)
                                                     (lambda (a b) (or (< (first a) (first b))
                                                                       (and (= (first a) (first b)) (< (second a) (second b))))))))
                                   (loop for line-text across lines
                                         for n from 0
                                         do (let ((pos 0))
                                              (loop for (l s e) in pieces
                                                    when (= l n)
                                                      do (write-string line-text out :start pos :end s)
                                                         (write-string (cond (regex (regex-replacement pattern (subseq line-text s e) replacement
                                                                                                       :case-sensitive case-sensitive))
                                                                             (case-sensitive replacement)
                                                                             (t (replacement-for (subseq line-text s e) replacement)))
                                                                       out)
                                                         (setf pos e))
                                              (write-string line-text out :start pos))
                                            (when (< n (1- (length lines))) (terpri out)))))))
                     (when checked
                       (if buffer (replace-buffer-text buffer new) (write-text-file path new))
                       (incf matches (length checked))
                       (incf files))))))
    (values matches files skipped)))

(defun confirm-project-replace ()
  (let* ((ps *project-search*)
         (count (loop for (nil rows) in (ps-results-data ps) sum (length (checked-matches rows))))
         (renaming (member (ps-mode ps) '(:symbol :lexical)))
         (replacement (gtk:editable-get-text (ps-replace-entry ps))))
    (cond
      ((zerop count) (message "Nothing to replace"))
      ((and renaming (string= replacement "")) (message "Type the new name first"))
      (t
       (let ((dialog (adw:alert-dialog-new
                      (if renaming
                          (format nil "Rename ~:[~;the local ~]~a to ~a?" (eq (ps-mode ps) :lexical) (ps-symbol ps) replacement)
                          (format nil "Replace ~d match~:*~[es~;~:;es~] with “~a”?" count replacement))
                      (format nil "~d place~:p in ~d file~:p. Open files change in their buffers (undo works there); other files are written to disk."
                              count (count-if (lambda (r) (checked-matches (second r))) (ps-results-data ps))))))
         (adw:alert-dialog-add-response dialog "cancel" "_Cancel")
         (adw:alert-dialog-add-response dialog "replace" (if renaming "_Rename" "_Replace"))
         (adw:alert-dialog-set-response-appearance dialog "replace" :suggested)
         (adw:alert-dialog-set-close-response dialog "cancel")
         (gio:async (adw:alert-dialog-choose dialog (window-gtk-window *window*))
                    (lambda (response)
                      (when (string= response "replace")
                        (finish-project-replace ps renaming replacement)))))))))

(defun finish-project-replace (ps renaming replacement)
  "Apply the checked replacements, say what changed and refresh the results."
  (multiple-value-bind (matches files skipped) (apply-project-replace)
    (message "~:[Replaced~;Renamed~] ~d occurrence~:p in ~d file~:p~[~:;; ~:*~d place~:p left alone because their context changed or could not be validated; review the preview and rename again~]"
             renaming matches files skipped)
    ;; The preview keeps the old name until every checked place is renamed.
    (when (and renaming (zerop skipped))
      (setf (ps-symbol ps) replacement)
      (gtk:editable-set-text (ps-entry ps) replacement))
    (run-project-search)))

;;; Commands

(defun selection-or-symbol-text (&key (symbol t))
  "The selection (if short and on one line), else the symbol at the cursor."
  (let ((view (and *window* (selected-view *window*))))
    (when view
      (let ((gtk-buffer (view-gtk-buffer view)))
        (multiple-value-bind (has start end) (gtk:text-buffer-get-selection-bounds gtk-buffer)
          (or (and has (let ((text (gtk:text-buffer-get-text gtk-buffer start end nil)))
                         (and (< (length text) 200) (not (find #\Newline text)) text)))
              (and symbol (symbol-at-cursor view))))))))

(defun symbol-at-cursor (view)
  (let ((syntax (buffer-sexp-syntax (view-buffer view))))
    (if syntax
        (multiple-value-bind (l c) (cursor-line-column view) (symbol-at syntax l c))
        ;; Outside Lisp, the word at the cursor.
        (let* ((gtk-buffer (view-gtk-buffer view))
               (point (point-offset view))
               (text (text-string gtk-buffer))
               (start (loop for i downfrom point while (and (> i 0) (symbol-constituent-p (char text (1- i)))) finally (return i)))
               (end (loop for i from point while (and (< i (length text)) (symbol-constituent-p (char text i))) finally (return i))))
          (and (< start end) (subseq text start end))))))

(defun open-project-search (&key text (mode :text) symbol target-package replace focus-replace)
  "Show search results with optional SYMBOL and TARGET-PACKAGE context."
  (when (and (eq mode :symbol) symbol)
    (let ((offset (symbol-name-part symbol)))
      (setf target-package (or (prefix-package (subseq symbol 0 offset)) target-package)
            symbol (subseq symbol offset)
            text symbol)))
  (show-sidebar-page *window* "search" :toggle nil)
  (let ((ps *project-search*))
    (setf (ps-mode ps) mode)
    (when symbol (setf (ps-symbol ps) symbol
                       (ps-target-package ps) target-package))
    (gtk:button-set-label (ps-apply-button ps) (if (member mode '(:symbol :lexical)) "Rename" "Replace All"))
    (when text
      (gtk:editable-set-text (ps-entry ps) text))
    (when (and symbol replace (eq mode :symbol))
      (gtk:editable-set-text (ps-replace-entry ps) symbol))
    (show-replace-row replace)
    (run-project-search)
    (if focus-replace
        (progn (gtk:widget-grab-focus (ps-replace-entry ps))
               (gtk:editable-select-region (ps-replace-entry ps) 0 -1))
        (progn (gtk:widget-grab-focus (ps-entry ps))
               (gtk:editable-select-region (ps-entry ps) 0 -1)))))

(define-command find-in-project ()
  "Search every file in the project (the selection, or the symbol at the cursor)."
  (open-project-search :text (selection-or-symbol-text)))

(define-command replace-in-project ()
  "Find and replace in every file in the project."
  (open-project-search :text (selection-or-symbol-text) :replace t))

(defun cursor-symbol-package (view name)
  "The package the symbol NAME at VIEW's cursor is read in: its prefix's, a
DEFPACKAGE clause's, or the IN-PACKAGE before it. Nil outside Lisp."
  (when (eq (buffer-major-mode (view-buffer view)) 'lisp-mode)
    (let* ((point (point-offset view))
           (context (find-if (lambda (c) (<= (sc-start c) point (sc-end c)))
                             (ignore-errors (symbol-contexts (buffer-string (view-buffer view)) name)))))
      (and context
           (or (prefix-package (sc-prefix context))
               (sc-clause-package context)
               (sc-package context))))))

(define-command find-symbol-in-project ()
  "Find every use of the symbol at the cursor in the project's Lisp files (not in strings or comments),
saying which are that symbol and which another of the same name."
  (let* ((name (or (selection-or-symbol-text) (editor-error "No symbol at the cursor")))
         (view (selected-view *window*)))
    (open-project-search :mode :symbol :symbol name
                         :target-package (and view (cursor-symbol-package view name)))))

(define-command rename-symbol ()
  "Rename the symbol at the cursor: review the places in the Search page,
then Rename. A local variable or function is renamed where it refers to that
binding, in its form. Another symbol is renamed in every Lisp file of the
project where an occurrence is that symbol (its package decides; others of
the same name, and those it can't tell, start unchecked). In Racket, the
binding at the cursor and its uses in the file (racket-rename-symbol)."
  (when (eq (buffer-major-mode (current-buffer)) 'racket-mode)
    (return-from rename-symbol (racket-rename-symbol)))
  (let* ((view (selected-view *window*))
         (name (or (and view (symbol-at-cursor view)) (editor-error "No symbol at the cursor")))
         (ps *project-search*)
         (buffer (view-buffer view)))
    (if (and (eq (buffer-major-mode buffer) 'lisp-mode)
             (lexical-rename-places (buffer-string buffer) (point-offset view)))
        ;; A local binding: its form in this buffer.
        (let ((gtk-buffer (buffer-text buffer)))
          (unless (buffer-file buffer) (editor-error "Save the buffer first, to rename in it"))
          (when (and (ps-lexical-buffer ps) (ps-lexical-mark ps) (member (ps-lexical-buffer ps) (buffer-list)))
            (gtk:text-buffer-delete-mark (buffer-text (ps-lexical-buffer ps)) (ps-lexical-mark ps)))
          (setf (ps-lexical-buffer ps) buffer
                (ps-lexical-mark ps) (gtk:text-buffer-create-mark gtk-buffer nil (cursor-iter gtk-buffer) t)
                (ps-symbol ps) name)
          (gtk:editable-set-text (ps-replace-entry ps) name)
          (open-project-search :text name :mode :lexical :replace t :focus-replace t))
        (open-project-search :symbol name :target-package (cursor-symbol-package view name)
                             :mode :symbol :replace t :focus-replace t))))
