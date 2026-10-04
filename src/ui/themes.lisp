;;;; themes.lisp — color themes: syntax colors, editor colors, and CSS
;;;;
;;;; A theme maps faces (:comment, :string, (:paren 0), :repl-prompt, …) to
;;;; text tag properties, and may set the editor's background and
;;;; foreground and add GTK CSS. A theme can inherit the faces it leaves out
;;;; from another. There is a preferred light theme and a preferred dark
;;;; one; *color-scheme* says whether to follow the system's light or dark
;;;; style or force one. Your own themes go in ~/.config/cadre/themes/*.lisp
;;;; (or your init file), written with DEFINE-THEME.

(in-package #:cadre-ui)

(defstruct (theme (:conc-name theme-))
  name title dark inherit background foreground css faces)

(defvar *themes* (make-hash-table :test 'equal) "Theme name (a string) → theme.")

(defmacro define-theme (name (&key title dark inherit background foreground css) &body faces)
  "Define a color theme. NAME is a string or symbol. Each of FACES is
(face property value …), with GtkTextTag properties such as :foreground,
:background, :paragraph-background, :style, :weight, :underline and
:underline-rgba. Faces left out come from the theme INHERIT names (by
default cadre-dark for DARK themes, else cadre-light). BACKGROUND and
FOREGROUND color the editor; CSS is extra GTK CSS while the theme is on."
  (let ((name (string-downcase (string name))))
    `(progn
       (setf (gethash ,name *themes*)
             (make-theme :name ,name :title ,(or title (string-capitalize (substitute #\Space #\- name)))
                         :dark ,dark :inherit ,inherit :background ,background :foreground ,foreground
                         :css ,css :faces ',faces))
       (when *window* (apply-theme))
       ,name)))

(defun find-theme (name) (gethash (string-downcase (string name)) *themes*))

(defun list-themes ()
  (sort (loop for theme being the hash-values of *themes* collect theme) #'string< :key #'theme-title))

(define-option *color-scheme* :system (member :system :light :dark)
  "Light or dark: :system follows the system's style, :light and :dark force one."
  :category "Appearance")

(define-option *light-theme* "cadre-light" string
  "The color theme to use when the style is light. See M-x choose-theme."
  :category "Appearance")

(define-option *dark-theme* "cadre-dark" string
  "The color theme to use when the style is dark."
  :category "Appearance")

(defun current-theme ()
  (or (find-theme (if (adw:dark-p) *dark-theme* *light-theme*))
      (find-theme (if (adw:dark-p) "cadre-dark" "cadre-light"))))

(defun theme-parent (theme)
  (let ((parent (or (theme-inherit theme) (if (theme-dark theme) "cadre-dark" "cadre-light"))))
    (unless (equal parent (theme-name theme)) (find-theme parent))))

(defun theme-face (face &optional (theme (current-theme)))
  "The properties THEME gives FACE (a plist), looking in the themes it inherits from."
  (loop for th = theme then (theme-parent th)
        while th
        do (let ((entry (assoc face (theme-faces th) :test #'equal)))
             (when entry (return (rest entry))))))

(defun theme-color (face key &optional default)
  (or (getf (theme-face face) key) default))

;;; Styling text tags

(defparameter *tag-style-properties*
  '(:foreground :background :paragraph-background :style :weight :underline :underline-rgba :strikethrough)
  "The tag properties themes set. Restyling first unsets them all.")

(defun hex-rgba (string)
  (let ((rgba (gdk:make-rgba)))
    (gdk:rgba-parse rgba string)
    rgba))

(defun restyle-tag (tag face)
  "Give TAG the look the current theme gives FACE."
  (dolist (key *tag-style-properties*)
    (setf (gobject:property tag (intern (format nil "~a-SET" key) :keyword)) nil))
  (loop for (key value) on (theme-face face) by #'cddr
        do (setf (gobject:property tag key) (if (eq key :underline-rgba) (hex-rgba value) value))))

(defun ensure-face-tag (gtk-buffer name face)
  "The tag NAME in GTK-BUFFER, made and styled as FACE if it is new."
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (or (gtk:text-tag-table-lookup table name)
        (let ((tag (make-instance 'gtk:text-tag :name name)))
          (gtk:text-tag-table-add table tag)
          (restyle-tag tag face)
          tag))))

(defparameter *named-tag-faces*
  '(("cadre-repl-prompt" . :repl-prompt) ("cadre-repl-result" . :repl-result) ("cadre-repl-note" . :repl-note)
    ("cadre-repl-presentation" . :repl-presentation)
    ("cadre-inspector-value" . :inspector-value) ("cadre-inspector-action" . :inspector-action)
    ("cadre-diff-added" . :diff-added) ("cadre-diff-removed" . :diff-removed)
    ("cadre-note-error" . :note-error) ("cadre-note-warning" . :note-warning) ("cadre-note-style" . :note-style))
  "Tags outside the syntax faces that themes style, by name.")

(defun restyle-named-tags (gtk-buffer)
  (let ((table (gtk:text-buffer-get-tag-table gtk-buffer)))
    (loop for (name . face) in *named-tag-faces*
          for tag = (gtk:text-tag-table-lookup table name)
          when tag do (restyle-tag tag face))))

;;; Applying the theme

(defvar *theme-provider* nil "The CSS provider for the current theme's colors.")
(defvar *other-text-buffers* '()
  "Text buffers not in a Cadre buffer that use theme faces (the inspector's, reviews).")

(defun theme-stylesheet (theme)
  (let ((background (loop for th = theme then (theme-parent th) while th
                          thereis (theme-background th)))
        (foreground (loop for th = theme then (theme-parent th) while th
                          thereis (theme-foreground th))))
    (format nil "~@[textview.cadre-editor, textview.cadre-editor text, .cadre-gutter { background-color: ~a; }~]~
~@[textview.cadre-editor text { color: ~a; } .cadre-gutter { color: ~:*~a; }~]~@[~a~]"
            background foreground (theme-css theme))))

(defun color-scheme-value ()
  (ecase *color-scheme* (:system :default) (:light :force-light) (:dark :force-dark)))

(defun apply-theme ()
  "Use the current theme everywhere."
  (adw:style-manager-set-color-scheme (adw:style-manager-get-default) (color-scheme-value))
  (let ((theme (current-theme)))
    (when *theme-provider*
      (gtk:style-context-remove-provider-for-display (gdk:display-get-default) *theme-provider*)
      (setf *theme-provider* nil))
    (when theme
      (let ((css (theme-stylesheet theme)))
        (when (plusp (length css))
          (setf *theme-provider* (gtk:add-css css :priority (1+ gtk:+style-provider-priority-application+)))))))
  (restyle-all-buffers))

(defun load-user-themes ()
  "Load the themes in ~/.config/cadre/themes/. Errors are reported, not fatal."
  (dolist (file (directory (merge-pathnames "themes/*.lisp" (config-directory))))
    (handler-case (let ((*package* (find-package :cadre-user))) (load file))
      (error (e) (format *error-output* "~&Error in theme ~a: ~a~%" file e)))))

(defun theme-choices-for (dark)
  (remove dark (list-themes) :key #'theme-dark :test-not #'eq))

(define-command choose-theme ()
  "Choose a color theme. It becomes the preferred theme for light or dark style, and is shown now."
  (open-picker (window-picker *window*)
               :items (list-themes)
               :label #'theme-title
               :detail (lambda (th) (if (theme-dark th) "Dark" "Light"))
               :placeholder "Color theme"
               :on-choose (lambda (theme)
                            (if (theme-dark theme)
                                (progn (setf *dark-theme* (theme-name theme)) (save-option '*dark-theme*))
                                (progn (setf *light-theme* (theme-name theme)) (save-option '*light-theme*)))
                            ;; Show it now: switch the style if the system's doesn't match.
                            (unless (and (eq *color-scheme* :system)
                                         (eq (and (adw:dark-p) t) (and (theme-dark theme) t)))
                              (setf *color-scheme* (if (theme-dark theme) :dark :light))
                              (save-option '*color-scheme*))
                            (apply-theme)
                            (message "Theme: ~a" (theme-title theme)))))

(define-command toggle-dark-style ()
  "Switch between the light and dark style (and their themes)."
  (setf *color-scheme* (if (adw:dark-p) :light :dark))
  (save-option '*color-scheme*)
  (apply-theme))

;;; Built-in themes

(define-theme cadre-light (:title "Cadre Light")
  (:comment :foreground "#7c828c" :style :italic)
  (:string :foreground "#2a7a2f")
  (:number :foreground "#b35c00")
  (:character :foreground "#b35c00")
  (:keyword :foreground "#8a3fb1")
  (:builtin :foreground "#1d5fb8")
  (:definer :foreground "#a3299e")
  (:definition-name :foreground "#0b6e80")
  (:special-variable :foreground "#b8322a")
  (:constant :foreground "#b35c00")
  (:lambda-keyword :foreground "#8a3fb1")
  (:reader-conditional :foreground "#7c828c" :style :italic)
  (:quote :foreground "#8a3fb1")
  (:invalid :foreground "#ffffff" :background "#d0312d")
  (:macro :foreground "#1d5fb8")
  (:undefined-function :underline :single :underline-rgba "#d0312d")
  ((:paren 0) :foreground "#a35a1c")
  ((:paren 1) :foreground "#2b6cb0")
  ((:paren 2) :foreground "#2f855a")
  ((:paren 3) :foreground "#9b2c9b")
  ((:paren 4) :foreground "#b7791f")
  ((:paren 5) :foreground "#2c7a7b")
  (:current-line :paragraph-background "#f2f4f7")
  (:search :background "#fff1a8")
  (:search-current :background "#ffc94d")
  (:paren-match :background "#cfe3ff" :weight 700)
  (:paren-mismatch :background "#ffc9c9" :weight 700)
  (:note-error :underline :error :underline-rgba "#d0312d")
  (:note-warning :underline :error :underline-rgba "#c27c0e")
  (:note-style :underline :error :underline-rgba "#8a8f98")
  (:repl-prompt :foreground "#1d5fb8" :weight 700)
  (:repl-result :foreground "#8a5a00")
  (:repl-presentation :foreground "#8a5a00" :underline :single :underline-rgba "#d9c7a3")
  (:repl-note :foreground "#7c828c" :style :italic)
  (:inspector-value :foreground "#2b6cb0")
  (:inspector-action :foreground "#a3299e" :underline :single)
  (:diff-added :paragraph-background "#dcf5e3")
  (:diff-removed :paragraph-background "#fbe0e0" :strikethrough t)
  (:md-heading :foreground "#1d5fb8" :weight 700)
  (:md-strong :weight 700)
  (:md-emphasis :style :italic)
  (:md-strike :strikethrough t)
  (:md-code :foreground "#a3299e")
  (:md-code-block :paragraph-background "#f3f4f6")
  (:md-link :foreground "#0b6e80" :underline :single)
  (:md-url :foreground "#7c828c")
  (:md-markup :foreground "#a0a6ae")
  (:md-quote :foreground "#5c6370" :style :italic)
  (:md-list :foreground "#b35c00" :weight 700))

(define-theme cadre-dark (:title "Cadre Dark" :dark t)
  (:comment :foreground "#7f848e" :style :italic)
  (:string :foreground "#98c379")
  (:number :foreground "#d19a66")
  (:character :foreground "#d19a66")
  (:keyword :foreground "#c678dd")
  (:builtin :foreground "#61afef")
  (:definer :foreground "#e386d8")
  (:definition-name :foreground "#e5c07b")
  (:special-variable :foreground "#e06c75")
  (:constant :foreground "#d19a66")
  (:lambda-keyword :foreground "#c678dd")
  (:reader-conditional :foreground "#7f848e" :style :italic)
  (:quote :foreground "#c678dd")
  (:invalid :foreground "#ffffff" :background "#be3a34")
  (:macro :foreground "#61afef")
  (:undefined-function :underline :single :underline-rgba "#ff6b66")
  ((:paren 0) :foreground "#d19a66")
  ((:paren 1) :foreground "#61afef")
  ((:paren 2) :foreground "#98c379")
  ((:paren 3) :foreground "#c678dd")
  ((:paren 4) :foreground "#e5c07b")
  ((:paren 5) :foreground "#56b6c2")
  (:current-line :paragraph-background "#2a2d33")
  (:search :background "#5c4d18")
  (:search-current :background "#9a7612")
  (:paren-match :background "#3d4c66" :weight 700)
  (:paren-mismatch :background "#6b2626" :weight 700)
  (:note-error :underline :error :underline-rgba "#ff6b66")
  (:note-warning :underline :error :underline-rgba "#e5a50a")
  (:note-style :underline :error :underline-rgba "#7f848e")
  (:repl-prompt :foreground "#61afef" :weight 700)
  (:repl-result :foreground "#e5c07b")
  (:repl-presentation :foreground "#e5c07b" :underline :single :underline-rgba "#6b5a35")
  (:repl-note :foreground "#7f848e" :style :italic)
  (:inspector-value :foreground "#61afef")
  (:inspector-action :foreground "#e386d8" :underline :single)
  (:diff-added :paragraph-background "#1f3d2a")
  (:diff-removed :paragraph-background "#4a2326" :strikethrough t)
  (:md-heading :foreground "#61afef" :weight 700)
  (:md-strong :weight 700)
  (:md-emphasis :style :italic)
  (:md-strike :strikethrough t)
  (:md-code :foreground "#e386d8")
  (:md-code-block :paragraph-background "#262a30")
  (:md-link :foreground "#56b6c2" :underline :single)
  (:md-url :foreground "#7f848e")
  (:md-markup :foreground "#666b75")
  (:md-quote :foreground "#9da5b4" :style :italic)
  (:md-list :foreground "#d19a66" :weight 700))

(define-theme solarized-light (:title "Solarized Light" :background "#fdf6e3" :foreground "#586e75")
  (:comment :foreground "#93a1a1" :style :italic)
  (:string :foreground "#2aa198")
  (:number :foreground "#d33682")
  (:character :foreground "#d33682")
  (:keyword :foreground "#6c71c4")
  (:builtin :foreground "#859900")
  (:definer :foreground "#859900" :weight 700)
  (:definition-name :foreground "#268bd2")
  (:special-variable :foreground "#cb4b16")
  (:constant :foreground "#b58900")
  (:lambda-keyword :foreground "#6c71c4")
  (:quote :foreground "#6c71c4")
  (:macro :foreground "#859900")
  ((:paren 0) :foreground "#b58900")
  ((:paren 1) :foreground "#268bd2")
  ((:paren 2) :foreground "#859900")
  ((:paren 3) :foreground "#d33682")
  ((:paren 4) :foreground "#cb4b16")
  ((:paren 5) :foreground "#2aa198")
  (:current-line :paragraph-background "#eee8d5")
  (:paren-match :background "#e0d9c3" :weight 700))

(define-theme solarized-dark (:title "Solarized Dark" :dark t :background "#002b36" :foreground "#93a1a1")
  (:comment :foreground "#586e75" :style :italic)
  (:string :foreground "#2aa198")
  (:number :foreground "#d33682")
  (:character :foreground "#d33682")
  (:keyword :foreground "#6c71c4")
  (:builtin :foreground "#859900")
  (:definer :foreground "#859900" :weight 700)
  (:definition-name :foreground "#268bd2")
  (:special-variable :foreground "#cb4b16")
  (:constant :foreground "#b58900")
  (:lambda-keyword :foreground "#6c71c4")
  (:quote :foreground "#6c71c4")
  (:macro :foreground "#859900")
  ((:paren 0) :foreground "#b58900")
  ((:paren 1) :foreground "#268bd2")
  ((:paren 2) :foreground "#859900")
  ((:paren 3) :foreground "#d33682")
  ((:paren 4) :foreground "#cb4b16")
  ((:paren 5) :foreground "#2aa198")
  (:current-line :paragraph-background "#073642")
  (:paren-match :background "#0d4a5a" :weight 700))

(define-theme high-contrast-dark (:title "High Contrast Dark" :dark t :background "#000000" :foreground "#ffffff")
  (:comment :foreground "#a0a0a0" :style :italic)
  (:string :foreground "#7fff7f")
  (:number :foreground "#ffb86c")
  (:keyword :foreground "#ff9ff3")
  (:builtin :foreground "#7fdbff")
  (:definer :foreground "#ff9ff3" :weight 700)
  (:definition-name :foreground "#ffff66" :weight 700)
  (:special-variable :foreground "#ff7070")
  (:current-line :paragraph-background "#1a1a1a")
  (:paren-match :background "#2050a0" :weight 700))
