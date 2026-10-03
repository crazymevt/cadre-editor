;;;; cadre.asd — Cadre, a Lisp editor

(defsystem "cadre"
  :description "Cadre: a Common Lisp editor with Emacs's depth and VS Code's interface."
  :author "Jessie Hughart"
  :license "MIT"
  :version "0.0.1"
  :depends-on ("cadre/core" "gtk4" "gtk4-adwaita")
  :pathname "src/ui/"
  :serial t
  :components ((:file "package")
               (:file "gtk-text")
               (:file "settings")
               (:file "keys")
               (:file "editor-view")
               (:file "explorer")
               (:file "panel")
               (:file "window")
               (:file "tabs")
               (:file "layout")
               (:file "commands")
               (:file "bindings")
               (:file "app"))
  :in-order-to ((test-op (test-op "cadre/tests"))))

(defsystem "cadre/core"
  :description "Cadre's editor model, with no GTK dependency: text, buffers, commands, keymaps, modes, hooks, options."
  :depends-on ()
  :pathname "src/core/"
  :serial t
  :components ((:file "package")
               (:file "hooks")
               (:file "options")
               (:file "text")
               (:file "keymaps")
               (:file "modes")
               (:file "buffers")
               (:file "editor")
               (:file "commands")))

(defsystem "cadre/tests"
  :description "Headless tests for cadre/core."
  :depends-on ("cadre/core" "parachute")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "buffers")
               (:file "commands")
               (:file "keymaps"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :cadre-tests)))
