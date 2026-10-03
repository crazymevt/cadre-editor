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
               (:file "highlight")
               (:file "editor-view")
               (:file "explorer")
               (:file "panel")
               (:file "window")
               (:file "tabs")
               (:file "layout")
               (:file "commands")
               (:file "picker")
               (:file "search")
               (:file "lisp-commands")
               (:file "session")
               (:file "repl")
               (:file "notes")
               (:file "debugger")
               (:file "completion")
               (:file "lisp-eval")
               (:file "bindings")
               (:file "app"))
  :in-order-to ((test-op (test-op "cadre/tests"))))

(defsystem "cadre/core"
  :description "Cadre's editor model, with no GTK dependency: text, buffers, commands, keymaps, modes, hooks, options."
  :depends-on ("sb-bsd-sockets" "sb-posix")
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
               (:file "commands")
               (:file "fuzzy")
               (:module "lisp"
                :serial t
                :components ((:file "lexer")
                             (:file "syntax")
                             (:file "indent")
                             (:file "faces")))
               (:module "swank"
                :serial t
                :components ((:file "sexp")
                             (:file "connection")
                             (:file "inferior")
                             (:file "forms")))))

(defsystem "cadre/tests"
  :description "Headless tests for cadre/core."
  :depends-on ("cadre/core" "parachute")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "text")
               (:file "buffers")
               (:file "commands")
               (:file "keymaps")
               (:file "lisp")
               (:file "swank"))
  :perform (test-op (op c) (uiop:symbol-call :parachute :test :cadre-tests)))
