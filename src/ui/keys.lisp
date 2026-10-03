;;;; keys.lisp — turning GTK key events into Cadre's keys

(in-package #:cadre-ui)

(defparameter *modifier-key-names*
  '("Shift_L" "Shift_R" "Control_L" "Control_R" "Alt_L" "Alt_R" "Meta_L" "Meta_R"
    "Super_L" "Super_R" "Hyper_L" "Hyper_R" "Caps_Lock" "Shift_Lock" "Num_Lock"
    "ISO_Level3_Shift" "ISO_Level5_Shift" "ISO_Next_Group" "ISO_Prev_Group")
  "Keys that only modify other keys; pressing one alone is not a key for Cadre.")

(defparameter *gdk-key-names*
  '(("Return" . "RET") ("KP_Enter" . "RET") ("Tab" . "TAB") ("ISO_Left_Tab" . "TAB")
    ("KP_Tab" . "TAB") ("space" . "SPC") ("KP_Space" . "SPC") ("Escape" . "ESC")
    ("BackSpace" . "DEL")))

(defun macos-p ()
  (member :darwin *features*))

(defun modifier-list (state)
  (cond ((listp state) state)
        ((keywordp state) (list state))
        (t '())))

(defun event-key (keyval state &key super-as-control)
  "The canonical key for a key press of KEYVAL with modifier STATE (as GTK
reports them), or nil for a modifier key pressed alone. With
SUPER-AS-CONTROL, Super (Command on macOS) counts as Control."
  (let ((name (gdk:keyval-name keyval)))
    (when (and name (not (member name *modifier-key-names* :test #'string=)))
      (let* ((mods (modifier-list state))
             (shift (member :shift-mask mods))
             (control (member :control-mask mods))
             (meta (member :alt-mask mods))
             (super (or (member :super-mask mods) (member :meta-mask mods)))
             (lower (gdk:keyval-to-lower keyval))
             (code (gdk:keyval-to-unicode lower))
             (char (and (> code 32) (/= code 127) (code-char code)))
             (key-name (cond ((and char (alpha-char-p char)) (string char))
                             ((and char (graphic-char-p char))
                              ;; Punctuation: use the character typed, which
                              ;; already includes Shift ("?" rather than S-/).
                              (let ((typed (gdk:keyval-to-unicode keyval)))
                                (if (> typed 32) (string (code-char typed)) (string char))))
                             (t (or (cdr (assoc name *gdk-key-names* :test #'string=)) name))))
             (shift (or shift (string= name "ISO_Left_Tab"))))
        (when (and super-as-control super)
          (setf control t super nil))
        (make-key key-name :control control :meta meta :super super :shift shift)))))

(defun key-modifiers (key)
  "The modifier letters of the canonical KEY, such as (#\\C #\\S) for \"C-S-p\"."
  (loop for i from 0 by 2
        while (and (< (+ i 2) (length key)) (char= (char key (1+ i)) #\-))
        collect (char key i)))

(defun plain-key-p (key)
  "True for KEY without Control, Meta or Super: typing, unless a prefix is pending."
  (not (intersection (key-modifiers key) '(#\C #\M #\s))))
