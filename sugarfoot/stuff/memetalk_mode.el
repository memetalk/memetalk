(defvar memetalk-mode-hook nil)

; "If your keymap will have very few entries, then you may want to consider
; ‘make-sparse-keymap’ rather than ‘make-keymap’."
(defvar memetalk-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for memetalk major mode")


(add-to-list 'auto-mode-alist '("\\.mm\\'" . memetalk-mode))


(setq memetalk-keywords '("preamble" "code" "endcode" "class" "fields"
                          "init" "fun" "super" "instance_method" "class_method"
                          "if" "else" "try" "end" "catch" "var" "primitive"))

(setq memetalk-constants '("true" "false" "null"))

(setq memetalk-keywords-regexp (regexp-opt memetalk-keywords 'words))
(setq memetalk-constants-regexp (regexp-opt memetalk-constants 'words))

;; font-lock-defaults
;; js--font-lock-keywords-3 js--font-lock-keywords-1 js--font-lock-keywords-2 js--font-lock-keywords-3))

(defconst memetalk-font-lock-keywords
  (list
   `(,memetalk-keywords-regexp . font-lock-keyword-face)
   `(,memetalk-constants-regexp . font-lock-constant-face)
   '("^\\s-*instance_method\\s-+\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" 1 font-lock-variable-name-face)
   '("^\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\):" 1 font-lock-function-name-face)
   '("var\\s-+\\([a-zA-Z_$]\\(?:\\s_\\|\\sw\\)*\\)" 1 font-lock-variable-name-face)
  "Highlighting expressions for memetalk mode"))

;; I believe the following is for c++ style comments
(defvar memetalk-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?* ". 23" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for memetalk-mode")

(defun memetalk-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table memetalk-mode-syntax-table)
  (use-local-map memetalk-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(memetalk-font-lock-keywords))
  ;; (set (make-local-variable 'indent-line-function) 'memetalk-indent-line)
  (setq major-mode 'memetalk-mode)
  (setq mode-name "memetalk")
  (run-hooks 'memetalk-mode-hook))

(provide 'memetalk-mode)
