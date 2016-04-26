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
  (memetalk-repl-setup-reconnection)
  (run-hooks 'memetalk-mode-hook))

;;;;;;; socket

(defvar memetalk-repl-recon-timer nil)

(defun memetalk-repl-setup-reconnection ()
  (unless memetalk-repl-recon-timer
    (setq memetalk-repl-recon-timer (run-at-time "1 second" 1 'memetalk-repl-connect))))

(defun memetalk-repl-detect-disconnect (process event)
  (message (format "memetalk repl: %s %s" process event))
  (memetalk-repl-setup-reconnection))

(defun memetalk-repl-connect ()
  (interactive)
  (setq mm-repl-socket nil)
  (condition-case nil
      (progn
        (setq mm-repl-socket (open-network-stream "mm-repl"
                                                  "*mm-repl*" "localhost" 4200))
        (set-process-sentinel mm-repl-socket 'memetalk-repl-detect-disconnect)
        (cancel-timer memetalk-repl-recon-timer)
        (setq memetalk-repl-recon-timer nil)
        (if memetalk-current-module
            (progn
              (memetalk-repl-send (concat "load " memetalk-current-module "\n"))
              (message (format "memetalk repl connected and %s loaded" memetalk-current-module)))
          (message "memetalk repl connected")))
    (error nil))
  mm-repl-socket)

(defun memetalk-repl-is-open ()
  (and (process-status "mm-repl") t))

(defun memetalk-trim-r (string)
  (when (string-match "[ \t\n]*$" string)
    (replace-match "" nil nil string)))

(defun memetalk-repl-last-response ()
  (if (memetalk-repl-is-open)
      (with-current-buffer "*mm-repl*"
        (memetalk-trim-r (thing-at-point 'line)))
    nil))

(defun memetalk-repl-send (cmd)
  (if (memetalk-repl-is-open)
      (progn
        (process-send-string "*mm-repl*" cmd)
        t)
    nil))


(defvar memetalk-current-module nil)

;;; operations

(defun memetalk-instantiate-module ()
  (interactive)
  (if (memetalk-repl-is-open)
      (let ((module-name (when (string-match ".mm" (buffer-name))
                           (replace-match "" nil nil (buffer-name)))))
        (setq memetalk-current-module module-name)
        (memetalk-repl-send (concat "load " module-name  "\n"))
        (message "memetalk repl: %s" (memetalk-repl-last-response)))))

(provide 'memetalk-mode)
(provide 'memetalk-instantiate-module)
