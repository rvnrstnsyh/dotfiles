;; ~/.emacs.d/modules/core.el
;; Core Emacs settings

;; Path Configuration - Platform specific
(cond
 ((eq system-type 'windows-nt)
  ;; Windows path configuration - Compatible with CMD commands
  (let ((system-paths (getenv "PATH")))
    ;; Windows already has system paths in PATH env var
    ;; We can add additional paths if needed
    (when (file-exists-p "C:/Windows/System32")
      (setq exec-path (append exec-path (list "C:/Windows/System32"))))
    ;; Optional: Add other Windows specific paths if needed
    ;; This one is for Git
    (when (file-exists-p "C:/Program Files/Git/cmd")
      (setenv "PATH" (concat (getenv "PATH") ";C:/Program Files/Git/cmd"))
      (setq exec-path (append exec-path (list "C:/Program Files/Git/cmd"))))))
 (t
  ;; Unix/Linux/macOS path configuration
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))))

;; Function to convert between Windows and Unix paths
(defun convert-standard-path (path) "Convert PATH between Windows and Unix formats based on current system."
  (if (eq system-type 'windows-nt)
      (replace-regexp-in-string "/" "\\" path t t)
    (replace-regexp-in-string "\\\\" "/" path t t)))

;; Miscellaneous Settings
(setq-default
 inhibit-splash-screen t
 make-backup-files nil
 tab-width 4
 indent-tabs-mode nil
 compilation-scroll-output t
 visible-bell (equal system-type 'windows-nt))
(setq confirm-kill-emacs 'y-or-n-p)

;; General Features
(show-paren-mode 1)        ;; Highlight matching parentheses
(global-company-mode 1)    ;; Enable completion everywhere
(global-font-lock-mode 1)  ;; Enable syntax highlighting

(provide 'core)
