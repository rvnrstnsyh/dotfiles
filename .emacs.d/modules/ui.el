;; ~/.emacs.d/modules/ui.el
;; User interface configuration.

;; Default cursor.
(blink-cursor-mode 1)             ;; Enable blinking cursor.
(setq blink-cursor-blinks 0)      ;; 0 means blink indefinitely.
(setq blink-cursor-delay 0.75)    ;; Use the default delay (0.75 seconds).
(setq blink-cursor-interval 0.5)  ;; Set blink interval to 0.5 seconds.
(set-default 'cursor-type 'bar)
(add-hook 'after-init-hook (lambda () (set-cursor-color "#ADFF2F")))

;; Load and configure theme.
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night t))

;; Enable Ivy and Counsel for better completion.
(use-package ivy
  :ensure t
  :init (ivy-mode 1))

(use-package counsel
  :ensure t
  :after ivy
  :init (counsel-mode 1))

;; Disable unnecessary UI elements.
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode 0)))

;; Enable line numbers for Emacs >= 26.
(when (>= emacs-major-version 26) (global-display-line-numbers-mode))

(provide 'ui)
