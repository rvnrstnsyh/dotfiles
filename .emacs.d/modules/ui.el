;; ~/.emacs.d/modules/ui.el
;; User interface configuration.

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
