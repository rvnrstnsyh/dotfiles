;;; ui.el --- User interface configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures the Emacs user interface, including cursor behavior,
;; themes, completion frameworks, and UI elements.

;;; Code:

;; Window.
(set-fringe-style '(0 . 0))       ;; Fringe (Left 0px . Right 0px).

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

;; Git Gutter default sign and color.
(setq git-gutter:added-sign    " +")
(setq git-gutter:modified-sign " ~")
(setq git-gutter:deleted-sign  " -")

(custom-set-faces
 '(git-gutter:added    ((t (:weight bold :foreground "#ADFF2F"))))   ;; Added/new line.
 '(git-gutter:modified ((t (:weight bold :foreground "#FFFF00"))))   ;; Modified/edited line.
 '(git-gutter:deleted  ((t (:weight bold :foreground "#FF0000")))))  ;; Deleted/removed line.

(provide 'ui)

;;; ui.el ends here
