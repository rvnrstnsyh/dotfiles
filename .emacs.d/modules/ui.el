;; ~/.emacs.d/modules/ui.el
;; User interface configuration

;; Load Theme
(require 'doom-themes)
(load-theme 'doom-tokyo-night t)

;; UI Tweaks
(menu-bar-mode 0)    ;; Disable menu bar
(tool-bar-mode 0)    ;; Disable tool bar
(scroll-bar-mode 0)  ;; Disable scroll bar

;; Enable line numbers for Emacs 26+
(when (version<= "26.0.50" emacs-version)
  (global-display-line-numbers-mode))

;; Enable Ivy for enhanced completion
(ivy-mode 1)
(counsel-mode 1)

(provide 'ui)
