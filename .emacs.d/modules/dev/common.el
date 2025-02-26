;; ~/.emacs.d/modules/dev/common.el
;; Common development settings

;; File associations for various modes
(add-to-list 'auto-mode-alist '("\\.[cm]?js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.[cm]?ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx?$" . web-mode))

;; Eglot configuration
(require 'eglot)

(provide 'common)
