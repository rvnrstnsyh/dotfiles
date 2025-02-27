;; ~/.emacs.d/modules/dev/common.el
;; Common development settings.

;; File associations for various modes.
(use-package js2-mode
  :ensure t
  :mode ("\\.[cm]?js$" . js2-mode))

(use-package typescript-mode
  :ensure t
  :mode ("\\.[cm]?ts$" . typescript-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.jsx?$" . web-mode)
  :mode ("\\.tsx?$" . web-mode))

(provide 'common)
