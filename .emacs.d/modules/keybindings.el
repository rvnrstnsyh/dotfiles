;;; keybindings.el --- Global keybindings configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module defines global keybindings for commonly used actions, including
;; Ivy, LSP, Counsel, and multiple cursors.

;;; Code:

(use-package ivy
  :ensure t
  :bind (("C-s" . swiper)))

(use-package lsp-mode
  :ensure t
  :bind
  (:map lsp-mode-map
        ("C-c l d" . lsp-describe-session)
        ("C-c l r" . lsp-restart-workspace)
        ("C-c l q" . lsp-shutdown-workspace)
        ("C-c l f" . lsp-format-buffer)
        ("C-c l a" . lsp-execute-code-action)))

(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c M-x" . execute-extended-command)))

;; Multiple Cursors Keybindings
(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(global-set-key [backtab] 'other-window)
(global-set-key (kbd "M-;") 're/toggle-comment)
(global-set-key (kbd "S-<return>") 're/newline-at-end)
(global-set-key (kbd "C-<return>") 're/newline-and-indent)
(global-set-key (kbd "C-S-<return>") 're/open-line-above)
(global-set-key (kbd "M-<up>") 're/move-line-up)
(global-set-key (kbd "M-<down>") 're/move-line-down)
(global-set-key (kbd "C-,") 're/duplicate-line-or-region)
(global-set-key (kbd "C-.") 're/duplicate-line-or-region-sticky-cursor)

(provide 'keybindings)

;;; keybindings.el ends here
