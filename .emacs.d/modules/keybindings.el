;; ~/.emacs.d/modules/keybindings.el
;; Global keybindings configuration

;; Keybindings for Ivy and Counsel
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; Multiple Cursors Keybindings
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Editing Enhancement Keybindings
(global-set-key (kbd "S-<return>") (lambda () (interactive) (end-of-line) (newline)))
(global-set-key (kbd "C-<return>") (lambda () (interactive) (end-of-line) (newline-and-indent)))
(global-set-key (kbd "C-S-<return>") (lambda () (interactive) (beginning-of-line) (open-line 1) (indent-for-tab-command)))
(with-eval-after-load 'editing
  (global-set-key (kbd "M-<up>") 'rc/move-line-up)
  (global-set-key (kbd "M-<down>") 'rc/move-line-down)
  (global-set-key (kbd "C-,") 'rc/duplicate-line))

(provide 'keybindings)
