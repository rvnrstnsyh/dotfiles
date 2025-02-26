;; ~/.emacs.d/modules/dev/typescript.el
;; TypeScript/JavaScript development configuration

(require 'tide)

;; Configure TypeScript/JavaScript development environment
(defun my/setup-deno-or-tide () "Setup either Eglot for Deno or Tide for TypeScript development."
  (when buffer-file-name
    (if (string-match-p "\\.[cm]?js$\\|\\.[cm]?ts$\\|\\.jsx?$\\|\\.tsx?$" buffer-file-name)
        (progn
          ;; If the TypeScript file is in a Deno project
          (if (or (locate-dominating-file buffer-file-name "deno.json")
                  (locate-dominating-file buffer-file-name "deno.jsonc"))
              (progn
                (setq-local eglot-workspace-configuration
                            '(:deno (:enable t :lint t :unstable t)))
                (eglot-ensure))
            ;; If not in a Deno project, use Tide
            (progn
              (tide-setup)
              (tide-hl-identifier-mode +1)
              (eldoc-mode +1)
              (setq tide-format-options '(:indentSize 2 :tabSize 2))
              (company-mode +1)
              (flycheck-mode +1)))))))

;; Add Deno language server to Eglot
(add-to-list 'eglot-server-programs '((js-mode js2-mode typescript-mode web-mode) . ("deno" "lsp")))

;; Hooks for TypeScript and JSX/TSX files
(add-hook 'typescript-mode-hook 'my/setup-deno-or-tide)
(add-hook 'js-mode-hook 'my/setup-deno-or-tide)
(add-hook 'web-mode-hook (lambda ()
                           (when (string-match-p "\\.tsx?$\\|\\.jsx?$" (buffer-file-name))
                             (my/setup-deno-or-tide))))

(provide 'typescript)
