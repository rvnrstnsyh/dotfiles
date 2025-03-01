;;; lsp.el --- Global LSP configuration for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; This module configures LSP support for multiple programming languages in Emacs.
;; It includes general performance optimizations, UI enhancements, and automatic
;; detection for specific language servers (e.g., Deno for JavaScript/TypeScript).

;;; Code:

(require 'lsp-mode)

;; Install and configure lsp-mode.
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l") ;; Keybinding prefix for LSP.
  :config
  (setq lsp-enable-snippet t
        lsp-completion-enable t
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-headerline-breadcrumb-enable nil
        ;; Performance optimizations.
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        read-process-output-max (* 1024 1024))) ;; 1 MB.

;; LSP UI configuration.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil  ;; Instead, use documents on the sidelines.
        lsp-ui-doc-position 'top
        lsp-ui-doc-show-with-cursor t
        lsp-ui-doc-show-with-mouse t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5))

;; Add which-key integration for better discoverability of LSP commands.
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

;; Function to auto-detect Deno projects.
(defun re/use-deno-lsp () "Enable Deno LSP if a deno.json or deno.jsonc file is found."
  (when (or (locate-dominating-file default-directory "deno.json")
            (locate-dominating-file default-directory "deno.jsonc"))
    (setq-local lsp-server 'deno)
    (setq-local lsp-enabled-clients '(deno-ls))
    (lsp-deferred))) ;; Use lsp-deferred instead of lsp for better startup performance.

;; Register Deno Language Server.
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("deno" "lsp"))
  :major-modes '(js-mode js2-mode typescript-mode web-mode)
  :priority -1  ;; Lower priority than default.
  :server-id 'deno-ls
  :activation-fn (lambda (&rest _)
                   (or (locate-dominating-file default-directory "deno.json")
                       (locate-dominating-file default-directory "deno.jsonc")))
  :initialized-fn (lambda (workspace)
                    (with-lsp-workspace workspace
                      (lsp--set-configuration
                       '(:deno (:enable t :lint t :unstable t)))))))

;; Suppress Deno LSP warnings.
(setq warning-suppress-types '((lsp-mode)))

;; Enable built-in completion support for LSP.
;; LSP-mode now uses the built-in Emacs completion API.
(setq lsp-completion-provider :capf)

;; Automatically use Deno LSP for projects with `deno.json`.
(add-hook 'typescript-mode-hook #'re/use-deno-lsp)
(add-hook 'js-mode-hook #'re/use-deno-lsp)
(add-hook 'web-mode-hook #'re/use-deno-lsp)

;; Default TypeScript LSP for non-Deno projects.
(setq-default lsp-clients-typescript-server "typescript-language-server"
              lsp-clients-typescript-server-args '("--stdio"))

(provide 'lsp)

;;; lsp.el ends here
