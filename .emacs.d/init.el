;;; init.el --- Main configuration file -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the main Emacs configuration file that loads all modules.

;;; Code:

(defconst emacs-start-time (current-time))

;; Set custom file
(setq custom-file "~/.emacs.d/custom.el")

;; Miscellaneous modules.
;; Load this first to ensure custom functions for other modules are available.
(add-to-list 'load-path (expand-file-name "modules/misc" user-emacs-directory))
(require 'behavior)
(require 'editing)

;; Core modules.
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'packages)  ;; High priority.
(require 'core)
(require 'ui)
(require 'keybindings)

;; Development modules (environments).
(add-to-list 'load-path (expand-file-name "modules/dev" user-emacs-directory))
(require 'common)    ;; Common development settings.
(require 'lsp)       ;; General LSP configuration for multiple programming languages.

;; Load Custom File if it exists.
(when (file-exists-p custom-file) (load-file custom-file))

(let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
  (message "Modules loaded in %.3fs" elapsed))

;;; init.el ends here
