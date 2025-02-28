;;; core.el --- Core Emacs settings -*- lexical-binding: t; -*-

;;; Commentary:
;; This module contains core Emacs settings, including path configuration,
;; default encoding, miscellaneous options, and essential packages.

;;; Code:

(require 'cl-lib)

;; Path Configuration - Platform specific.
(cond
 ((eq system-type 'windows-nt)
  ;; Windows path configuration - Compatible with CMD commands.
  (let ((sys-path (getenv "PATH")))
    ;; Windows already has system paths in PATH env var,
    ;; we can add additional paths if needed.
    (when (file-exists-p (convert-standard-path "C:/Windows/System32"))
      (cl-pushnew (convert-standard-path "C:/Windows/System32") exec-path :test 'equal))
    ;; Optional: Add other Windows specific paths if needed.
    ;; This one is for Git.
    (when (file-exists-p (convert-standard-path "C:/Program Files/Git/cmd"))
      (setenv "PATH" (concat sys-path ";" (convert-standard-path "C:/Program Files/Git/cmd")))
      (cl-pushnew (convert-standard-path "C:/Program Files/Git/cmd") exec-path :test 'equal))))
 (t
  ;; Unix/Linux/macOS path configuration.
  (setenv "PATH" (concat (getenv "PATH") ":" (convert-standard-path "/usr/local/bin")))
  (cl-pushnew (convert-standard-path "/usr/local/bin") exec-path :test 'equal)))

;; Default coding system.
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-selection-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
;; Default read and write coding system.
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)
(setq file-name-coding-system 'utf-8-unix)
;; Default scratch message.
(setq initial-scratch-message ";; -*- mode: emacs-lisp; lexical-binding: t -*-\n")

;; Miscellaneous Settings.
(setq-default
 inhibit-splash-screen t                              ;; Disable splash screen.
 make-backup-files nil                                ;; Disable backup file creation.
 tab-width 4                                          ;; Set default tab width to 4 spaces.
 indent-tabs-mode nil                                 ;; Use spaces instead of tabs.
 compilation-scroll-output t                          ;; Automatically scroll compilation output.
 visible-bell (eq system-type 'windows-nt))           ;; Use visible bell only on Windows.

(setq confirm-kill-emacs                              ;; Confirm before exiting Emacs.
  (lambda (_)
    (yes-or-no-p "You need some rest, leave now?")))

;; General Features.
(show-paren-mode 1)                                   ;; Highlight matching parentheses.
(global-font-lock-mode 1)                             ;; Enable syntax highlighting.

;; Packages.
(use-package company
  :ensure t
  :init (global-company-mode))                        ;; Enable completion everywhere.

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))            ;; Use company-box for better UI.

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))                       ;; Enable Flycheck globally for real-time linting.

;; Git Gutter configurations.
(global-git-gutter-mode t)
(setq git-gutter:update-interval 5)

(provide 'core)

;;; core.el ends here
