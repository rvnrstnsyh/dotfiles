;;; packages.el --- Package management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; This module handles package management, including setting up repositories,
;; ensuring required packages are installed, and managing package priorities.

;;; Code:

(require 'package)

;; Add package archives.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
;; Initialize package system.
(package-initialize)

;; Package Archive Priorities.
(setq package-archive-priorities
      '(("melpa" . 10)  ;; Highest priority for MELPA.
        ("elpa" . 1)))  ;; Lowest priority for ELPA.

;; Refresh package list only when necessary.
(defvar rc/package-contents-refreshed nil)

(defun rc/package-refresh-contents-once () "Refresh package contents once per session."
  (unless rc/package-contents-refreshed
    (setq rc/package-contents-refreshed t)
    (package-refresh-contents)))

(defun rc/require-one-package (package) "Ensure PACKAGE is installed and refresh if necessary."
  (when (not (package-installed-p package))
    (rc/package-refresh-contents-once)
    (package-install package)))

(defun rc/require (&rest packages) "Ensure all PACKAGES are installed."
  (cl-loop for package in packages do (rc/require-one-package package)))

;; Automatically install essential packages.
(rc/require
 'company
 'company-box
 'counsel
 'dash
 'dockerfile-mode
 'doom-themes
 'flycheck
 'ivy
 'js2-mode
 'less-css-mode
 'lsp-mode
 'lsp-ui
 'markdown-mode
 'multiple-cursors
 'nginx-mode
 'php-mode
 'swiper
 'typescript-mode
 'web-mode
 'which-key
 'yaml-mode)

(provide 'packages)

;;; packages.el ends here
