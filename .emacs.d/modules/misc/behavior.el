;;; behavior.el --- Various Emacs behaviors -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides miscellaneous behavior-related functions for Emacs.

;;; Code:

;; Function to convert between Windows and Unix paths.
(defun convert-standard-path (path) "Convert PATH between Windows and Unix formats based on the current system."
  (if (eq system-type 'windows-nt)
      (replace-regexp-in-string "/" "\\\\" path)  ;; Unix to Windows.
    (replace-regexp-in-string "\\\\" "/" path)))  ;; Windows to Unix.

(provide 'behavior)

;;; behavior.el ends here
