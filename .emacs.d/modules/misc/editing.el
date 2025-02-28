;;; editing.el --- Text editing enhancements -*- lexical-binding: t; -*-

;;; Commentary:
;; This module provides enhancements for text editing, including functions
;; for inserting newlines, moving lines, and duplicating lines.

;;; Code:

(defun rc/newline-at-end () "Move to end of line and insert a newline."
  (interactive)
  (end-of-line)
  (newline))

(defun rc/newline-and-indent () "Move to end of line, insert a newline, and indent."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun rc/open-line-above () "Open a new line above the current line and indent."
  (interactive)
  (beginning-of-line)
  (open-line 1)
  (indent-for-tab-command))

(defun rc/move-line (direction) "Move the current line up or down based on DIRECTION (\='up or \='down)."
  (let ((col (current-column)))
    (if (eq direction 'up)
        (progn (transpose-lines 1) (forward-line -2))
      (progn (forward-line 1) (transpose-lines 1) (forward-line -1)))
    (move-to-column col)))

(defun rc/move-line-up () "Move the current line up."
  (interactive)
  (rc/move-line 'up))

(defun rc/move-line-down () "Move the current line down."
  (interactive)
  (rc/move-line 'down))

(defun rc/duplicate-line-or-region () "Duplicate the current line or the selected region."
  (interactive)
  (let ((column (current-column)))
    (if (use-region-p)
        (let* ((beg (region-beginning))
               (end (region-end))
               (region-text (buffer-substring-no-properties beg end)))
          (goto-char end)
          (newline)
          (insert region-text))
      (let ((line (string-trim-right (thing-at-point 'line t))))
        (move-end-of-line 1)
        (newline)
        (insert line)))
    (move-to-column column)))

(defun rc/duplicate-line-or-region-sticky-cursor () "Duplicate the current line or selected region while keeping the cursor position."
  (interactive)
  (let ((column (current-column)))
    (save-excursion
      (if (use-region-p)
          (let* ((beg (region-beginning))
                 (end (region-end))
                 (region-text (buffer-substring-no-properties beg end)))
            (goto-char end)
            (newline)
            (insert region-text))
        (let ((line (string-trim-right (thing-at-point 'line t))))
          (move-end-of-line 1)
          (newline)
          (insert line))))
    (move-to-column column)))

(provide 'editing)

;;; editing.el ends here
