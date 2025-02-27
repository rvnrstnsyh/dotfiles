;; ~/.emacs.d/modules/editing.el
;; Text editing enhancements.

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

(defun rc/move-line (direction) "Move the current line up or down based on DIRECTION ('up or 'down)."
  (let ((col (current-column)))
    (if (eq direction 'up)
        (progn (transpose-lines 1) (previous-line 2))
      (progn (next-line 1) (transpose-lines 1) (previous-line 1)))
    (move-to-column col)))

(defun rc/move-line-up () "Move the current line up."
  (interactive)
  (rc/move-line 'up))

(defun rc/move-line-down () "Move the current line down."
  (interactive)
  (rc/move-line 'down))

(defun rc/duplicate-line () "Duplicate current line and move cursor to the duplicated line."
  (interactive)
  (let ((column (current-column))
        (line (string-trim-right (thing-at-point 'line t))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-to-column column)))

(defun rc/duplicate-line-sticky-cursor () "Duplicate current line with sticky cursor."
  (interactive)
  (save-excursion
    (let ((column (current-column))
          (line (string-trim-right (thing-at-point 'line t))))
      (move-end-of-line 1)
      (push-mark)
      (newline)
      (insert line)
      (move-to-column column))))

(provide 'editing)
