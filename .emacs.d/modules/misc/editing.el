;; ~/.emacs.d/modules/editing.el
;; Text editing enhancements

;; Move lines up/down
(defun rc/move-line-up () "Move the current line up."
  (interactive)
  (let ((col (current-column)))
    (transpose-lines 1)
    (previous-line 2)
    (move-to-column col)))

(defun rc/move-line-down () "Move the current line down."
  (interactive)
  (let ((col (current-column)))
    (next-line 1)
    (transpose-lines 1)
    (previous-line 1)
    (move-to-column col)))

;; Duplicate line
(defun rc/duplicate-line () "Duplicate current line."
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(provide 'editing)
