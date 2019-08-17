
;; Basics

(defun my-command()
  "One sentence summary of what this command do.

More details here. Be sure to mention the return value if relevant.
Lines here should not be longer than 70 chars,
and don't indent them."
  (interactive)
  (let (var1 var2)
    (setq var1 5)
    (setq var2 10)
    (message (format "%d" (+ var1 var2)))
    ;; do something
  )
)

;; Mark and Region

(defun ff-region()
  (interactive)
  (message "begin at %s and end at %s" (region-beginning) (region-end))
)

(kill-region)
(comment-region)
(indent-region)
(fill-region)

(setq mark-active t)

(defun my-is-region-active()
  "print whether region is active"
  (interactive)
  (if (use-region-p)
      (message "region active")
    (message "region not active")
  )
)

(defun my-select-line()
  "Select current line"
  (interactive)
  (let (p1 p2)
    (setq p1 (line-beginning-position))
    (setq p2 (line-end-position))
    (goto-char p1)
    (push-mark p2)
    (setq mark-active t)
  )
)

(defun ff-r (p1 p2)
  "print region begin/end"
  (interactive "r")
  (message "Region starts %d, ends %d" p1 p2)
)

(defun downcase-word-or-region()
  "Downcase current word or region"
  (interactive)
  (let (pos1 pos2 bds)
    (if (use-region-p)
        (setq pos1 (region-beginning) pos2 (region-end))
      (progn
        (setq bds (bounds-of-thing-at-point 'symbol))
        (setq pos1 (car bds) pos2 (cdr bds))
      )
    )
    (downcase-region pos1 pos2)
  )
)

;; Functions on line

(line-move-visual 3)

(move-beginning-of-line nil)
(move-end-of-line nil) ;; etsetoe

(beginning-of-line)
(end-of-line) ;; test

(line-beginning-position)
(line-end-position)

(forward-line)
(forward-line 1)
(forward-line -1)

(next-line)
(previous-line)

(setq myLine (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
(message myLine)


