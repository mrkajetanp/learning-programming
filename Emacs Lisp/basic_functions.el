
;; Cursor position functions

(point)
(goto-char 20)
(backward-char 8)
(skip-chars-forward " )")

;; Text editing

(insert "test")
(delete-char 12)
(erase-buffer)

(insert (delete-and-extract-region 1 54))

;; Buffer string

(current-word)

(bounds-of-thing-at-point)


(defun my-select-inside-quotes()
  (interactive)
  (let (p1 p2)
    (search-backward "\"")
    (setq p1 (point))
    (skip-chars-forward "\"")
    (search-forward "\"")
    (setq p2 (point))

    (goto-char (- p2 1))
    (set-mark p1)
  )
)

"okay_then_my_friedn"



