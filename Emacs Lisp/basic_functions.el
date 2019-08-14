
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

;; String functions

(match-string 1 "testing ouay")
(split-string "okay_there_my_friend" "_")
(string-reverse "oh my god")
(string-blank-p "  ")
(string-empty-p "")
(string-trim "  test  ")
(string-trim-right "  test  ")
(string-trim-right "  test  ")
(string-remove-prefix "test" "testMe")

;; Buffer functions

(save-current-buffer
  (set-buffer "*scratch*")
  (insert "test")
)

(with-current-buffer "*scratch*"
  (insert "hello there")
)

(with-temp-buffer
  (insert "testing friend")
  (message "%s" (buffer-string))
)

(setq newBuf (generate-new-buffer "xyz"))
(save-current-buffer
  (set-buffer "xyz")
  ;; (insert "ohh")
  (message "%s" (buffer-string))
)


