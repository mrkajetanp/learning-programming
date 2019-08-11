
(point)

(region-beginning)
(region-end)

(line-beginning-position)
(line-end-position)

;; some

(point-min)
(point-max)

(goto-char 39)
(forward-char 4)
(backward-char 20)

(re-search-backward "[0-9]")
(re-search-forward "[0-9]")

(skip-chars-backward "a-z")

;; 4

(search-backward "some")
(search-forward "some")

;; some

(delete-char 9)
(delete-region 3 20)

aoeueoauiaaouoeuaoeu7oaeueao

(insert "i love cats")

(message (buffer-substring 1 50))
(capitalize-region 1 50)

(length "abc")
(substring "abcdef" 2 4)
(replace-regexp-in-string "[0-9]" "X" "abc123")

(buffer-name)
(buffer-file-name)

(set-buffer "*scratch*")
(save-buffer)
(kill-buffer "*scratch*")

(with-current-buffer "*scratch*"
  (insert "testing testing 123"))

(find-file "~/.spacemacs")

(defun insert-p-tag()
  "Insert <p></p> at cursor point"
  (interactive)
  (insert "<p></p>")
  (backward-char 4))





