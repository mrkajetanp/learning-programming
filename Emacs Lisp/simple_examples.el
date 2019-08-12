
(defun wrap-markup-region(start end)
  "Insert a markup <b></b> around a region"
  (interactive "r")
  (save-excursion
    (goto-char end) (insert "</b>")
    (goto-char start) (insert "<b>")
  )
)

;; turn on highlight selection

(defun select-current-word()
  "Select the word under cursor."
  (transient-mark-mode 1)
  (interactive)
  (let (pt)
    (skip-chars-backward "-_A-Za-z0-9")
    (setq pt (point))
    (skip-chars-forward "-_A-Za-z0-9")
    (set-mark pt)
  )
)

(defun select-current-line()
  "Select the current line"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position))
)

(defun replace-greek-region (start end)
  "Replace 'alpha' to 'α' and other greek letters in current region"
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward " alpha" nil t) (replace-match " α" nil t))
    (goto-char (point-min))
    (while (search-forward " beta" nil t) (replace-match " β" nil t))
    (goto-char (point-min))
    (while (search-forward " gamma" nil t) (replace-match " γ" nil t))
  )
)

(defun delete-enclosed-text()
  "Delete texts between any pair of delimeters"
  (interactive)
  (save-excursion
    (let (p1 p2)
      (skip-chars-backward "^([<>“") (setq p1 (point))
      (skip-chars-forward "^)]<>“") (setq p2 (point))
      (delete-region p1 p2)
    )
  )
)

(random t)

(defun insert-random-number()
  "Insert a random number between 0 and 99999."
  (interactive)
  (insert (number-to-string (random 99999)))
)

(defun word-definition-lookup()
  "Look up the word under cursor in a browser"
  (interactive)
  (browse-url
   (concat "http://www.answers.com/main/ntquery?s=" (thing-at-point 'symbol))
  )
)

(defun to-unix-eol (fPath)
  "Change file's line ending to unix convention."
  (let ((myBuffer (find-file fPath)))
    (set-buffer-file-coding-system 'unix) ; or 'mac or 'dos
    (save-buffer)
    (kill-buffer myBuffer)
  )
)

(mapc 'to-unix-eol
      (list
       "~/myFile1"
       "~/myFile2"
       "~/myFile3"
      )
)

<testing bloody hell>
(oh mate)
[holy shit]

