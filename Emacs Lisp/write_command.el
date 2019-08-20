
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

;; Cut Copy Paste to/from kill-ring

(line-beginning-position)
(line-end-position)

(kill-region 1869 1914)
(copy-region-as-kill 1869 1914)

(kill-new "cute cat")
(kill-append " hello there " t)

(yank)

(defun my-select-text-in-quote()
  "Select text between the nearest left and right quotes."
  (interactive)
  (let ($pos ($skipChars "^\""))
    (skip-chars-backward $skipChars)
    (setq $pos (point))
    (skip-chars-forward $skipChars)
    (push-mark $pos)
    (setq mark-active t)
  )
)

;; Get User Input

(read-string "Input name: ")
(read-file-name "Input file name: ")
(read-directory-name "Directory: ")
(read-regexp "Regexp: ")

(defun ff-file-name()
  (interactive)
  (message "String is %s" (read-file-name "Enter file name: "))
)

(defun ff-string()
  (interactive)
  (message "String is %s" (read-string "Enter string: "))
)

(require 'ido)

(defun my-pick-one()
  "Prompt user to pick a choice from the list"
  (interactive)
  (let ((choices '("cat" "dog" "dragon" "tiger")))
    (message "%s" (ido-completing-read "Open bookmark:" choices))
  )
)

(if (y-or-n-p "Do it?")
    (progn
      (message "Chosen yes")
    )
  (progn
    (message "Chosen no")
  )
)

;; Interactive Form

(defun ask-name (x)
  (interactive "sEnter your name: ")
  (message "Name: %s" x)
)

(defun ask-age (x)
  (interactive "nEnter your age: ")
  (message "Age: %d" x)
)

(defun do-something (x y)
  (interactive (list "Mary" 22))
  (message "Name is: %s, age is: %d" x y)
)

(defun ask-name-and-age (x y)
  "Ask name and age"
  (interactive "sEnter your name: 
nEnter your age: ")
  (message "Name is: %s, Age is: %d" x y)
)

;; Get universal-argument

(defun univ-1 (x)
  (interactive "P")
  (message "%s" x)
  ;; value of x is from universal argument
)

(defun univ-2 (x)
  (interactive "p")
  (message "%s" x)
  ;; value of x is from universal argument
)

(defun guniv ()
  (interactive)
  (message "%s" current-prefix-arg)
)

(guniv)

(defun utest (arg1 &optional arg2 arg3)
  "Sample command to test `universal-argument'."
  (interactive
   (cond
    ((equal current-prefix-arg nil)
     (list 1 nil nil))
    ((equal current-prefix-arg '(4))
     (list 1 2 nil))
    ((equal current-prefix-arg 2)
     (list 1 2 3))
    ;; more special cases

    (t
     (list
      (read-string "arg1:")
      (read-string "arg2:")
      (read-string "arg3:" )))))

  ;; function body
  (message "args are: %s %s %s" arg1 arg2 arg3)
)

(utest 4)
(utest 4 5)
(utest 4 5 6)

;; Find Replace String in Buffer

;; string replacement in buffer

;; patrickx
;; mystr1x
;; myStr2

;; (let ((case-fold-search nil)) ;; case sensitive
(let ((case-fold-search t))
  (goto-char (point-min))
  (while (search-forward "myStr1" nil t)
    (replace-match "myReplaceStr1"))
  (goto-char (point-min))
  (while (search-forward "myStr2" nil t)
    (replace-match "myReplaceStr2"))
  )

(replace-match "TEST")
(match-string 2)

;; idiom for string replacement within a region
(save-restriction
  (narrow-to-region pos1 pos2)

  (goto-char (point-min))
  (while (search-forward "myStr1" nil t)
    (replace-match "myReplaceStr1"))

  ;; repeat for other string pairs
  )

;; Using thing-at-point

(defun print-word ()
  (interactive)
  (message "%s" (thing-at-point 'word))
  )

(defun get-boundary-and-thing ()
  (interactive)
  (let (bounds pos1 pos2 mything)
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (setq pos1 (car bounds))
    (setq pos2 (cdr bounds))
    (setq mything (buffer-substring-no-properties pos1 pos2))
    ;; (delete-region pos1 pos2)
    (message "begin: [%s], end: [%s], thing: [%s]" pos1 pos2 mything)
    )
  )

;; Get Dired Marked File List

(defun myProcessFile (s)
  (message "path: %s" s)
  )

(defun dired-my-process-file ()
  (interactive)
  (require 'dired)
  (mapc 'myProcessFile (dired-get-marked-files))
  )



