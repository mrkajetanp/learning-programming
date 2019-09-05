
;; Emacs Commands with external scripts

(setq scriptPath "/usr/bin/wc")

(defun do-something-region (startPos endPos)
  (interactive "r")
  (let (cmdStr)
    (setq cmdStr scriptPath)
    (shell-command-on-region startPos endPos cmdStr nil t nil t)))


;; Toggle Letter Case

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0)))))

(defun xah-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2015-12-22"
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

testing okay okay testing

(defun xah-title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current text block or selection.

URL `http://ergoemacs.org/emacs/emacs_upcase_sentence.html'
Version 2019-06-21"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq $p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward "\\. \\{1,2\\}\\([a-z]\\)" nil "move") ; after period
            (upcase-region (match-beginning 1) (match-end 1))
            ;; (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face '((t :background "red" :foreground "white")))
            (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face 'highlight))

          ;;  new line after period
          (goto-char (point-min))
          (while (re-search-forward "\\. ?\n *\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 1) (match-end 1))
            (overlay-put (make-overlay (match-beginning 1) (match-end 1)) 'face 'highlight))

          ;; after a blank line, after a bullet, or beginning of buffer
          (goto-char (point-min))
          (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))

          ;; for HTML. first letter after tag
          (goto-char (point-min))
          (while (re-search-forward "\\(<p>\n?\\|<li>\\|<td>\n?\\|<figcaption>\n?\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2))
            (overlay-put (make-overlay (match-beginning 2) (match-end 2)) 'face 'highlight))

          (goto-char (point-min)))))))


(defun xah-cycle-hyphen-underscore-space ( &optional @begin @end )
  "Cycle {underscore, space, hypen} chars in selection or inside quote/bracket or line.
When called repeatedly, this command cycles the {“_”, “-”, “ ”} characters, in that order.

The region to work on is by this order:
 ① if there's active region (text selection), use that.
 ② If cursor is string quote or any type of bracket, and is within current line, work on that region.
 ③ else, work on current line.

URL `http://ergoemacs.org/emacs/elisp_change_space-hyphen_underscore.html'
Version 2017-01-27"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of -charArray.
  (let ($p1 $p2)
    (if (and @begin @end)
        (progn (setq $p1 @begin $p2 @end))
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (if (nth 3 (syntax-ppss))
            (save-excursion
              (skip-chars-backward "^\"")
              (setq $p1 (point))
              (skip-chars-forward "^\"")
              (setq $p2 (point)))
          (let (
                ($skipChars
                 (if (boundp 'xah-brackets)
                     (concat "^\"" xah-brackets)
                   "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕（）")))
            (skip-chars-backward $skipChars (line-beginning-position))
            (setq $p1 (point))
            (skip-chars-forward $skipChars (line-end-position))
            (setq $p2 (point))
            (set-mark $p1)))))
    (let* (
           ($charArray ["_" "-" " "])
           ($length (length $charArray))
           ($regionWasActive-p (region-active-p))
           ($nowState
            (if (equal last-command this-command )
                (get 'xah-cycle-hyphen-underscore-space 'state)
              0 ))
           ($changeTo (elt $charArray $nowState)))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (goto-char (point-min))
          (while
              (re-search-forward
               (elt $charArray (% (+ $nowState 2) $length))
               ;; (concat
               ;;  (elt -charArray (% (+ -nowState 1) -length))
               ;;  "\\|"
               ;;  (elt -charArray (% (+ -nowState 2) -length)))
               (point-max)
               "NOERROR")
            (replace-match $changeTo "FIXEDCASE" "LITERAL"))))
      (when (or (string= $changeTo " ") $regionWasActive-p)
        (goto-char $p2)
        (set-mark $p1)
        (setq deactivate-mark nil))
      (put 'xah-cycle-hyphen-underscore-space 'state (% (+ $nowState 1) $length)))))

