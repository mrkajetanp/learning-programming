
;; Emacs Commands with external scripts

(setq scriptPath "/usr/bin/wc")

(defun do-something-region (startPos endPos)
  (interactive "r")
  (let (cmdStr)
    (setq cmdStr scriptPath)
    (shell-command-on-region startPos endPos cmdStr nil t nil t)))

