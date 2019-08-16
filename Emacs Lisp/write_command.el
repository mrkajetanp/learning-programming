
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


