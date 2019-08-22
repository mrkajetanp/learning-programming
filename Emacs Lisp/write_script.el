
(message "argv 0: %s" (elt argv 0))
(message "argv 1: %s" (elt argv 1))
(message "argv 2: %s" (elt argv 2))
(message "argv 3: %s" (elt argv 3))

(with-temp-buffer
  (insert-file-contents "write_script.el")
  (message (buffer-string 1 100))
  )

;; Write File

(write-region (point-min) (point-max) "~/temp.el")
(append-to-file (point-min) (point-max) "filePath")


