
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

;; Print, Output

(setq xbuff (generate-new-buffer "*my output*"))
(print "something" xbuff)
(switch-to-buffer xbuff )
(princ '("x" "y")) ; (x y)
(prin1 '("x" "y")) ; ("x" "y")

(setq xbuff (generate-new-buffer "*my output*"))

(with-output-to-temp-buffer xbuff

  ;; this is inserted in current buffer
  (insert "xyz")

  ;; this is printed in buffer xbuff
  (print "abc"))

(switch-to-buffer xbuff )

;; String Format

(message (format "%04d-%02d-%02d" 2012 4 10))


