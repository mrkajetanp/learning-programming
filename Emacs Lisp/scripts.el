;; Grep

;; input dir
;; In elisp, dir path should end with a slash
(setq inputDir "/Users/xah/web/ergoemacs_org/" )

(setq findStr "stuff")

(defun my-process-file (fPath)
  "Process the file at FPATH "
  (let (myBuffer p1 p2 (ii 0) searchStr)
    (when (and (not (string-match "/xx" fPath))) ; exclude some dir

      (with-temp-buffer
        (insert-file-contents fPath nil nil nil t)

        (setq searchStr findStr  )

        (goto-char 1)
        (while (search-forward searchStr nil t)
          (setq ii (1+ ii)))

        ;; report if the occurrence is not n times
        (when (not (= ii 0))
          (princ (format "%d %s\n" ii fPath)))))))

;; walk the dir
(let (outputBuffer)
  (setq outputBuffer "*my find output*" )
  (with-output-to-temp-buffer outputBuffer
    (mapc 'my-process-file
          (directory-files-recursively inputDir "\.html$" ))
    (princ "Done")))
