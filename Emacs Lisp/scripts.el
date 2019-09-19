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

(setq inputDir "/Users/xah/web/wordyenglish_com/arabian_nights/" ) ; dir should end with a slash

;; need sgml-skip-tag-forward
(require 'sgml-mode)

(defun my-process-file (fPath)
  "Process the file at FPATH …"
  (let (myBuffer
        p3 p4  (bulletCount 0) )

    ;; (print fPath)

    (when
        (and (not (string-match "/xx" fPath))) ; skip some dir

      (setq myBuffer (get-buffer-create " myTemp"))
      (set-buffer myBuffer)
      (insert-file-contents fPath nil nil nil t)

      (setq bulletCount 0 )
      (goto-char 1)
      (while
          (search-forward "<div class=\"xnote\">"  nil t)

        (setq p3 (point)) ; beginning of innerText, after <div class="xnote">
        (backward-char)
        (sgml-skip-tag-forward 1)
        (backward-char 6)
        (setq p4 (point)) ; end of innerText, before </div>

        (setq bulletCount (count-matches "•" p3 p4))

        (when (> bulletCount 1)
          (princ (format "Found: %d %s\n" bulletCount fPath))))

      (kill-buffer myBuffer))))

(let (outputBuffer)
  (setq outputBuffer "*my output*" )
  (with-output-to-temp-buffer outputBuffer
    (mapc 'my-process-file
          (directory-files-recursively inputDir "\.html$" ))
    (princ "Done")))
