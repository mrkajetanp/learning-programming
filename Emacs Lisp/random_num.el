
(random t)

(defun xah-insert-random-number (NUM)
  "Insert NUM random digits.
NUM default to 5.
Call `universal-argument` before for different count."
  (interactive "P")
  (let (($charset "1234567890") ($baseCount 10))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount)))
    )
  )
)

(xah-insert-random-number 6)

(defun xah-insert-random-xeh (NUM)
  "Insert NUM random hex digits"
  (interactive "P")
  (let (($n (if (numberp NUM) (abs NUM) 5)))
    (insert (format (concat "%0" (number-to-string $n) "x") (random (1- (expt 16 $n)))))
  )
)

(format "oo: %d" (random 100))

(defun xah-insert-random-string (NUM)
  (interactive "P")
  (let* (($charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
         ($baseCount (length $charset)))
    (dotimes (_ (if (numberp NUM) (abs NUM) 5))
      (insert (elt $charset (random $baseCount)))
    )
  )
)

