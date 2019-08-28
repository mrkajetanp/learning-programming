
;; List

(setq mylist (list 1 "b" 3))
(message "%s" mylist)

(setq mylist '(1 "b" 3))
(message "%s" mylist)

(let ((x 3) (y 4) (z 5))
  (message "%s" (list x y z))
  )

(make-list 5 1)

(number-sequence 5)
(number-sequence 2 5)
(number-sequence 0 9 3)
(number-sequence 5 0 -1)
(number-sequence 2.2 5.3)

(length '("a" "b" "c"))
(car '("a" "b" "c"))
(cdr '("a" "b" "c"))
(nth 1 '("a" "b" "c"))
(car (last '("a" "b" "c")))
(nthcdr 2 '("a" "b" "c" "d"))
(butlast '("a" "b" "c" "d"))
(butlast '("a" "b" "c" "d") 2)

(cons "a" '("b" "c"))
(cons "a" '("b" "c"))
(append '(1 2) '(3 4))

(setq mylist '(1 2 3 4))
(setcar mylist 8)
(setcdr mylist 8)
(pop mylist)
(message "%s" mylist)

(mapconcat 'number-to-string '(1 2 3 4) ",")
(mapconcat 'identity '("a" "b" "c") ",")

(substring (format "%s" '(1 "two" 3)) 1 -1)

;; Vector

(make-vector 5 0)
(vector 3 4 5)

(setq x 7)
(vector 1 2 x)
[3 n 5]
(setq aa [3 4 5])
(fillarray aa nil)
(length (vector 7 5 3))
(aref ["a" "b" "c"] 0)
(elt ["a" "b" "c"] 1)
(aset aa 0 "b")

(vconcat [3 4] ["a" "b"])
(vconcat [3 4] "ab")

(append [1 2 3] nil)
(append [1 2 3] [4 5])
(append [1 2 3] [4 5] nil)
(append [1 2 3] [4 5] '(6))

;; Sequence Functions

(number-sequence 1 8 2)
(vconcat (number-sequence 1 3) (number-sequence 3 1 -1))

(append (list 1 2 3) [3 2 1] nil)
(append (list 1 2 3) [3 2 1])
(cons 1 (list 4 5 6))

(sequencep 1)
(sequencep '(1))
(elt '(1 2 3 4 5) 1)
(reverse '(1 2 3))

(defun predicate-rising(a b)
  (< a b))

(defun double(a)
  (* a 2))

(sort '(6 3 8 7 2) 'predicate-rising)
(seq-map 'double '(2 3 4 5 6))

(defun positive(a)
  (> a 0))

(seq-filter 'positive '(2 3 -1 0 8 -4 5))
(member "4" '("3" "4" "5" "6"))

(setq xx '(3 4 5 3 2 3 5 1))
(sort (delete-dups xx) 'predicate-rising)

;; Map List/Vector

(1+ 2)
(mapcar '1+ '(1 2 3))
(mapcar 'car '((1 2) (3 4) (5 6)))

(mapcar
 (lambda (x) (elt x 1))
 [[1 2] [3 4] [5 6]])

(mapcar
 (lambda (x) (+ x 1))
 (list 1 2 3 4))

(mapc
 (lambda (x) (insert (number-to-string (aref x 0))))
 [[1 2] [3 4]])

(let ( (xlist (number-sequence 97 122)) )
  (dolist (n xlist) (insert n)))

(dotimes (i 4)
  (insert (number-to-string i)))

(let ((v [3 4 5 6 7]))
  (dotimes (i (length v))
    (insert
     (number-to-string
      (elt v i)))))

(let ((myList '(1 2 3 4)))
  (while myList
    (insert (number-to-string (pop myList)))))

(setq v [3 4 5])
(setq i 0)

(while (< i (length v))
  (insert (format "%d" (elt v i)))
  (setq i (1+ i)))

;; Exit Loop/Function, catch/throw

(defun test-exit-f ()
  (interactive)
  (catch 'aaa
    (if (y-or-n-p "exit?")
        (progn
          (message "existing")
          (throw 'aaa 3) ; if yes, exit right away, return 3 to catch
          )
      (progn ; else, go on
        (message "went on")
        4 ; return 4
        ))))

(defun test-exit-f2 ()
  (interactive)
  (if (y-or-n-p "invoke user-error to exit?")
      (user-error "Error, because: %s" "you said so!")
    (progn ; else go on
      (message "went on")
      )))

(setq myList [0 1 2 3 4 5])

(catch 'bbb
  (mapc
   (lambda (x)
     (insert (number-to-string x))
     (when (equal x 3) (throw 'bbb x)))
   myList)
  nil
  )

(while (and (not foundFlag-p) (<= i (length myList)))
  (when (equal (elt myList i) 3)
    (setq foundFlag-p t))

  (message "value: %s" i)
  (setq i (1+ i)))

;; Association List

(setq x
      '(("mary" . 23)
        ("john" . 24)
        ("james" . 26)
        ("smith" . 33)))

x

(assoc "john" x)
(car (assoc "john" x))
(cdr (assoc "john" x))

(rassoc 24 x)
(rassoc 27 x)

;; Property List

(plist-get '(x 1 y 2) 'y)
(plist-get '(x 1 y 2) 'x)
(plist-get '(x 1 y 2) 'b)

(setq xx '(a 1 b 2))
(setq xx (plist-put xx 'b 4))
(setq xx (plist-put xx 'c 3))

(plist-get xx 'b)
(plist-member xx 'b)

;; Symbol Property List

(put 'ff 'xx 5)
(get 'ff 'xx)
(symbol-plist 'ff)
(setplist 'ff '(a 1 b 2))
