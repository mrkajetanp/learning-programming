
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
