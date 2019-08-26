
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

