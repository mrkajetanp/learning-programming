
(+ 1 2 3)

(cons 1 '(2 3))

(cons 1 (cons 2 (cons 3 '())))

(append '(1 2) '(3 4) '(5 6))

;; Variables

(set 'some-list '(1 2 3))

some-list

(setq my-list '(1 2 3 4)) ;; set quoted

my-list

(let ((a 1)
      (b 5))
  (format "a is %d and b is %d" a b))

;; Defining functions

(defun say-hello ()
  (message "hello !"))

(say-hello)

(defun square (x)
  (* x x))

(square 4)

(sqrt (square 4))
(sqrt 81)

;; Conditionals

(when (= (+ 2 2) 4)
  (message "sanity check passed!"))

(defun evens-or-odds (n)
  (if (= 0 (% n 2))
      "even!"
    "odd!"))

(evens-or-odds 4)
(evens-or-odds 3)

(defun pick-a-word (n)
  (cond
   ((= n 1) "one")
   ((= n 2) "two")
   ((= n 3) "three")
   (t "just true")
   ))

(pick-a-word 1)
(pick-a-word 2)
(pick-a-word 3)
(pick-a-word -7)

;; Recursion

(defun factorial (n)
  (if (< n 1)
      1
    (* n (factorial (- n 1)))))

(factorial 5)
(factorial 4)
(factorial 63) ;; biggest factorial you can evaluate

;; Anonymous functions

(lambda (x) (* x x x)) ;; just a lambda
((lambda (x) (* x x x)) 5) ;; lambda called with argument 5

(fset 'cube (lambda (x) (* x x x))) ;; bind a lambda to the name "cube"

(setq cube "foo")

(cube 5)
(cube 2)

;; lisp uses different namespace for functions and variables

cube

;; Higher-order functions

(mapcar 'upcase '("foo" "bar" "baz"))

(oddp 3)
(oddp 4)

(remove-if-not 'oddp
               '(1 2 3 4 5 6 7 8 9))

(remove-if 'oddp
               '(1 2 3 4 5 6 7 8 9))

;; Keybindings

(global-set-key (kbd "M-#") 'sort-lines)

foo
bar
baz

major-mode

(describe-key (kbd "M-#"))

(describe-function 'sort-lines)



