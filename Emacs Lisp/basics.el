(require 'cl)  ; get lots of Common Lisp goodies

(concat "foo" "baz" "bar")
(string= "foo" "bar") ;; false
(substring "foobar" 0 3)
(upcase "foobar")

(if (> 3 2)
    (message "hello there"))

(if (>= 3 2)
    (message "hello there")
  (message "holy shit"))

(if (> 1 0)
    (progn
      (message "oh my god")
      (message "hey mate")
      (message "bloody hell")
      (message "it works")))

(unless (> 1 2)
  (message "there we are"))

(cond
 ((> 1 2)
  (message "test"))
 ((> 5 4)
  (message "oh god"))
)

(setq x 10
      total 0)

(while (plusp x)
  (incf total x)
  (decf x))

total ;; 55

(setq x 0 total 0)
(catch 'break
  (while t
    (incf total x)
    (if (> (incf x) 10)
        (throw 'break total))))

(setq x 0 total 0)
(while (< x 100)
  (catch 'continue
    (incf x)
    (if (zerop (% x 5))
        (throw 'continue nil))
    (incf total x)))

total

(setq x 0 total 0)
(catch 'break
  (while t
    (catch 'continue
      (incf x)
      (if (>= x 100)
          (throw 'break nil))
      (if (zerop (% x 5))
          (throw 'continue nil))
      (incf total x))))

total

(setq x 0)

(loop do
      (setq x (1+ x))
      while
      (< x 1))

x

(loop for i in '(1 2 3 4 5 6)
      collect (* i i))

(setq y 0)
(incf y 5)
(decf y)

y

(defun bar()
  (setq x 8))

(defun foo()
  (let ((x 6))
  (bar)
  x))

(foo)

(if (zerop 1)
    (message "zeroo")
  (message "non zero"))

(condition-case nil
    (message "case here")
  (error
   (message "error")))

(ignore-errors
  (message "ignoring errors in this code"))


(defstruct person
  "A person structure"
  name
  (age 0)
  (height 0.0))

(make-person)
(make-person :age 39)
(make-person :name "Steve" :height 5.83 :age 39)

(defstruct (employee
            (:include person))
  "An employee structure"
  company
  (level 1)
  (title "noob"))

(defconst pi 3.14 "Approximation of PI")
pi

(setq e (make-employee))
(setf (employee-name e) "Steve"
      (employee-age e) 39
      (employee-company e) "Google"
      (employee-title e) "Janitor")
e
