
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
