;; Cx-Ce
;; Basics

(+ 1 4 3)

(message "hello buffer!")
(insert " ;; inserted text!") ;; inserted text!

;; Lists

(1 2 3)

(quote (1 2 3))

'(1 2 3) ;; same as function quote

(car '(1 2 3)) ;; returns the first item

(cdr '(1 2 3)) ;; returns the rest

(null nil) ;; checks if the argument is 
(null '())
(null '(1))
