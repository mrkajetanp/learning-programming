
(setq f '1+)
(setq f 'cos)
(setq f 'sqrt)

(setq myList '(1 2 3 4))
(mapcar '1+ myList)
(mapcar f myList)

(setq x1 122)
(symbolp x1)
(symbolp 'x1)

(symbol-name 'sin)
(symbol-value 'sin)
(symbol-function 'sin)
(symbol-plist 'sin)

(setq y "yes yes")
(symbol-value 'y)

(boundp 'h)
(boundp 'y)

(defun z() 4)
(symbol-function 'z)

(intern "x")
(intern-soft "x")

