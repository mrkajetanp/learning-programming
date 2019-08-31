
;; Date Time

(format-time-string "%Y-%m-%d")

(concat
 (format-time-string "%Y-%m-%dT%T")
 ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
  (format-time-string "%z")))

(format-time-string "%s")

(format-time-string "%B")
(format-time-string "%b")

(format-time-string "%A")
(format-time-string "%a")

(format-time-string "%Y-%j")

