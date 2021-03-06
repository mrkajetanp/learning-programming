
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

;; Parsing Date Time

(require 'parse-time)

(equal
 (parse-time-string "Date: Mon, 01 Aug 2011 12:24:51 -0400")
 '(51 24 12 1 8 2011 1 nil -14400))

;; (SEC MIN HOUR DAY MON YEAR DOW DST TZ)

(equal
 (parse-time-string "2007, August 1")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "August 1, 2007")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "august 1, 2007")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "August 1st, 2007")
 '(nil nil nil nil 8 2007 nil nil nil))

(equal
 (parse-time-string "aug 1, 2007")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "1 aug, 2007")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "2007-08-01")
 '(nil nil nil 1 8 2007 nil nil nil))

(equal
 (parse-time-string "2007")
 '(nil nil nil nil nil 2007 nil nil nil))

(equal
 (parse-time-string "2007-08")
 '(nil nil nil nil nil nil nil nil nil))

(equal
 (parse-time-string "2011-08-01T11:55:37-07:00")
 '(nil nil nil nil nil nil nil nil nil))

;; Function Optional Parameters

(defun myfun (aa bb &optional cc dd)
  "test optional arguments"
  (insert aa bb cc dd)
  )

;; call it
(myfun "1" "2" "3" "4")

(defun ff (aa bb &rest cc)
  "test rest arguments"
  (message "%s" cc) ; cc is a list
  )

;; test
(ff "1" "2" "3" "4")

;; Regex

(re-search-forward
 "<img src=\"\\([^\"]+?\\)\" alt=\"\\([^\"]+?\\)\" width=\"\\([0-9]+\\)\" height=\"\\([0-9]+\\)\" />" )

;; Docstring Markup

(defun xlsl-mode ()
  "See URL `http://xahsl.org/sl/ls-emacs.html'."
  )

(defun xx ()
  "See Info node `(emacs) Dired'."
  )

(defun read-lines (file-path)
  "Return a list of lines of a file at FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file)
    (split-string (buffer-string) "\n" t)))

;; Unicode Escape Sequence

;; lower case “a”
(search-forward "\u0061" )

;; ♥ BLACK HEART SUIT codepoint 9829, #x2665
(search-forward "\u2665" )

;; 😸 GRINNING CAT FACE WITH SMILING EYES codepoint 128568, #x1f638
(search-forward "\U0001f638" )

;; ♥ 😸
