
(setq mymath-highlights
      '(("Sin\\|Cos\\|Sum" . font-lock-function-name-face)
        ("Pi\\|Infinity" . font-lock-constant-face)))

(define-derived-mode mymath-mode fundamental-mode "mymath"
  "major maths mode"
  (setq font-lock-defaults '(mymath-highlights)))

;; Sin[x]^2 + Cos[y]^2 == 1
;; Pi^2/6 == Sum[1/x^2,{x,1,Infinity}]

(setq mylsl-font-lock-keywords
      (let* (
            ;; define several category of keywords
            (x-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while"))
            (x-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
            (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
            (x-events '("at_rot_target" "at_target" "attach"))
            (x-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

            ;; generate regex string for each category of keywords
            (x-keywords-regexp (regexp-opt x-keywords 'words))
            (x-types-regexp (regexp-opt x-types 'words))
            (x-constants-regexp (regexp-opt x-constants 'words))
            (x-events-regexp (regexp-opt x-events 'words))
            (x-functions-regexp (regexp-opt x-functions 'words)))

        `(
          (,x-types-regexp . font-lock-type-face)
          (,x-constants-regexp . font-lock-constant-face)
          (,x-events-regexp . font-lock-builtin-face)
          (,x-functions-regexp . font-lock-function-name-face)
          (,x-keywords-regexp . font-lock-keyword-face)
          ;; note: order above matters, because once colored, that part won't change.
          ;; in general, put longer words first
          )))

;;;###autoload
(define-derived-mode mylsl-mode c-mode "lsl mode"
  "Major mode for editing LSL (Linden Scripting Language)…"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((mylsl-font-lock-keywords))))

;; add the mode to the `features' list
(provide 'mylsl-mode)


;; Font Lock Mode


(defvar myhtml-highlights nil "first element for `font-lock-defaults'")

(setq myhtml-highlights
      '(("<h1>\\|</h1>" . font-lock-function-name-face)
        ("<h1>\\([^<]+?\\)</h1>" . (1 font-lock-constant-face))))

(define-derived-mode myhtml-mode fundamental-mode "myhtml"
  "major mode for editing myhtml language code."
  (setq font-lock-defaults '(myhtml-highlights)))

;; Define Face

(defface my-lang-phi-word
  '((t :foreground "black"
       :background "aquamarine"
       :weight bold
       :underline t
       ))
  "Face for function parameters."
  :group 'my-lang-mode )

(defface my-lang-gamma-word
  '((t :foreground "red"
       :background "#f5f5f5"
       ))
  "Face for global variables."
  :group 'my-lang-mode )
