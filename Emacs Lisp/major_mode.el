
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

(defface highlight
  '((((class color) (min-colors 88) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 88) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 16) (background light))
     :background "darkseagreen2")
    (((class color) (min-colors 16) (background dark))
     :background "darkolivegreen")
    (((class color) (min-colors 8))
     :background "green" :foreground "black")
    (t :inverse-video t))
  "Basic face for highlighting."
  :group 'basic-faces)

;; Comment Colouring

;; sample mode that does coloring of python style commment syntax

(defvar xpy-mode-syntax-table nil "Syntax table for `xpy-mode'.")

(setq xpy-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; python style comment: “# …”
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

(define-derived-mode xpy-mode prog-mode "xpy"
  "xpy-mode is a major mode for editing language xpy."

  (setq font-lock-defaults (list nil nil))

  (set-syntax-table xpy-mode-syntax-table)
  ;; actually no need, because our syntax table name is “xpy-mode” + “-syntax-table”, so define-derived-mode will find it and set it
  )


;; Java Style Comments

(defvar xjv-mode-syntax-table nil "Syntax table for `xjv-mode'.")

(setq xjv-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
        ;; comment style “/* … */”
        (modify-syntax-entry ?\/ ". 14" synTable)
        (modify-syntax-entry ?* ". 23" synTable)
        synTable))

(define-derived-mode xjv-mode prog-mode "xjv"
  "xjv-mode is a major mode for editing language xjv."
  (setq font-lock-defaults (list nil)))

;; Comment Command

(define-derived-mode css-mode fundamental-mode "CSS"
  "Major mode to edit Cascading Style Sheets."

  (setq-local font-lock-defaults css-font-lock-defaults)

  (setq-local comment-start "/*")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "*/")
  (setq-local comment-end-skip "[ \t]*\\*+/")

  ;; ...

  )

;; Own Comment Command

(defun my-comment-dwim ()
  "Comment or uncomment the current line or text selection."
  (interactive)

  ;; If there's no text selection, comment or uncomment the line
  ;; depending whether the WHOLE line is a comment. If there is a text
  ;; selection, using the first line to determine whether to
  ;; comment/uncomment.
  (let (p1 p2)
    (if (use-region-p)
        (save-excursion
          (setq p1 (region-beginning) p2 (region-end))
          (goto-char p1)
          (if (wholeLineIsCmt-p)
              (my-uncomment-region p1 p2)
            (my-comment-region p1 p2)
            ))
      (progn
        (if (wholeLineIsCmt-p)
            (my-uncomment-current-line)
          (my-comment-current-line)
          )) )))

(defun wholeLineIsCmt-p ()
  (save-excursion
    (beginning-of-line 1)
    (looking-at "[ \t]*//")
    ))

(defun my-comment-current-line ()
  (interactive)
  (beginning-of-line 1)
  (insert "//")
  )

(defun my-uncomment-current-line ()
  "Remove “//” (if any) in the beginning of current line."
  (interactive)
  (when (wholeLineIsCmt-p)
    (beginning-of-line 1)
    (search-forward "//")
    (delete-backward-char 2)
    ))

(defun my-comment-region (p1 p2)
  "Add “//” to the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (my-comment-current-line)
        (previous-line)
        ))))

(defun my-uncomment-region (p1 p2)
  "Remove “//” (if any) in the beginning of each line of selected text."
  (interactive "r")
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char p2)
      (while (>= (point) p1)
        (my-uncomment-current-line)
        (previous-line) )) ))
