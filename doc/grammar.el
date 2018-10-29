(setq grammar-highlights
      '(("\'.*\'" . font-lock-string-face)
	("[A-Z][A-Za-z]*" . font-lock-function-name-face)
	("[a-z][A-Za-z_]*" . font-lock-constant-face)
	))

(define-derived-mode grammar-mode fundamental-mode "grammar"
  "major mode for editing grammar files."
  (setq font-lock-defaults '(grammar-highlights)))
