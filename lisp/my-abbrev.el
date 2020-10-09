(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ("prots@" "ports@")

    ("supprots" "supports")

    ("het" "the")
    ("teh" "the")

    ("wehn" "when")
    ))

(when (boundp 'clojure-mode-abbrev-table)
  (clear-abbrev-table clojure-mode-abbrev-table))

(define-abbrev-table 'clojure-mode-abbrev-table
  '(("erq" "req")))

(when (boundp 'c-mode-abbrev-table)
  (clear-abbrev-table c-mode-abbrev-table))

(define-abbrev-table 'c-mode-abbrev-table
  '(("inculde" "include")))

;; turn on abbrev mode globally
(setq-default abbrev-mode t)
