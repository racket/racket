(module slldef mzscheme
  ;; A compile-time table shared by eopl and sllgen:
  (define sllgen-def (make-hash-table))
  (provide sllgen-def))
