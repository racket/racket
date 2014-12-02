(module kernel-5 '#%kernel
  (#%require racket/private/map)
  (define-values (id) (λ (x) x))
  (define-values (xs) (list 1 2 3 4 5))
  (map id (map id xs)))
