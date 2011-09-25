(module math racket/base
  (require racket/math)
  (provide (all-from-out racket/math)
           e)
  (define e (exp 1.0)))
