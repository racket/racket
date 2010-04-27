
(module math scheme/base
  (require scheme/math)
  (provide (all-from-out scheme/math)
           e)
  (define e (exp 1.0)))
