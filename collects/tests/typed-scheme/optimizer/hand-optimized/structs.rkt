(module structs typed/scheme #:optimize
  (require racket/unsafe/ops)
  (define-struct: pt ((x : Integer) (y : Integer)) #:mutable)
  (define a (pt 3 4))
  (unsafe-struct-ref  a 0)
  (unsafe-struct-set! a 1 5))
