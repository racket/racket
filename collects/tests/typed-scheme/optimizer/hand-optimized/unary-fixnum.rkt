(module unary-fixnum typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fxnot 4))
