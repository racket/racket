(module unary-fixnum typed/scheme #:optimize
  (require racket/unsafe/ops)
  (bitwise-not 4))
