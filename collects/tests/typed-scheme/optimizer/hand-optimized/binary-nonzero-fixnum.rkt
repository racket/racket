(module binary-nonzero-fixnum typed/scheme #:optimize
  (require racket/unsafe/ops)
  (unsafe-fxquotient (vector-length '#(1 2 3)) 2))
