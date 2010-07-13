(module binary-nonzero-fixnum typed/scheme 
  (require racket/unsafe/ops)
  (quotient (vector-length '#(1 2 3)) 2))
