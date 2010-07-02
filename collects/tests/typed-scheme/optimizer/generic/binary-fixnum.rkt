(module binary-fixnum typed/scheme #:optimize
  (require racket/unsafe/ops)
  (: f (All (X) ((Vectorof X) -> Natural)))
  (define (f v)
    (bitwise-and (vector-length v) 1)))
