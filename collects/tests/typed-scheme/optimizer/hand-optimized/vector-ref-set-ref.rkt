(module vector-ref-set-ref typed/scheme #:optimize
  (require racket/unsafe/ops)
  (: x (Vector Integer String))
  (define x (vector 1 "1"))
  (unsafe-vector*-ref x 0)
  (unsafe-vector*-set! x 1 "2")
  (unsafe-vector*-ref x 1))
