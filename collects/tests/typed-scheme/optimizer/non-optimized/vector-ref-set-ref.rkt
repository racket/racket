(module vector-ref-set-ref typed/scheme 
  (require racket/unsafe/ops)
  (: x (Vector Integer String))
  (define x (vector 1 "1"))
  (vector-ref x 0)
  (vector-set! x 1 "2")
  (vector-ref x 1))
