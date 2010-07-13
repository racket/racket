(module known-vector-length typed/scheme #:optimize
  (require racket/unsafe/ops)
  (+ 2 (begin (vector 1 2) 2)))
