(module flvector-length typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (unsafe-flvector-length (flvector 0.0 1.2)))
