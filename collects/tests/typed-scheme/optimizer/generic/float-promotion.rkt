(module float-promotion typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (+ 1 2.0))
