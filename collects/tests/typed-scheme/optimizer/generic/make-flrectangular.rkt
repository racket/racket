(module make-flrectangular typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (make-rectangular 1.0 2.2)
  (make-flrectangular 1.0 2.2))
