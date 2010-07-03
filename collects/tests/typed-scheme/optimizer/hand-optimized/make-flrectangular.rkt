(module make-flrectangular typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (unsafe-make-flrectangular 1.0 2.2)
  (unsafe-make-flrectangular 1.0 2.2))
