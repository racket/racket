(module float-promotion typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (unsafe-fl+ (->fl 1) 2.0))
