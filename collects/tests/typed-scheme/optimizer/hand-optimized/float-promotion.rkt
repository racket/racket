(module float-promotion typed/scheme #:optimize
  (require racket/unsafe/ops racket/flonum)
  (unsafe-fl+ (unsafe-fx->fl (quotient 1 1)) 2.0)
  (unsafe-fl+ (->fl (expt 100 100)) 2.0))
