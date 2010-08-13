(module float-promotion typed/scheme 
  (require racket/unsafe/ops racket/flonum)
  (+ (quotient 1 1) 2.0)
  (+ (expt 100 100) 2.0))
