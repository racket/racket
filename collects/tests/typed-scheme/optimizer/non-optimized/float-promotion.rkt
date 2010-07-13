(module float-promotion typed/scheme 
  (require racket/unsafe/ops racket/flonum)
  (+ 1 2.0)
  (+ (expt 100 100) 2.0))
