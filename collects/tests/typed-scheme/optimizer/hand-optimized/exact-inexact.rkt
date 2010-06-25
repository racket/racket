(module exact-inexact typed/scheme #:optimize
  (require racket/flonum)
  (->fl (expt 10 100))) ; must not be a fixnum
