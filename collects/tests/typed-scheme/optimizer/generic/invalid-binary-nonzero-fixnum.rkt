#lang typed/scheme
#:optimize
(: f ( -> Void))
(define (f) ; in a function, to prevent evaluation
  (display (quotient 4 0))) ; 2 fixnums, but the second is 0, cannot optimize
