#;#;
#<<END
TR opt: sqrt.rkt 12:2 (sqrt x) -- unary float
END
""

#lang typed/scheme
#:optimize

(: f (Nonnegative-Float -> Nonnegative-Float))
(define (f x)
  (sqrt x))
