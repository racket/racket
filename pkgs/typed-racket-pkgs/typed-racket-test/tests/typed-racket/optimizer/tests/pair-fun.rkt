#;#;
#<<END
TR opt: pair-fun.rkt 14:6 (car x) -- pair
END
""

#lang typed/scheme
#:optimize

(: f ((Listof Integer) -> Integer))
(define (f x)
  (if (null? x)
      1
      (car x)))
