#;#;
#<<END
TR opt: float-fun.rkt 13:2 (+ x 1.0) -- binary float
END
""

#lang typed/racket
#:optimize


(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
