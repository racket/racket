#;
(
float-fun.rkt 12:3 + -- binary float
)

#lang typed/racket
#:optimize


(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
