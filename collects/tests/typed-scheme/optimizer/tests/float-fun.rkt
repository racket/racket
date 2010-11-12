#;
(
float-fun.rkt line 12 col 3 - + - binary float
)

#lang typed/racket
#:optimize


(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
