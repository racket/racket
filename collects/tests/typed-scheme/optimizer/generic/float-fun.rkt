#lang typed/racket
#:optimize

(require racket/unsafe/ops)
(: f (Float -> Float))
(define (f x)
  (+ x 1.0))
