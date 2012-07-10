#lang typed/racket/base

(require (only-in racket/math pi))

(provide sinpx)

;; Computes x * sin(pi * x) in a way that keeps the argument to sin < 2*pi
(: sinpx (Float -> Float))
(define (sinpx z)
  (let ([z  (abs z)])
    (* z (sin (* (- z (* 2.0 (floor (* 0.5 z)))) pi)))))
