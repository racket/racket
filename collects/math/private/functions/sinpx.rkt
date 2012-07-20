#lang typed/racket/base

(require "../../constants.rkt"
         (only-in racket/math exact-floor))

(provide sinpx)

;; Computes x * sin(pi * x) in a way that keeps the argument to sin < 2*pi
#;;(: sinpx (Float -> Float))
(define (sinpx z)
  (let ([z  (abs z)])  ; even function
    (define x (- z (* 2.0 (floor (* 0.5 z)))))
    (* z (sin (* x pi.0)))))

(: sinpx (Float -> Float))
(define (sinpx z)
  (define sign 1.0)
  (cond [(z . < . 0.0)  (set! z (- z))]
        [else  (set! sign (- sign))])
  (define fl (exact-floor z))
  (define dist
    (cond [(odd? fl)  (set! fl (+ fl 1))
                      (set! sign (- sign))
                      (- fl z)]
          [else  (- z fl)]))
  (when (dist . > . 0.5)
    (set! dist (- 1.0 dist)))
  (* sign z (sin (* dist pi.0))))
