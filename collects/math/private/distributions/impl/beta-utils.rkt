#lang typed/racket/base

(require "../../../flonum.rkt")

(provide flbeta-appx-median flbeta-stddev)

(: flbeta-appx-median (Flonum Flonum -> Flonum))
;; Computes (a-1/3)/(a+b-2/3) in a way that avoids floating-point error
(define (flbeta-appx-median a b)
  (cond [(and (a . < . 1e20) (b . < . 1e20))
         (/ (+ (* 3.0 a) -1.0)
            (+ (* 3.0 b) (* 3.0 a) -2.0))]
        [else
         (/ a (+ a b))]))

(: flbeta-stddev (Flonum Flonum -> Flonum))
;; Computes a*b/((a+b)^2*(a+b+1)) in a way that avoids overflow
(define (flbeta-stddev a b)
  (define n (+ a b))
  (cond [(n . > . 1e40)
         (let ([a  (* 0.5 a)] [b  (* 0.5 b)])
           (define n (+ a b))
           (/ (* (flsqrt (/ a n)) (flsqrt (/ b n)))
              (* (flsqrt 2.0) (flsqrt n))))]
        [else
         (/ (* (flsqrt (/ a n)) (flsqrt (/ b n)))
            (flsqrt (+ n 1.0)))]))
