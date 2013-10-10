#lang typed/racket/base

(require "../../../flonum.rkt")

(provide flbeta-appx-median flbeta-stddev)

(: flbeta-appx-median (Flonum Flonum -> Flonum))
;; Computes (a-1/3)/(a+b-2/3) in a way that avoids floating-point error
(define (flbeta-appx-median a b)
  (cond [(and (a . fl< . 1e20) (b . fl< . 1e20))
         (fl/ (fl+ (fl* 3.0 a) -1.0)
              (fl+ (fl+ (fl* 3.0 b) (fl* 3.0 a)) -2.0))]
        [else
         (fl/ a (fl+ a b))]))

(: flbeta-stddev (Flonum Flonum -> Flonum))
;; Computes a*b/((a+b)^2*(a+b+1)) in a way that avoids overflow
(define (flbeta-stddev a b)
  (define n (fl+ a b))
  (cond [(n . fl> . 1e40)
         (let ([a  (fl* 0.5 a)] [b  (fl* 0.5 b)])
           (define n (fl+ a b))
           (fl/ (fl* (flsqrt (fl/ a n)) (flsqrt (fl/ b n)))
                (fl* (flsqrt 2.0) (flsqrt n))))]
        [else
         (fl/ (fl* (flsqrt (fl/ a n)) (flsqrt (fl/ b n)))
              (flsqrt (fl+ n 1.0)))]))
