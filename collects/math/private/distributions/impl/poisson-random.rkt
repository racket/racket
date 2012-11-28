#lang typed/racket/base

(require racket/fixnum
         "../../../flonum.rkt"
         "../../../base.rkt"
         "../../functions/log-gamma.rkt")

(provide flpoisson-sample)

(: flpoisson-sample-small (Flonum Natural -> FlVector))
;; Good for l < -log(+min.0); suffers from underflow otherwise
;; O(l) in time and the number of uniform random variates used
(define (flpoisson-sample-small l n)
  (define exp-l (flexp (- l)))
  (build-flvector
   n (λ (_)
       (let loop ([k 0.0] [p 1.0])
         (define u (random))
         (let ([p  (fl* p u)])
           (cond [(p . fl<= . exp-l)  k]
                 [else  (loop (fl+ k 1.0) p)]))))))

(: flpoisson-sample-atkinson (Flonum Natural -> FlVector))
;; For l < 5, converges so slowly it's not even worth considering; fast for l > 30 or so,
;; just as fast as flpoisson-random-small for l > 9
;; For l > 9, uses 5 random variates on average, which converges to 1 as l grows
(define (flpoisson-sample-atkinson l n)
  (define c (fl- 0.767 (fl/ 3.36 l)))
  (define beta (fl/ pi (flsqrt (fl* 3.0 l))))
  (define alpha (fl* beta l))
  (define k (fl- (fl- (fllog c) l) (fllog beta)))
  (define log-l (fllog l))
  (build-flvector
   n (λ (_)
       (let loop ()
         (define u (random))
         (define x (fl/ (fl- alpha (fllog (fl/ (fl- 1.0 u) u))) beta))
         (define n (flfloor (fl+ x 0.5)))
         (cond [(n . fl< . 0.0)  (loop)]
               [else
                (define v (random))
                (define y (fl- alpha (fl* beta x)))
                (define 1+exp-y (fl+ 1.0 (flexp y)))
                (define lhs (fl+ y (fllog (fl/ (fl/ v 1+exp-y) 1+exp-y))))
                (define rhs (fl- (fl+ k (fl* n log-l)) (fllog-gamma (fl+ n 1.0))))
                (cond [(lhs . fl<= . rhs)  n]
                      [else  (loop)])])))))

(: flpoisson-sample (Flonum Integer -> FlVector))
(define (flpoisson-sample l n)
  (cond [(n . < . 0)  (raise-argument-error 'flpoisson-sample "Natural" 1 l n)]
        [(l . fl< . 0.0)  (build-flvector n (λ (_) +nan.0))]
        [(l . fl= . 0.0)  (build-flvector n (λ (_) 0.0))]
        [(l . fl<= . 9.0)  (flpoisson-sample-small l n)]
        [(l . fl<= . 1e35)  (flpoisson-sample-atkinson l n)]
        [else
         ;; At this point, the flonums are so sparse that:
         ;;  1. The mean `l' must be an integer; it is therefore also the mode
         ;;  2. The only flonum integer with probability >= +min.0 is `l'
         (build-flvector n (λ (_) l))]))
