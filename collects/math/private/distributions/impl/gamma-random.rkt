#lang typed/racket/base

#|
Generate Gamma samples using various algorithms

For 0 <= k < 1:

J H Ahrens and U Dieter.
Computer Methods for Sampling from Gamma, Beta, Poisson and Binomial Distributions.
Computing, 1974, vol 12, pp 223--246.

For 3 <= k < 1e10:

Pandu R Tadikamalla.
Computer Generation of Gamma Random Variables--II.
Communications of the ACM, May 1978, vol 21, issue 5, pp 419--422.

For others: sum of Gamma and Exponential variables, Normal approximation.
|#

(require "../../../flonum.rkt"
         "normal-random.rkt")

(provide standard-flgamma-random)

(: standard-flgamma-random-small (Float -> Float))
;; Ahrens and Dieter's rejection method
;; Good for 0.0 <= k < 1.0
(define (standard-flgamma-random-small k)
  (cond [(= k 0.0)  0.0]
        [else
         (define e (+ 1.0 (* k (exp -1.0))))
         (let loop ()
           (define p (* e (random)))
           (define q (fllog (random)))
           (cond [(p . >= . 1.0)
                  (define x (- (fllog (/ (- e p) k))))
                  (cond [(q . <= . (* (- k 1.0) (fllog x)))  x]
                        [else  (loop)])]
                 [else
                  (define x (flexpt p (/ 1.0 k)))
                  (cond [(q . <= . (- x))  x]
                        [else  (loop)])]))]))

(: standard-flgamma-random-1-2 (Float -> Float))
;; Sum of Gamma and Exponential rvs
;; Good for 1.0 <= k < 2.0
(define (standard-flgamma-random-1-2 k)
  (- (standard-flgamma-random-small (- k 1.0))
     (fllog (random))))

(: standard-flgamma-random-2-3 (Float -> Float))
;; Sum of Gamma and two Exponential rvs
;; Good for 2.0 <= k < 3.0
(define (standard-flgamma-random-2-3 k)
  (- (standard-flgamma-random-small (- k 2.0))
     (fllog (random))
     (fllog (random))))

(: standard-flgamma-random-large (Float -> Float))
;; Tadikamalla's rejection method (Laplacian candidate)
;; Good for 1.0 <= k < huge (where "huge" causes the floating-point ops to behave badly)
;; Faster than the other methods for large k when k >= 3 or so (Laplacian left tail generates too
;; many negative candidates, which are rejected, when k < 3)
(define (standard-flgamma-random-large k)
  (define A (- k 1.0))
  (define B (+ 0.5 (* 0.5 (flsqrt (- (* 4.0 k) 3.0)))))
  (define C (/ (* A (+ 1.0 B)) B))
  (define D (/ (- B 1.0) (* A B)))
  (let loop ()
    (define lx (max -max.0 (fllog (random))))
    (define x (+ A (* B (if ((random) . < . 0.5) (- lx) lx))))
    (cond [(x . < . 0.0)  (loop)]
          [((fllog (random)) . <= . (+ (* A (fllog (* D x))) (- x) (- lx) C))  x]
          [else  (loop)])))

(: standard-flgamma-random-huge (Float -> Float))
;; Normal approximation
;; Good for 1e10 <= k <= +inf.0
(define (standard-flgamma-random-huge k)
  (cond [(= k +inf.0)  +inf.0]
        [else  (max 0.0 (+ k (* (flsqrt k) (standard-flnormal-random))))]))

(: standard-flgamma-random (Float -> Float))
(define (standard-flgamma-random k)
  (cond [(k . >= . 1e10)  (standard-flgamma-random-huge k)]
        [(k . >= . 3.0)   (standard-flgamma-random-large k)]
        [(k . >= . 2.0)   (standard-flgamma-random-2-3 k)]
        [(k . >= . 1.0)   (standard-flgamma-random-1-2 k)]
        [(k . >= . 0.0)   (standard-flgamma-random-small k)]
        [else  +nan.0]))
