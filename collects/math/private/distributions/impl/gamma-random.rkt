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
  (cond [(fl= k 0.0)  0.0]
        [else
         (define e (fl+ 1.0 (fl* k (flexp -1.0))))
         (let loop ()
           (define p (fl* e (random)))
           (define q (fllog (random)))
           (cond [(p . fl>= . 1.0)
                  (define x (- (fllog (fl/ (fl- e p) k))))
                  (cond [(q . fl<= . (fl* (fl- k 1.0) (fllog x)))  x]
                        [else  (loop)])]
                 [else
                  (define x (flexpt p (fl/ 1.0 k)))
                  (cond [(q . fl<= . (- x))  x]
                        [else  (loop)])]))]))

(: standard-flgamma-random-1-2 (Float -> Float))
;; Sum of Gamma and Exponential rvs
;; Good for 1.0 <= k < 2.0
(define (standard-flgamma-random-1-2 k)
  (fl- (standard-flgamma-random-small (fl- k 1.0))
       (fllog (random))))

(: standard-flgamma-random-2-3 (Float -> Float))
;; Sum of Gamma and two Exponential rvs
;; Good for 2.0 <= k < 3.0
(define (standard-flgamma-random-2-3 k)
  (fl- (fl- (standard-flgamma-random-small (fl- k 2.0))
            (fllog (random)))
       (fllog (random))))

(: standard-flgamma-random-large (Float -> Float))
;; Tadikamalla's rejection method (Laplacian candidate)
;; Good for 1.0 <= k < huge (where "huge" causes the floating-point ops to behave badly)
;; Faster than the other methods for large k when k >= 3 or so (Laplacian left tail generates too
;; many negative candidates, which are rejected, when k < 3)
(define (standard-flgamma-random-large k)
  (define A (fl- k 1.0))
  (define B (fl+ 0.5 (fl* 0.5 (flsqrt (fl- (fl* 4.0 k) 3.0)))))
  (define C (fl/ (fl* A (fl+ 1.0 B)) B))
  (define D (fl/ (fl- B 1.0) (fl* A B)))
  (let loop ()
    (define lx (flmax -max.0 (fllog (random))))
    (define x (fl+ A (fl* B (if ((random) . fl< . 0.5) (- lx) lx))))
    (cond [(x . fl< . 0.0)  (loop)]
          [((fllog (random)) . fl<= . (fl+ (fl- (fl- (* A (fllog (* D x))) x) lx) C))  x]
          [else  (loop)])))

(: standard-flgamma-random-huge (Float -> Float))
;; Normal approximation
;; Good for 1e10 <= k <= +inf.0
(define (standard-flgamma-random-huge k)
  (cond [(fl= k +inf.0)  +inf.0]
        [else  (flmax 0.0 (fl+ k (fl* (flsqrt k) (standard-flnormal-random))))]))

(: standard-flgamma-random (Float -> Float))
(define (standard-flgamma-random k)
  (cond [(k . fl>= . 1e10)  (standard-flgamma-random-huge k)]
        [(k . fl>= . 3.0)   (standard-flgamma-random-large k)]
        [(k . fl>= . 2.0)   (standard-flgamma-random-2-3 k)]
        [(k . fl>= . 1.0)   (standard-flgamma-random-1-2 k)]
        [(k . fl>= . 0.0)   (standard-flgamma-random-small k)]
        [else  +nan.0]))
