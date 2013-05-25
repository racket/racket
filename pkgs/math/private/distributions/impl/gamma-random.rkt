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
         "../../unsafe.rkt"
         "normal-random.rkt")

(provide flgamma-sample)

(: flgamma-sample-small (Flonum Flonum Natural -> FlVector))
;; Ahrens and Dieter's rejection method
;; Good for 0.0 <= k < 1.0
(define (flgamma-sample-small k s n)
  (cond
    [(fl= k 0.0)  (make-flvector n 0.0)]
    [else
     (define e (fl+ 1.0 (fl* k (flexp -1.0))))
     (define k-1 (fl- k 1.0))
     (define 1/k (fl/ 1.0 k))
     (build-flvector
      n (λ (_)
          (let loop ()
            (define p (fl* e (random)))
            (define q (fllog (random)))
            (cond [(p . fl>= . 1.0)
                   (define x (- (fllog (fl/ (fl- e p) k))))
                   (cond [(q . fl<= . (fl* k-1 (fllog x)))  (fl* s x)]
                         [else  (loop)])]
                  [else
                   (define x (flexpt p 1/k))
                   (cond [(q . fl<= . (- x))  (fl* s x)]
                         [else  (loop)])]))))]))

(: flgamma-sample-1-2 (Flonum Flonum Natural -> FlVector))
;; Sum of Gamma and Exponential rvs
;; Good for 1.0 <= k < 2.0
(define (flgamma-sample-1-2 k s n)
  (define xs (flgamma-sample-small (fl- k 1.0) s n))
  (for ([i  (in-range n)])
    (define x (unsafe-flvector-ref xs i))
    (unsafe-flvector-set! xs i (fl- x (fl* s (fllog (random))))))
  xs)

(: flgamma-sample-2-3 (Flonum Flonum Natural -> FlVector))
;; Sum of Gamma and two Exponential rvs
;; Good for 2.0 <= k < 3.0
(define (flgamma-sample-2-3 k s n)
  (define xs (flgamma-sample-small (fl- k 2.0) s n))
  (for ([i  (in-range n)])
    (define x (unsafe-flvector-ref xs i))
    (unsafe-flvector-set! xs i (fl- x (fl* s (fl+ (fllog (random)) (fllog (random)))))))
  xs)

(: flgamma-sample-large (Flonum Flonum Natural -> FlVector))
;; Tadikamalla's rejection method (Laplacian candidate)
;; Good for 1.0 <= k < huge (where "huge" causes the floating-point ops to behave badly)
;; Faster than the other methods for large k when k >= 3 or so (Laplacian left tail generates too
;; many negative candidates, which are rejected, when k < 3)
(define (flgamma-sample-large k s n)
  (define A (fl- k 1.0))
  (define B (fl+ 0.5 (fl* 0.5 (flsqrt (fl- (fl* 4.0 k) 3.0)))))
  (define C (fl/ (fl* A (fl+ 1.0 B)) B))
  (define D (fl/ (fl- B 1.0) (fl* A B)))
  (build-flvector
   n (λ (_)
       (let loop ()
         (define lx (flmax -max.0 (fllog (random))))
         (define x (fl+ A (fl* B (if ((random) . fl< . 0.5) (- lx) lx))))
         (cond [(x . fl< . 0.0)
                (loop)]
               [((fllog (random)) . fl<= . (fl+ (fl- (fl- (fl* A (fllog (fl* D x))) x) lx) C))
                (fl* s x)]
               [else
                (loop)])))))

(: flgamma-sample-huge (Flonum Flonum Natural -> FlVector))
;; Normal approximation
;; Good for 1e10 <= k <= +inf.0
(define (flgamma-sample-huge k s n)
  (cond [(fl= k +inf.0)  (build-flvector n (λ (_) +inf.0))]
        [else
         (define xs (flnormal-sample k (flsqrt k) n))
         (for ([i  (in-range n)])
           (define x (unsafe-flvector-ref xs i))
           (unsafe-flvector-set! xs i (flmax 0.0 (fl* s x))))
         xs]))

(: flgamma-sample (Flonum Flonum Integer -> FlVector))
(define (flgamma-sample k s n)
  (cond [(n . < . 0)  (raise-argument-error 'flgamma-sample "Natural" 2 k s n)]
        [(k . fl>= . 1e10)  (flgamma-sample-huge k s n)]
        [(k . fl>= . 3.0)   (flgamma-sample-large k s n)]
        [(k . fl>= . 2.0)   (flgamma-sample-2-3 k s n)]
        [(k . fl>= . 1.0)   (flgamma-sample-1-2 k s n)]
        [(k . fl>= . 0.0)   (flgamma-sample-small k s n)]
        [else  (build-flvector n (λ (_) +nan.0))]))
