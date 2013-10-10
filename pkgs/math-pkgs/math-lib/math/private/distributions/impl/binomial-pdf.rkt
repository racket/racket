#lang typed/racket/base

(require "../../../flonum.rkt"
         "../../../base.rkt"
         "../../functions/stirling-error.rkt"
         "../../functions/log-gamma.rkt"
         "../normal-dist.rkt")

(provide flbinomial-pdf flbinomial-log-pdf)

(: flbinomial-pdf-normal (Flonum Flonum Flonum -> Flonum))
;; Normal approximation with "continuity correction" (i.e. adding 0.5)
(define (flbinomial-pdf-normal n p k)
  (flnormal-pdf (* n p) (flsqrt (* n p (- 1.0 p))) (+ k 0.5) #f))

(: near-pow2 (Flonum -> Flonum))
(define (near-pow2 d)
  (max 1.0 (flexpt 2.0 (flfloor (/ (fllog d) (fllog 2.0))))))

(: flbinomial-pdf (Flonum Flonum Flonum -> Flonum))
;; Maximum error grows without bound in the size of `n' (todo: figure out what it's reasonable for)
(define (flbinomial-pdf n p k)
  (cond [(or (not (integer? n)) (n . fl< . 0.0)
             (p . fl< . 0.0) (p . fl> . 1.0)
             (not (integer? k)))
         +nan.0]
        [(or (k . fl< . 0.0) (k . fl> . n))  0.0]
        [(fl= p 0.0)  (if (fl= k 0.0) 1.0 0.0)]
        [(fl= p 1.0)  (if (fl= k n) 1.0 0.0)]
        [(fl= k 0.0)  (flexpt1p (- p) n)]
        [(fl= k n)    (flexpt p n)]
        [(n . fl> . 2e13)
         ;; About here, the normal approximation does better than the algorithms used below
         ;; The error is NOT GREAT: it's somewhere around 7e10 ulps max
         (flbinomial-pdf-normal n p k)]
        [else
         (define n-k (fl- n k))
         (define binom-n-k (flbinomial n k))
         (cond
           [(rational? binom-n-k)
            ;; Figure out how much to reduce exponents so flexpt doesn't underflow
            (define d (near-pow2 (fl/ (flmin (fl* n-k (fllog1p (- p))) (fl* k (fllog p)))
                                      (fllog (flsqrt +max-subnormal.0)))))
            ;; Standard definition, with argument reduction
            (flexpt (* (flexpt binom-n-k (/ d))
                       (flexpt p (fl/ k d))
                       (flexpt1p (- p) (fl/ n-k d)))
                    d)]
           [else
            (define-values (n/n-k n/n-k-lo) (fast-fl//error n n-k))
            (define-values (n/k n/k-lo) (fast-fl//error n k))
            ;; Figure out how much to reduce exponents so flexpt doesn't underflow OR overflow
            (define d (near-pow2 (flmax (fl/ (flmax (fl* n-k (fllog n/n-k)) (fl* k (fllog n/k)))
                                             (fllog (flexpt +max.0 #i1/3)))
                                        (fl/ (flmin (fl* n-k (fllog1p (- p))) (fl* k (fllog p)))
                                             (fllog (flexpt +max-subnormal.0 #i1/3))))))
            ;; Following arrived at by expanding `flgamma' in terms of `flstirling' and
            ;; recombining terms; note that exponentiation is by `n-k' and `k', which are both
            ;; smaller than `n'
            (* (flexpt (* (flexpt1p (- p) (fl/ n-k d))
                          (flexpt+ n/n-k n/n-k-lo (fl/ n-k d))
                          (flexpt p (fl/ k d))
                          (flexpt+ n/k n/k-lo (fl/ k d)))
                       d)
               (flexp (- (flstirling n) (flstirling k) (flstirling n-k)))
               (flsqrt (fl/ (fl/ n/k n-k) (fl* 2.0 pi))))])]))

(: flbinomial-log-pdf (Flonum Flonum Flonum -> Flonum))
;; Maximum error grows without bound in `n', but is <= 4 ulps for n <= 1e4
(define (flbinomial-log-pdf n p k)
  (cond [(or (not (flinteger? n)) (n . fl< . 0.0)
             (p . fl< . 0.0) (p . fl> . 1.0)
             (not (flinteger? k)))
         +nan.0]
        [(or (k . fl< . 0.0) (k . fl> . n))  -inf.0]
        [(fl= p 0.0)  (if (fl= k 0.0) 0.0 -inf.0)]
        [(fl= p 1.0)  (if (fl= k n) 0.0 -inf.0)]
        [(fl= k 0.0)  (fl* n (fllog1p (- p)))]
        [(fl= k n)    (fl* n (fllog p))]
        [(n . fl> . 1e300)
         (define n-k (fl- n k))
         (define n/k (fl/ n k))
         ;; Dividing by 64 is an exact argument reduction that keeps this working when n = +max.0
         (+ (- (flstirling n) (flstirling k) (flstirling n-k))
            (fllog (flsqrt (fl/ (fl/ n/k n-k) (fl* 2.0 pi))))
            (fl* 64.0 (+ (* #i1/64 n-k (fllog1p (- p)))
                         (* #i1/64 n-k (fllog (fl/ n n-k)))
                         (* #i1/64 k (fllog p))
                         (* #i1/64 k (fllog n/k)))))]
        [else
         ;; A direct port of the last algorithm in `flbinomial-pdf' to log space
         (define n-k (fl- n k))
         (define-values (n/n-k n/n-k-lo) (fast-fl//error n n-k))
         (define-values (n/k n/k-lo) (fast-fl//error n k))
         ;; Figure out how much to reduce exponents so flexpt doesn't underflow or overflow
         (define d (near-pow2 (flmax (fl/ (flmax (fl* n-k (fllog n/n-k)) (fl* k (fllog n/k)))
                                          (fllog (flexpt +max.0 #i1/3)))
                                     (fl/ (flmin (fl* n-k (fllog1p (- p))) (fl* k (fllog p)))
                                          (fllog (flexpt +max-subnormal.0 #i1/3))))))
         ;; More accurate here to use d*log(x) = log(x^d)
         (+ (fl* d (fllog (* (flexpt1p (- p) (fl/ n-k d))
                             (flexpt+ n/n-k n/n-k-lo (fl/ n-k d))
                             (flexpt p (fl/ k d))
                             (flexpt+ n/k n/k-lo (fl/ k d)))))
            (- (flstirling n) (flstirling k) (flstirling n-k))
            (fllog (flsqrt (fl/ (fl/ n/k n-k) (fl* 2.0 pi)))))]))
