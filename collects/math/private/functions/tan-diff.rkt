#lang typed/racket/base

(require racket/fixnum
         "../../flonum.rkt"
         "../vector/vector.rkt")

(provide fltan-diff/y flcot-diff/y)

(define coef-hash-length 164)

(: coef-hash (Vectorof (Option FlVector)))
(define coef-hash (make-vector coef-hash-length #f))

(: tan-diff-coefs (Natural -> FlVector))
(define (tan-diff-coefs n)
  (vector-ref!
   coef-hash n
   (λ ()
     (cond [(zero? n)  (flvector 0.0 1.0)]
           [else
            (define cs (tan-diff-coefs (- n 1)))
            (build-flvector
             (+ n 2)
             (λ (k) (fl+ (cond [(k . > . 0)  (define k-1 (- k 1))
                                             (fl* (fl k-1) (flvector-ref cs k-1))]
                               [else  0.0])
                         (cond [(k . < . n)  (define k+1 (+ k 1))
                                             (fl* (fl k+1) (flvector-ref cs k+1))]
                               [else  0.0]))))]))))

(: fltan-diff/y (Natural Flonum -> Flonum))
(define (fltan-diff/y n y)
  (cond [(zero? n)  y]
        [(n . < . coef-hash-length)
         (define cs (tan-diff-coefs n))
         (let: loop : Flonum ([i : Nonnegative-Fixnum  (flvector-length cs)]
                              [z : Flonum  0.0])
           (cond [(zero? i)  z]
                 [else  (let ([i  (fx- i 1)])
                          (loop i (fl+ (fl* y z) (flvector-ref cs i))))]))]
        [(even? n)
         (cond [(y . fl< . 0.0)  -inf.0]
               [(y . fl> . 0.0)  +inf.0]
               [else  y])]
        [else  +inf.0]))

(: flcot-diff/y (Natural Flonum -> Flonum))
(define (flcot-diff/y n y)
  (define z (fltan-diff/y n y))
  (if (even? n) z (- z)))
