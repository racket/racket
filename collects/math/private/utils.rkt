#lang typed/racket/base

(require racket/flonum racket/unsafe/ops)

(provide (all-defined-out))

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)
(define-predicate listof-nonnegative-fixnum? (Listof Nonnegative-Fixnum))

(: build-flvector (Integer (Index -> Float) -> FlVector))
(define (build-flvector n proc)
  (cond [(and (index? n) (positive? n))
         (define v (make-flvector n))
         (let: loop : FlVector ([i : Positive-Index  n])
           (let ([i  (sub1 i)])
             (unsafe-flvector-set! v i (proc i))
             (cond [(zero? i)  v]
                   [else  (loop i)])))]
        [(= n 0)  (make-flvector 0)]
        [else  (raise-type-error 'build-flvector "nonnegative Index" 0 n proc)]))
