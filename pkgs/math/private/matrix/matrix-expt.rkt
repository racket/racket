#lang typed/racket/base

(require "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-arithmetic.rkt"
         "utils.rkt"
         "../array/array-struct.rkt"
         "../array/utils.rkt"
         "../unsafe.rkt")

(provide matrix-expt)

(: matrix-expt/ns (case-> ((Matrix Flonum) Positive-Integer -> (Matrix Flonum))
                          ((Matrix Real) Positive-Integer -> (Matrix Real))
                          ((Matrix Float-Complex) Positive-Integer -> (Matrix Float-Complex))
                          ((Matrix Number) Positive-Integer -> (Matrix Number))))
(define (matrix-expt/ns a n)
  (define n/2 (quotient n 2))
  (if (zero? n/2)
      ;; n = 1
      a
      (let ([m  (* n/2 2)])
        (if (= n m)
            ;; n is even
            (let ([a^n/2  (matrix-expt/ns a n/2)])
              (matrix* a^n/2 a^n/2))
            ;; m = n - 1
            (matrix* a (matrix-expt/ns a m))))))

(: matrix-expt (case-> ((Matrix Flonum) Integer -> (Matrix Flonum))
                       ((Matrix Real) Integer -> (Matrix Real))
                       ((Matrix Float-Complex) Integer -> (Matrix Float-Complex))
                       ((Matrix Number) Integer -> (Matrix Number))))
(define (matrix-expt a n)
  (cond [(not (square-matrix? a))  (raise-argument-error 'matrix-expt "square-matrix?" 0 a n)]
        [(negative? n)  (raise-argument-error 'matrix-expt "Natural" 1 a n)]
        [(zero? n)
         (define proc (unsafe-array-proc a))
         (array-default-strict
          (unsafe-build-array
           (array-shape a)
           (λ: ([ij : Indexes])
             (define x (proc ij))
             (if (= (unsafe-vector-ref ij 0) (unsafe-vector-ref ij 1)) (one* x) (zero* x)))))]
        [else  (call/ns (λ () (matrix-expt/ns a n)))]))
