#lang typed/racket/base

(require "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-arithmetic.rkt"
         "utils.rkt")

(provide matrix-expt)

(: matrix-expt/ns (case-> ((Matrix Real) Positive-Integer -> (Matrix Real))
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

(: matrix-expt (case-> ((Matrix Real) Integer -> (Matrix Real))
                       ((Matrix Number) Integer -> (Matrix Number))))
(define (matrix-expt a n)
  (cond [(not (square-matrix? a))  (raise-argument-error 'matrix-expt "square-matrix?" 0 a n)]
        [(negative? n)  (raise-argument-error 'matrix-expt "Natural" 1 a n)]
        [(zero? n)  (identity-matrix (square-matrix-size a))]
        [else  (call/ns (λ () (matrix-expt/ns a n)))]))
