#lang typed/racket

(require math/array
         "matrix-types.rkt"
         "matrix-constructors.rkt"
         "matrix-arithmetic.rkt")

(provide matrix-expt)

(: matrix-expt : (Matrix Number) Integer -> (Matrix Number))
(define (matrix-expt a n)
  (cond [(not (square-matrix? a))  (raise-argument-error 'matrix-expt "square-matrix?" 0 a n)]
        [(negative? n)  (raise-argument-error 'matrix-expt "Natural" 1 a n)]
        [(zero? n)      (identity-matrix (square-matrix-size a))]
        [else
         (let: loop : (Matrix Number) ([n : Positive-Integer  n])
           (cond [(= n 1)  a]
                 [(= n 2)  (matrix* a a)]
                 [(even? n) (let ([a^n/2 (matrix-expt a (quotient n 2))])
                              (matrix* a^n/2 a^n/2))]
                 [else     (matrix* a (matrix-expt a (sub1 n)))]))]))
