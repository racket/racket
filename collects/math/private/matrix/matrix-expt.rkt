#lang typed/racket

(require "../../array.rkt"
         "matrix-constructors.rkt"
         "matrix-multiply.rkt"
         "matrix-types.rkt")

(provide matrix-expt)

(: matrix-expt : (Matrix Number) Integer -> (Matrix Number))
(define (matrix-expt a n)
  (unless (array-matrix? a)
    (raise-type-error 'matrix-expt "(Matrix Number)" a))
  (unless (square-matrix? a)
    (error 'matrix-expt "Square matrix expected, got ~a" a))
  (cond
    [(= n 0)  (identity-matrix (square-matrix-size a))]
    [(= n 1)  a]
    [(= n 2)  (matrix* a a)]
    [(even? n) (let ([a^n/2 (matrix-expt a (quotient n 2))])
                 (matrix* a^n/2 a^n/2))]
    [else     (matrix* a (matrix-expt a (sub1 n)))]))
