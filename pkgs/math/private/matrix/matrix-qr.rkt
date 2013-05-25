#lang typed/racket/base

(require "matrix-types.rkt"
         "matrix-basic.rkt"
         "matrix-arithmetic.rkt"
         "matrix-constructors.rkt"
         "matrix-gram-schmidt.rkt"
         "utils.rkt"
         "../array/array-transform.rkt"
         "../array/array-struct.rkt")

(provide matrix-qr)

#|
QR decomposition currently does Gram-Schmidt twice, as suggested by

  Luc Giraud, Julien Langou, Miroslav Rozloznik.
  On the round-off error analysis of the Gram-Schmidt algorithm with reorthogonalization.
  Technical Report, 2002.

It normalizes only the second time.

I've verified experimentally that, with random, square matrices (elements in [0,1]), doing so
produces matrices for which `matrix-orthogonal?' returns #t with eps <= 10*epsilon.0, apparently
independently of the matrix size.
|#

(: matrix-qr/ns (case-> ((Matrix Flonum) Any -> (Values (Matrix Flonum) (Matrix Flonum)))
                        ((Matrix Real) Any -> (Values (Matrix Real) (Matrix Real)))
                        ((Matrix Float-Complex) Any -> (Values (Matrix Float-Complex)
                                                               (Matrix Float-Complex)))
                        ((Matrix Number) Any -> (Values (Matrix Number) (Matrix Number)))))
(define (matrix-qr/ns M full?)
  (define x00 (matrix-ref M 0 0))
  (define zero (zero* x00))
  (define one (one* x00))
  (define B (matrix-gram-schmidt M #f))
  (define Q
    (matrix-gram-schmidt
     (cond [(or (square-matrix? B) (and (matrix? B) (not full?)))  B]
           [(matrix? B)  (array-append* (list B (matrix-basis-extension B)) 1)]
           [full?  (identity-matrix (matrix-num-rows M) one zero)]
           [else  (matrix-col (identity-matrix (matrix-num-rows M) one zero) 0)])
     #t))
  (values Q (matrix-upper-triangle (matrix* (matrix-hermitian Q) M) zero)))

(: matrix-qr (case-> ((Matrix Flonum)     -> (Values (Matrix Flonum) (Matrix Flonum)))
                     ((Matrix Flonum) Any -> (Values (Matrix Flonum) (Matrix Flonum)))
                     ((Matrix Real)     -> (Values (Matrix Real) (Matrix Real)))
                     ((Matrix Real) Any -> (Values (Matrix Real) (Matrix Real)))
                     ((Matrix Float-Complex)     -> (Values (Matrix Float-Complex)
                                                            (Matrix Float-Complex)))
                     ((Matrix Float-Complex) Any -> (Values (Matrix Float-Complex)
                                                            (Matrix Float-Complex)))
                     ((Matrix Number)     -> (Values (Matrix Number) (Matrix Number)))
                     ((Matrix Number) Any -> (Values (Matrix Number) (Matrix Number)))))
(define (matrix-qr M [full? #t])
  (define-values (Q R) (parameterize ([array-strictness #f])
                         (matrix-qr/ns M full?)))
  (values (array-default-strict Q)
          (array-default-strict R)))
