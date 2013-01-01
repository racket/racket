#lang typed/racket/base

#|
Two of the functions defined here currently just raise an error: `matrix-op-2norm' and
`matrix-op-angle'. They need to compute, respectively, the maximum and minimum singular values of
their matrix argument.

See "How to Measure Errors" in the LAPACK manual for more details:

  http://www.netlib.org/lapack/lug/node75.html
  http://www.netlib.org/lapack/lug/node76.html
|#

(require racket/list
         racket/fixnum
         math/flonum
         "matrix-types.rkt"
         "matrix-arithmetic.rkt"
         "matrix-constructors.rkt"
         "matrix-basic.rkt"
         "utils.rkt"
         "../array/array-struct.rkt"
         "../array/array-pointwise.rkt"
         "../array/array-fold.rkt"
         )


(provide
 ;; Operator norms
 matrix-op-1norm
 matrix-op-2norm
 matrix-op-inf-norm
 matrix-basis-angle
 ;; Error measurement
 matrix-error-norm
 matrix-absolute-error
 matrix-relative-error
 ;; Approximate predicates
 matrix-identity?
 matrix-orthonormal?
 )

(: matrix-op-1norm ((Matrix Number) -> Nonnegative-Real))
;; When M is a column matrix, this is equivalent to matrix-1norm
(define (matrix-op-1norm M)
  (assert (apply max (map matrix-1norm (matrix-cols M))) nonnegative?))

(: matrix-op-2norm ((Matrix Number) -> Nonnegative-Real))
;; When M is a column matrix, this is equivalent to matrix-2norm
(define (matrix-op-2norm M)
  ;(matrix-max-singular-value M)
  ;(sqrt (matrix-max-eigenvalue M))
  (error 'unimplemented))

(: matrix-op-inf-norm ((Matrix Number) -> Nonnegative-Real))
;; When M is a column matrix, this is equivalent to matrix-inf-norm
(define (matrix-op-inf-norm M)
  (assert (apply max (map matrix-1norm (matrix-rows M))) nonnegative?))

(: matrix-basis-angle (case-> ((Matrix Real) (Matrix Real) -> Real)
                              ((Matrix Number) (Matrix Number) -> Number)))
;; Returns the angle between the two subspaces spanned by the two given sets of column vectors
(define (matrix-basis-angle M R)
  ;(acos (matrix-min-singular-value (matrix* (matrix-hermitian M) R)))
  (error 'unimplemented))

;; ===================================================================================================
;; Error measurement

(: matrix-error-norm (Parameterof ((Matrix Number) -> Nonnegative-Real)))
(define matrix-error-norm (make-parameter matrix-op-inf-norm))

(: matrix-absolute-error
   (case-> ((Matrix Number) (Matrix Number) -> Nonnegative-Real)
           ((Matrix Number) (Matrix Number) ((Matrix Number) -> Nonnegative-Real)
                            -> Nonnegative-Real)))
(define (matrix-absolute-error M R [norm (matrix-error-norm)])
  (define-values (m n) (matrix-shapes 'matrix-absolute-error M R))
  (array-strict! M)
  (array-strict! R)
  (cond [(array-all-and (inline-array-map eqv? M R))  0]
        [(and (array-all-and (inline-array-map number-rational? M))
              (array-all-and (inline-array-map number-rational? R)))
         (norm (matrix- (inline-array-map inexact->exact M)
                        (inline-array-map inexact->exact R)))]
        [else  +inf.0]))

(: matrix-relative-error
   (case-> ((Matrix Number) (Matrix Number) -> Nonnegative-Real)
           ((Matrix Number) (Matrix Number) ((Matrix Number) -> Nonnegative-Real)
                            -> Nonnegative-Real)))
(define (matrix-relative-error M R [norm (matrix-error-norm)])
  (define-values (m n) (matrix-shapes 'matrix-relative-error M R))
  (array-strict! M)
  (array-strict! R)
  (cond [(array-all-and (inline-array-map eqv? M R))  0]
        [(and (array-all-and (inline-array-map number-rational? M))
              (array-all-and (inline-array-map number-rational? R)))
         (define num (norm (matrix- M R)))
         (define den (norm R))
         (cond [(and (zero? num) (zero? den))  0]
               [(zero? den)  +inf.0]
               [else  (assert (/ num den) nonnegative?)])]
        [else  +inf.0]))

;; ===================================================================================================
;; Approximate predicates

(: matrix-identity? (case-> ((Matrix Number) -> Boolean)
                            ((Matrix Number) Real -> Boolean)))
(define (matrix-identity? M [eps (* 10 epsilon.0)])
  (cond [(eps . < . 0)  (raise-argument-error 'matrix-identity? "Nonnegative-Real" 1 M eps)]
        [else  (and (square-matrix? M)
                    (<= (matrix-relative-error M (identity-matrix (square-matrix-size M))) eps))]))

(: matrix-orthonormal? (case-> ((Matrix Number) -> Boolean)
                               ((Matrix Number) Real -> Boolean)))
(define (matrix-orthonormal? M [eps (* 10 epsilon.0)])
  (cond [(eps . < . 0)  (raise-argument-error 'matrix-orthonormal? "Nonnegative-Real" 1 M eps)]
        [else  (and (square-matrix? M)
                    (matrix-identity? (matrix* M (matrix-hermitian M)) eps))]))
