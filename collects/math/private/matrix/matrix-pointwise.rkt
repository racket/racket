#lang typed/racket

(require "../unsafe.rkt"
         "../../array.rkt"
         "matrix-types.rkt")

(provide matrix+ matrix-
         matrix.sqr matrix.magnitude)

;; The `make-matrix-*' operators have to be macros; see ../array/array-pointwise.rkt for an
;; explanation.

#;(: make-matrix-pointwise1 (All (A) (Symbol
                                      ((Array A) -> (Array A))
                                      -> ((Array A) -> (Array A)))))
(define-syntax-rule (make-matrix-pointwise1 name array-op1)
  (λ (arr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" arr))
    (array-op1 arr)))

#;(: make-matrix-pointwise2 (All (A) (Symbol
                                      ((Array A) (Array A) -> (Array A))
                                      -> ((Array A) (Array A) -> (Array A)))))
(define-syntax-rule (make-matrix-pointwise2 name array-op2)
  (λ (arr brr)
    (unless (array-matrix? arr) (raise-type-error name "matrix" 0 arr brr))
    (unless (array-matrix? brr) (raise-type-error name "matrix" 1 arr brr))
    (array-op2 arr brr)))

#;(: make-matrix-pointwise1/2 
     (All (A) (Symbol
               (case-> ((Array A)           -> (Array A))
                       ((Array A) (Array A) -> (Array A)))
               -> 
               (case-> ((Array A)           -> (Array A))
                       ((Array A) (Array A) -> (Array A))))))
(define-syntax-rule (make-matrix-pointwise1/2 name array-op1/2)
  (case-lambda
    [(arr)      ((make-matrix-pointwise1 name array-op1/2) arr)]
    [(arr brr)  ((make-matrix-pointwise2 name array-op1/2) arr brr)]))

;; ---------------------------------------------------------------------------------------------------

(: matrix+ (case-> ((Matrix Real)   (Matrix Real)   -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) -> (Matrix Number))))
(: matrix- (case-> ((Matrix Real)                   -> (Matrix Real))
                   ((Matrix Number)                 -> (Matrix Number))
                   ((Matrix Real)   (Matrix Real)   -> (Matrix Real))
                   ((Matrix Number) (Matrix Number) -> (Matrix Number))))

(define matrix+ (make-matrix-pointwise2 'matrix+ array+))
(define matrix- (make-matrix-pointwise1/2 'matrix- array-))
(define matrix.sqr array-sqr)
(define matrix.magnitude array-magnitude)
