#lang typed/racket
(provide Matrix
         Column Result-Column
         Column-Matrix
         array-matrix?
         matrix-all=
         square-matrix? 
         square-matrix-size
         matrix-dimensions
         matrix-row-dimension
         matrix-column-dimension)

(require math/array)

(define-type (Matrix A) (Array A))
; matrices are represented as arrays
(define-type (Column A)        (U (Vectorof A) (Matrix A)))
; functions accepting column vectors accept
; both mx1 matries and (Racket) vectors as column vectors
(define-type (Result-Column A) (Matrix A))
; result columns are always lazy arrays
(define-type (Column-Matrix A) (Matrix A))
; a column vector represented as a matrix

(: array-matrix? (All (A) ((Array A) -> Boolean)))
(define (array-matrix? x)
  (= 2 (array-dims x)))

(: square-matrix? : (All (A) (Matrix A) -> Boolean))
(define (square-matrix? a)
  (and (array-matrix? a)
       (let ([sh (array-shape a)])
         (= (vector-ref sh 0) (vector-ref sh 1)))))

(: square-matrix-size : (All (A) (Matrix A) -> Index))
(define (square-matrix-size a)
  (vector-ref (array-shape a) 0))

(: matrix-all= : (Matrix Number) (Matrix Number) -> Boolean)
(define matrix-all= array-all=)

(: matrix-dimensions : (Matrix Number) -> (Values Index Index))
(define (matrix-dimensions a)
  (define sh (array-shape a))
  ; TODO: Remove list conversion when trbug1 is fixed
  (define sh-tmp (vector->list sh))
  (values (car sh-tmp) (cadr sh-tmp))
  ; (values (vector-ref sh 0) (vector-ref sh 1))
  )

(: matrix-row-dimension : (Matrix Number) -> Index)
(define (matrix-row-dimension a)
  (define sh (array-shape a))
  (vector-ref sh 0))

(: matrix-column-dimension : (Matrix Number) -> Index)
(define (matrix-column-dimension a)
  (define sh (array-shape a))
  (vector-ref sh 1))
