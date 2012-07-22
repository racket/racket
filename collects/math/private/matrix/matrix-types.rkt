#lang typed/racket

(provide Matrix Result-Matrix
         array-matrix?
         matrix=
         square-matrix? 
         square-matrix-size
         matrix-dimensions
         matrix-row-dimension
         matrix-column-dimension)

(require math/array)

(define-type (Matrix A) (Array A))
(define-type (Result-Matrix A) (lazy-array A))

(: array-matrix? (All (A) ((Array A) -> Boolean)))
(define (array-matrix? x)
  (= 2 (array-dims x)))

(: square-matrix? : (All (A) (Matrix A) -> Boolean))
(define (square-matrix? a)
  (and (array-matrix? a)
       (let ([sh (unsafe-array-shape a)])
         (= (vector-ref sh 0) (vector-ref sh 1)))))

(: square-matrix-size : (All (A) (Matrix A) -> Index))
(define (square-matrix-size a)
  (vector-ref (unsafe-array-shape a) 0))

(: matrix= : (Matrix Number) (Matrix Number) -> Boolean)
(define matrix= array=)

(: matrix-dimensions : (Matrix Number) -> (Values Index Index))
(define (matrix-dimensions a)
  (define sh (unsafe-array-shape a))
  (values (vector-ref sh 0) (vector-ref sh 1)))

(: matrix-row-dimension : (Matrix Number) -> Index)
(define (matrix-row-dimension a)
  (define sh (unsafe-array-shape a))
  (displayln (list 'matrix-row-dimension: 'sh= sh))
  (displayln (list 'matrix-row-dimension: 'vector-ref-sh-0 (vector-ref sh 0)))
  (vector-ref sh 0))

(: matrix-column-dimension : (Matrix Number) -> Index)
(define (matrix-column-dimension a)
  (define sh (unsafe-array-shape a))
  (vector-ref sh 1))
