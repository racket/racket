#lang typed/racket

(provide Matrix Result-Matrix
         array-matrix?
         matrix=
         square-matrix? 
         square-matrix-size)

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
