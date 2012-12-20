#lang typed/racket/base

(require "../array/array-struct.rkt"
         "../array/array-fold.rkt"
         "../array/array-pointwise.rkt"
         "../unsafe.rkt")

(provide Matrix
         Column Result-Column
         Column-Matrix
         matrix?
         square-matrix?
         row-matrix?
         col-matrix?
         matrix-shape
         square-matrix-size
         matrix-num-rows
         matrix-num-cols)

(define-type (Matrix A) (Array A))
; matrices are represented as arrays
(define-type (Column A)        (U (Vectorof A) (Matrix A)))
; functions accepting column vectors accept
; both mx1 matries and (Racket) vectors as column vectors
(define-type (Result-Column A) (Matrix A))
; result columns are always lazy arrays
(define-type (Column-Matrix A) (Matrix A))
; a column vector represented as a matrix

(define matrix?
  (plambda: (A) ([arr : (Array A)])
    (and (> (array-size arr) 0)
         (= (array-dims arr) 2))))

(define square-matrix?
  (plambda: (A) ([arr : (Array A)])
    (and (matrix? arr)
         (let ([sh (array-shape arr)])
           (= (vector-ref sh 0) (vector-ref sh 1))))))

(define row-matrix?
  (plambda: (A) ([arr : (Array A)])
    (and (matrix? arr)
         (= (vector-ref (array-shape arr) 0) 1))))

(define col-matrix?
  (plambda: (A) ([arr : (Array A)])
    (and (matrix? arr)
         (= (vector-ref (array-shape arr) 1) 1))))

(: matrix-shape : (All (A) (Matrix A) -> (Values Index Index)))
(define (matrix-shape a)
  (cond [(matrix? a)  (define sh (array-shape a))
                      (values (unsafe-vector-ref sh 0) (unsafe-vector-ref sh 1))]
        [else  (raise-argument-error 'matrix-shape "matrix?" a)]))

(: square-matrix-size (All (A) ((Matrix A) -> Index)))
(define (square-matrix-size arr)
  (cond [(square-matrix? arr)  (unsafe-vector-ref (array-shape arr) 0)]
        [else  (raise-argument-error 'square-matrix-size "square-matrix?" arr)]))

(: matrix-num-rows (All (A) ((Matrix A) -> Index)))
(define (matrix-num-rows a)
  (cond [(matrix? a)  (vector-ref (array-shape a) 0)]
        [else  (raise-argument-error 'matrix-col-length "matrix?" a)]))

(: matrix-num-cols (All (A) ((Matrix A) -> Index)))
(define (matrix-num-cols a)
  (cond [(matrix? a)  (vector-ref (array-shape a) 1)]
        [else  (raise-argument-error 'matrix-row-length "matrix?" a)]))
