#lang typed/racket/base

(require "../array/array-struct.rkt"
         "../array/array-fold.rkt"
         "../array/array-pointwise.rkt"
         "../unsafe.rkt")

(provide Matrix
         matrix?
         square-matrix?
         row-matrix?
         col-matrix?
         matrix-shape
         square-matrix-size
         matrix-num-rows
         matrix-num-cols)

;; Matrices are represented as arrays, but additionally must be nonempty and have exactly two axes
(define-type (Matrix A) (Array A))

(: matrix? (All (A) ((Array A) -> Boolean)))
(define (matrix? arr)
  (and (> (array-size arr) 0)
       (= (array-dims arr) 2)))

(: square-matrix? (All (A) ((Array A) -> Boolean)))
(define (square-matrix? arr)
  (define ds (array-shape arr))
  (and (= (vector-length ds) 2)
       (let ([d0  (unsafe-vector-ref ds 0)]
             [d1  (unsafe-vector-ref ds 1)])
         (and (> d0 0) (> d1 0) (= d0 d1)))))

(: row-matrix? (All (A) ((Array A) -> Boolean)))
(define (row-matrix? arr)
  (define ds (array-shape arr))
  (and (= (vector-length ds) 2)
       (= (unsafe-vector-ref ds 0) 1)
       (> (unsafe-vector-ref ds 1) 0)))

(: col-matrix? (All (A) ((Array A) -> Boolean)))
(define (col-matrix? arr)
  (define ds (array-shape arr))
  (and (= (vector-length ds) 2)
       (> (unsafe-vector-ref ds 0) 0)
       (= (unsafe-vector-ref ds 1) 1)))

(: matrix-shape (All (A) ((Array A) -> (Values Index Index))))
(define (matrix-shape a)
  (define ds (array-shape a))
  (if (and (> (array-size a) 0)
           (= (vector-length ds) 2))
      (values (unsafe-vector-ref ds 0)
              (unsafe-vector-ref ds 1))
      (raise-argument-error 'matrix-shape "matrix?" a)))

(: square-matrix-size (All (A) ((Array A) -> Index)))
(define (square-matrix-size arr)
  (cond [(square-matrix? arr)  (unsafe-vector-ref (array-shape arr) 0)]
        [else  (raise-argument-error 'square-matrix-size "square-matrix?" arr)]))

(: matrix-num-rows (All (A) ((Array A) -> Index)))
(define (matrix-num-rows a)
  (cond [(matrix? a)  (vector-ref (array-shape a) 0)]
        [else  (raise-argument-error 'matrix-num-rows "matrix?" a)]))

(: matrix-num-cols (All (A) ((Array A) -> Index)))
(define (matrix-num-cols a)
  (cond [(matrix? a)  (vector-ref (array-shape a) 1)]
        [else  (raise-argument-error 'matrix-num-cols "matrix?" a)]))
