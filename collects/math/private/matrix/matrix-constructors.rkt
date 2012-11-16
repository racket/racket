#lang typed/racket

(require math/array
         "../unsafe.rkt"
         "matrix-types.rkt")

(provide identity-matrix flidentity-matrix 
         matrix->list list->matrix fllist->matrix
         matrix->vector vector->matrix flvector->matrix
         flat-vector->matrix
         make-matrix
         matrix-row
         matrix-column
         submatrix)

(: identity-matrix (Integer -> (Matrix Real)))
(define (identity-matrix m) (diagonal-array 2 m 1 0))

(: flidentity-matrix (Integer -> (Matrix Float)))
(define (flidentity-matrix m) (diagonal-array 2 m 1.0 0.0))

(: make-matrix (All (A) (Integer Integer A -> (Matrix A))))
(define (make-matrix m n x)
  (make-array (vector m n) x))

(: list->matrix : (Listof* Number) -> (Matrix Number))
(define (list->matrix rows)
  (list*->array rows number?))

(: fllist->matrix : (Listof* Flonum) -> (Matrix Flonum))
(define (fllist->matrix rows)
  (list*->array rows flonum? ))

(: matrix->list : (All (A) (Matrix A) -> (Listof* A)))
(define (matrix->list a)
  (array->list* a))

(: vector->matrix : (Vectorof* Number) -> (Matrix Number))
(define (vector->matrix rows)
  (vector*->array rows number? ))

(: flvector->matrix : (Vectorof* Flonum) -> (Matrix Flonum))
(define (flvector->matrix rows)
  (vector*->array rows flonum? ))

(: matrix->vector : (All (A) (Matrix A) -> (Vectorof* A)))
(define (matrix->vector a)
  (array->vector* a))

(: submatrix : (Matrix Number) (Sequenceof Index) (Sequenceof Index) -> (Matrix Number))
(define (submatrix a row-range col-range)
  (array-slice-ref a (list row-range col-range)))

(: matrix-row : (Matrix Number) Index -> (Matrix Number))
(define (matrix-row a i)
  (define-values (m n) (matrix-dimensions a))
  (array-slice-ref a (list (in-range i (add1 i)) (in-range n))))

(: matrix-column : (Matrix Number) Index -> (Matrix Number))
(define (matrix-column a j)
  (define-values (m n)(matrix-dimensions a))
  (array-slice-ref a (list (in-range m) (in-range j (add1 j)))))
