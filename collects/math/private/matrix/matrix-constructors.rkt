#lang typed/racket

(require racket/unsafe/ops
         math/array
         "matrix-types.rkt")

(provide identity-matrix flidentity-matrix 
         matrix->list list->matrix fllist->matrix
         matrix->vector vector->matrix flvector->matrix
         const-matrix
         matrix-row
         matrix-column
         submatrix)

(: identity-matrix (Integer -> (Result-Matrix Real)))
(define (identity-matrix m) (diagonal-array 2 m 1 0))

(: flidentity-matrix (Integer -> (Result-Matrix Float)))
(define (flidentity-matrix m) (diagonal-array 2 m 1.0 0.0))

(: const-matrix (All (A) (Integer Integer A -> (Result-Matrix A))))
(define (const-matrix m n x)
  (const-array (list m n) x))

(: list->matrix : (Listof* Number) -> (Result-Matrix Number))
(define (list->matrix rows)
  (list->array number? rows))

(: fllist->matrix : (Listof* Flonum) -> (Result-Matrix Flonum))
(define (fllist->matrix rows)
  (list->array flonum? rows))

(: matrix->list : (All (A) (Matrix A) -> (Listof* A)))
(define (matrix->list a)
  (array->list a))

(: vector->matrix : (Vectorof* Number) -> (Result-Matrix Number))
(define (vector->matrix rows)
  (vector->array number? rows))

(: flvector->matrix : (Vectorof* Flonum) -> (Result-Matrix Flonum))
(define (flvector->matrix rows)
  (vector->array flonum? rows))

(: matrix->vector : (All (A) (Matrix A) -> (Vectorof* A)))
(define (matrix->vector a)
  (array->vector a))

(: submatrix : (Matrix Number) (Sequenceof Index) (Sequenceof Index) -> (Result-Matrix Number))
(define (submatrix a row-range col-range)
  ((inst array-slice Number) a (list row-range col-range)))

(: matrix-row : (Matrix Number) Index -> (Result-Matrix Number))
(define (matrix-row a i)
  (define-values (m n) (matrix-dimensions a))
  ((inst array-slice Number) a (list (in-range i (add1 i)) (in-range n))))

(: matrix-column : (Matrix Number) Index -> (Result-Matrix Number))
(define (matrix-column a j)
  (define-values (m n)(matrix-dimensions a))
  ((inst array-slice Number) a (list (in-range m) (in-range j (add1 j)))))
