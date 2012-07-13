#lang typed/racket/base

(require "../../array.rkt"
         "matrix-types.rkt")

(provide identity-matrix flidentity-matrix 
         matrix->list list->matrix fllist->matrix
         matrix->vector vector->matrix flvector->matrix
         const-matrix)

(: identity-matrix (Integer -> (Result-Matrix Real)))
(define (identity-matrix size) (diagonal-array 2 size 1 0))

(: flidentity-matrix (Integer -> (Result-Matrix Float)))
(define (flidentity-matrix size) (diagonal-array 2 size 1.0 0.0))

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
