#lang typed/racket/base

(require "../../array.rkt")

(provide identity-matrix flidentity-matrix 
         matrix->list list->matrix fllist->matrix
         matrix->vector vector->matrix flvector->matrix
         const-matrix)

(: identity-matrix (Integer -> (lazy-array Real)))
(define (identity-matrix size) (diagonal-array 2 size 1 0))

(: flidentity-matrix (Integer -> (lazy-array Float)))
(define (flidentity-matrix size) (diagonal-array 2 size 1.0 0.0))

(: const-matrix (All (A) (Integer Integer A -> (lazy-array A))))
(define (const-matrix m n x)
  (const-array (list m n) x))

(: list->matrix : (Listof* Number) -> (lazy-array Number))
(define (list->matrix rows)
  (list->array number? rows))

(: fllist->matrix : (Listof* Flonum) -> (lazy-array Flonum))
(define (fllist->matrix rows)
  (list->array flonum? rows))

(: matrix->list : (All (A) (Array A) -> (Listof* A)))
(define (matrix->list a)
  (array->list a))

(: vector->matrix : (Vectorof* Number) -> (lazy-array Number))
(define (vector->matrix rows)
  (vector->array number? rows))

(: flvector->matrix : (Vectorof* Flonum) -> (lazy-array Flonum))
(define (flvector->matrix rows)
  (vector->array flonum? rows))

(: matrix->vector : (All (A) (Array A) -> (Vectorof* A)))
(define (matrix->vector a)
  (array->vector a))

(module* test typed/racket
  (require typed/rackunit 
           (submod "..")
           "../../array.rkt")
  
  (check-equal? (array->list (identity-matrix 1)) '[[1]])
  (check-equal? (array->list (identity-matrix 2)) '[[1 0] [0 1]])
  (check-equal? (array->list (identity-matrix 3)) '[[1 0 0] [0 1 0] [0 0 1]]) 
  (check-equal? (array->list (flidentity-matrix 1)) '[[1.]])
  (check-equal? (array->list (flidentity-matrix 2)) '[[1. 0.] [0. 1.]])
  (check-equal? (array->list (flidentity-matrix 3)) '[[1. 0. 0.] [0. 1. 0.] [0. 0. 1.]])
  
  (check-equal? (array->list (const-matrix 2 3 0)) '((0 0 0) (0 0 0)))
  (check-equal? (array->list (const-matrix 2 3 0.)) '((0. 0. 0.) (0. 0. 0.)))
  
  (check-equal? (matrix->list (list->matrix '((1 2) (3 4)))) '((1 2) (3 4)))
  (check-equal? (matrix->list (fllist->matrix '((1. 2.) (3. 4.)))) '((1. 2.) (3. 4.)))
  
  (check-equal? (matrix->vector (vector->matrix '#(#(1 2) #(3 4)))) '#(#(1 2) #(3 4)))
  (check-equal? (matrix->vector (flvector->matrix '#(#(1. 2.) #(3. 4.)))) '#(#(1. 2.) #(3. 4.))))