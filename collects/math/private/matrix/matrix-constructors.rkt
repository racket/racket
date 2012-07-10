#lang typed/racket/base

(require "../../array.rkt")

(provide identity-matrix flidentity-matrix)

(: identity-matrix (Integer -> (lazy-array Real)))
(define (identity-matrix size) (diagonal-array 2 size 1 0))

(: flidentity-matrix (Integer -> (lazy-array Float)))
(define (flidentity-matrix size) (diagonal-array 2 size 1.0 0.0))

(module* test typed/racket
  (require typed/rackunit 
           (submod "..")
           "../../array.rkt")
  (check-equal? (array->list (identity-matrix 1)) '[[1]])
  (check-equal? (array->list (identity-matrix 2)) '[[1 0] [0 1]])
  (check-equal? (array->list (identity-matrix 3)) '[[1 0 0] [0 1 0] [0 0 1]]) 
  (check-equal? (array->list (flidentity-matrix 1)) '[[1.]])
  (check-equal? (array->list (flidentity-matrix 2)) '[[1. 0.] [0. 1.]])
  (check-equal? (array->list (flidentity-matrix 3)) '[[1. 0. 0.] [0. 1. 0.] [0. 0. 1.]]))
  
  

  

