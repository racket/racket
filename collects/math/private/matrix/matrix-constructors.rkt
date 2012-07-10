#lang typed/racket/base

(require "../../array.rkt")

(provide identity-matrix flidentity-matrix)

(: identity-matrix (Integer -> (lazy-array Real)))
(define (identity-matrix size) (diagonal-array 2 size 1 0))

(: flidentity-matrix (Integer -> (lazy-array Float)))
(define (flidentity-matrix size) (diagonal-array 2 size 1.0 0.0))
