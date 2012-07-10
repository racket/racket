#lang typed/racket/base

(require math/array)

(provide (all-defined-out))

(: array-matrix? (All (A) ((Array A) -> Boolean)))
(define (array-matrix? x)
  (= 2 (array-dims x)))
