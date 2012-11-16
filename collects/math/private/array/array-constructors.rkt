#lang racket/base

(require typed/untyped-utils)

(require/untyped-contract
 (begin (require "array-struct.rkt")
        (require "utils.rkt"))
 "typed-array-constructors.rkt"
 [make-array        (All (A) ((Vectorof Integer) A -> (Array A)))]
 [axis-index-array  ((Vectorof Integer) Integer -> (Array Index))]
 [index-array       ((Vectorof Integer) -> (Array Index))]
 [indexes-array     ((Vectorof Integer) -> (Array Indexes))])

(require (except-in "typed-array-constructors.rkt"
                    make-array
                    axis-index-array
                    index-array
                    indexes-array))

(provide make-array
         axis-index-array
         index-array
         indexes-array
         diagonal-array)
