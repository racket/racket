#lang racket/base

(require typed/untyped-utils
         (except-in "typed-array-transform.rkt"
                    array-transform
                    array-reshape))

(require/untyped-contract
 (begin (require "array-struct.rkt"
                 "utils.rkt"))
 "typed-array-transform.rkt"
 [array-transform  (All (A) ((Array A) (Vectorof Integer) (Indexes -> (Vectorof Integer))
                                       -> (Array A)))]
 [array-reshape    (All (A) ((Array A) (Vectorof Integer) -> (Array A)))])

(provide array-transform
         unsafe-array-transform
         array-axis-permute
         array-axis-swap
         array-axis-insert
         array-axis-ref
         array-reshape
         array-flatten
         array-append*)
