#lang racket/base

(require typed/untyped-utils
         (except-in "typed-array-indexing.rkt"
                    array-ref
                    array-set!
                    array-indexes-ref
                    array-indexes-set!))

(require/untyped-contract
 (begin (require (only-in "array-struct.rkt" Array Settable-Array)))
 "typed-array-indexing.rkt"
 [array-ref   (All (A) ((Array A) (Vectorof Integer) -> A))]
 [array-set!  (All (A) ((Settable-Array A) (Vectorof Integer) A -> Void))]
 [array-indexes-ref   (All (A) ((Array A) (Array (Vectorof Integer)) -> (Array A)))]
 [array-indexes-set!  (All (A) ((Settable-Array A) (Array (Vectorof Integer)) (Array A) -> Void))])

(provide
 array-ref
 array-set!
 unsafe-array-ref
 unsafe-array-set!
 ;; Indexing by array of indexes
 array-indexes-ref
 array-indexes-set!
 ;; Slicing
 (rename-out [-Slice Slice]
             [-Slice-Dots Slice-Dots]
             [-Slice-New-Axis Slice-New-Axis])
 Slice-Spec
 :: slice? slice-start slice-end slice-step slice->range-values
 ::... slice-dots?
 ::new slice-new-axis? slice-new-axis-length
 array-slice-ref
 array-slice-set!)
