#lang racket/base

(require typed/untyped-utils
         typed/racket/base
         (for-syntax racket/base syntax/parse)
         "array-syntax.rkt"
         (except-in "typed-mutable-array.rkt"
                    vector->array))

(require/untyped-contract
 (begin (require (only-in "typed-mutable-array.rkt" Mutable-Array)))
 "typed-mutable-array.rkt"
 [vector->array  (All (A) (case-> ((Vectorof A) -> (Mutable-Array A))
                                  ((Vectorof Integer) (Vectorof A) -> (Mutable-Array A))))])

(provide
 ;; Mutable-Array
 Mutable-Array
 mutable-array?
 mutable-array-data
 vector->array
 unsafe-vector->array
 mutable-array-copy
 mutable-array
 ;; Conversion
 array->mutable-array)

(define-syntax (mutable-array stx)
  (syntax-parse stx #:literals (:)
    [(_ e:expr)
     (syntax/loc stx (array/syntax mutable-array vector unsafe-vector->array e))]
    [(_ e:expr : T:expr)
     (syntax/loc stx (array/syntax mutable-array (inst vector T) unsafe-vector->array e))]
    [_:id  (raise-syntax-error 'mutable-array "not allowed as an expression" stx)]))
