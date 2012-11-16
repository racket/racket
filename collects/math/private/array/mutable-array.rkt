#lang racket/base

(require typed/untyped-utils
         (for-syntax racket/base syntax/parse)
         "array-syntax.rkt"
         (except-in "typed-mutable-array.rkt"
                    make-mutable-array))

(require/untyped-contract
 (begin (require "typed-mutable-array.rkt"))
 "typed-mutable-array.rkt"
 [make-mutable-array  (All (A) ((Vectorof Integer) (Vectorof A) -> (Mutable-Array A)))])

(provide
 ;; Mutable-Array
 Mutable-Array
 mutable-array?
 mutable-array-data
 make-mutable-array
 unsafe-mutable-array
 mutable-array-copy
 mutable-array
 ;; Conversion
 array->mutable-array
 array-strict
 flat-vector->matrix)

(define-syntax (mutable-array stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax mutable-array vector make-mutable-array e))]
    [(_ e:expr T:expr)
     (syntax/loc stx (array/syntax mutable-array (inst vector T) make-mutable-array e))]
    [_:id  (raise-syntax-error 'mutable-array "not allowed as an expression" stx)]))
