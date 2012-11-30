#lang racket/base

(require typed/untyped-utils
         typed/racket/base
         (for-syntax racket/base syntax/parse)
         "array-syntax.rkt"
         (except-in "typed-array-struct.rkt" build-array build-strict-array))

(require/untyped-contract
 (begin (require "typed-array-struct.rkt"))
 "typed-array-struct.rkt"
 [build-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))]
 [build-strict-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))])

(define-syntax array? (make-rename-transformer #'Array?))
(define-syntax array-shape (make-rename-transformer #'Array-shape))
(define-syntax array-size (make-rename-transformer #'Array-size))
(define-syntax unsafe-array-proc (make-rename-transformer #'Array-unsafe-proc))

(provide
 ;; Array
 Array
 array?
 array-shape
 array-dims
 array-size
 array-strict
 array-strict!
 array-strict?
 make-unsafe-array-proc
 build-array
 unsafe-build-array
 unsafe-build-strict-array
 unsafe-array-proc
 array-lazy
 array
 ;; Settable-Array
 Settable-Array
 settable-array?
 unsafe-settable-array-set-proc
 make-unsafe-array-set-proc
 ;; Printing
 print-array-fields
 array-custom-printer
 ;; Misc
 array-lift-comparison)

(define-syntax (array stx)
  (syntax-parse stx
    [(_ e:expr)  (syntax/loc stx (array/syntax array list flat-list->array e))]
    [_:id  (raise-syntax-error 'array "not allowed as an expression" stx)]))

(define-syntax-rule (array-strict arr-expr)
  (let ([arr arr-expr])
    (array-strict! arr)
    arr))
