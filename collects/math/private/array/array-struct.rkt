#lang racket/base

(require typed/untyped-utils
         typed/racket/base
         (for-syntax racket/base syntax/parse)
         "array-syntax.rkt"
         (except-in "typed-array-struct.rkt"
                    build-array
                    build-simple-array
                    list->array))

(require/untyped-contract
 (begin (require (only-in "typed-array-struct.rkt" Array)))
 "typed-array-struct.rkt"
 [build-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))]
 [build-simple-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))]
 [list->array (All (A) (case-> ((Listof A) -> (Array A))
                               ((Vectorof Integer) (Listof A) -> (Array A))))])

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
 array-strictness
 array-strict
 array-strict!
 array-default-strict
 array-default-strict!
 array-strict?
 build-array
 build-simple-array
 list->array
 make-unsafe-array-proc
 unsafe-build-array
 unsafe-build-simple-array
 unsafe-list->array
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
  (syntax-parse stx #:literals (:)
    [(_ e:expr)
     (syntax/loc stx (array/syntax array list unsafe-list->array e))]
    [(_ e:expr : T:expr)
     (syntax/loc stx (array/syntax array (inst list T) unsafe-list->array e))]
    [_:id  (raise-syntax-error 'array "not allowed as an expression" stx)]))

(define-syntax-rule (array-strict arr-expr)
  (let ([arr arr-expr])
    (array-strict! arr)
    arr))

(define-syntax-rule (array-default-strict arr-expr)
  (let ([arr arr-expr])
    (array-default-strict! arr)
    arr))
