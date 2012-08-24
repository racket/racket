#lang typed/racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "../unsafe.rkt"
         "../exception.rkt"
         "../vector/vector.rkt"
         "array-struct.rkt"
         "mutable-array.rkt"
         "utils.rkt")

(provide for/array: for*/array:)

(define-syntax (base-for/array: stx)
  (syntax-parse stx #:literals (:)
    [(_ name:id for/vector:id #:shape ds-expr:expr (~optional (~seq #:fill fill-expr:expr))
        (clause ...) : A:expr body:expr ...+)
     (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())])
       (syntax/loc stx
         (let*: ([ds : User-Indexes  ds-expr]
                 [ds : Indexes  (check-array-shape
                                 ds (λ () (raise-argument-error 'name "Indexes" ds)))])
           (define vs (for/vector #:length (array-shape-size ds) maybe-fill ...
                        (clause ...) : A body ...))
           (unsafe-mutable-array ds vs))))]
    [(_ name:id for/vector:id (clause ...) : A:expr body:expr ...+)
     (syntax/loc stx
       (let ()
         (define vs (for/vector (clause ...) : A body ...))
         (define ds ((inst vector Index) (vector-length vs)))
         (unsafe-mutable-array ds vs)))]))

(define-syntax-rule (for/array: e ...)
  (base-for/array: for/array: for/vector: e ...))

(define-syntax-rule (for*/array: e ...)
  (base-for/array: for*/array: for*/vector: e ...))
