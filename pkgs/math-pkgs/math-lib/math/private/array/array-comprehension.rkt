#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         typed/racket/base
         "mutable-array.rkt"
         "utils.rkt")

(provide for/array:
         for*/array:
         for/array
         for*/array)

(define-syntax (base-for/array: stx)
  (syntax-parse stx #:literals (:)
    [(_ name:id for/vector:id #:shape ds-expr:expr (~optional (~seq #:fill fill-expr:expr))
        (clause ...) (~optional (~seq : A:expr)) body:expr ...+)
     (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())]
                   [(maybe-type ...)  (if (attribute A) #'(: A) #'())])
       (syntax/loc stx
         (let*: ([ds : In-Indexes  ds-expr]
                 [ds : Indexes  (check-array-shape
                                 ds (λ () (raise-argument-error 'name "Indexes" ds)))])
           (define vs (for/vector #:length (array-shape-size ds) maybe-fill ...
                        (clause ...) maybe-type ... body ...))
           (unsafe-vector->array ds vs))))]
    [(_ name:id for/vector:id (clause ...) (~optional (~seq : A:expr)) body:expr ...+)
     (with-syntax ([(maybe-type ...)  (if (attribute A) #'(: A) #'())])
       (syntax/loc stx
         (let ()
           (define vs (for/vector (clause ...) maybe-type ... body ...))
           (define ds ((inst vector Index) (vector-length vs)))
           (unsafe-vector->array ds vs))))]))

(define-syntax-rule (for/array: e ...)
  (base-for/array: for/array: for/vector: e ...))

(define-syntax-rule (for*/array: e ...)
  (base-for/array: for*/array: for*/vector: e ...))

(define-syntax (base-for/array stx)
  (syntax-parse stx
    [(_ name:id for/vector:id #:shape ds-expr:expr (~optional (~seq #:fill fill-expr:expr))
        (clause ...) body:expr ...+)
     (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())])
       (syntax/loc stx
         (let* ([ds  ds-expr]
                [ds  (check-array-shape
                      ds (λ () (raise-argument-error 'name "Indexes" ds)))])
           (define vs (for/vector #:length (array-shape-size ds) maybe-fill ...
                        (clause ...) body ...))
           (unsafe-vector->array ds vs))))]
    [(_ name:id for/vector:id (clause ...) body:expr ...+)
     (syntax/loc stx
       (let ()
         (define vs (for/vector (clause ...) body ...))
         (define ds ((inst vector Index) (vector-length vs)))
         (unsafe-vector->array ds vs)))]))

(define-syntax-rule (for/array e ...)
  (base-for/array for/array for/vector e ...))

(define-syntax-rule (for*/array e ...)
  (base-for/array for*/array for*/vector e ...))
