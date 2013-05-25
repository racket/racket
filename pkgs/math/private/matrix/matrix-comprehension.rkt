#lang racket/base

(require (for-syntax racket/base
                     syntax/parse)
         "../array/array-comprehension.rkt")

(provide for/matrix:
         for*/matrix:
         for/matrix
         for*/matrix)

(module typed-defs typed/racket/base
  (require (for-syntax racket/base
                       syntax/parse)
           "../array/array-comprehension.rkt")
  
  (provide (all-defined-out))
  
  (: ensure-matrix-dims (Symbol Any Any -> (Values Positive-Index Positive-Index)))
  (define (ensure-matrix-dims name m n)
    (cond [(or (not (index? m)) (zero? m))  (raise-argument-error name "Positive-Index" 0 m n)]
          [(or (not (index? n)) (zero? n))  (raise-argument-error name "Positive-Index" 1 m n)]
          [else  (values m n)]))
  
  (define-syntax (base-for/matrix: stx)
    (syntax-parse stx #:literals (:)
      [(_ name:id for/array:id
          m-expr:expr n-expr:expr
          (~optional (~seq #:fill fill-expr:expr))
          (clause ...)
          (~optional (~seq : A:expr))
          body:expr ...+)
       (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())]
                     [(maybe-type ...)  (if (attribute A) #'(: A) #'())])
         (syntax/loc stx
           (let-values ([(m n)  (ensure-matrix-dims 'name
                                                    (ann m-expr Integer)
                                                    (ann n-expr Integer))])
             (for/array #:shape (vector m-expr n-expr) maybe-fill ... (clause ...) maybe-type ...
               body ...))))]))
  
  (define-syntax-rule (for/matrix:  e ...) (base-for/matrix: for/matrix:  for/array:  e ...))
  (define-syntax-rule (for*/matrix: e ...) (base-for/matrix: for*/matrix: for*/array: e ...))
  
  )

(require (submod "." typed-defs))

(define-syntax (base-for/matrix stx)
  (syntax-parse stx
    [(_ name:id for/array:id
        m-expr:expr n-expr:expr
        (~optional (~seq #:fill fill-expr:expr))
        (clause ...)
        body:expr ...+)
     (with-syntax ([(maybe-fill ...)  (if (attribute fill-expr) #'(#:fill fill-expr) #'())])
       (syntax/loc stx
         (let-values ([(m n)  (ensure-matrix-dims 'name m-expr n-expr)])
           (for/array #:shape (vector m-expr n-expr) maybe-fill ... (clause ...)
             body ...))))]))

(define-syntax-rule (for/matrix  e ...) (base-for/matrix for/matrix  for/array  e ...))
(define-syntax-rule (for*/matrix e ...) (base-for/matrix for*/matrix for*/array e ...))
