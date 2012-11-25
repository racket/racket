#lang racket/base

(require (for-syntax racket/base)
         typed/untyped-utils
         typed-racket/base-env/prims
         racket/unsafe/ops
         "array-struct.rkt"
         "utils.rkt"
         (except-in "typed-array-sequence.rkt" in-array-indexes))

(require/untyped-contract
 "typed-array-sequence.rkt"
 [in-array-indexes  ((Vectorof Integer) -> (Sequenceof (Vectorof Index)))])

(provide (rename-out [in-array-clause  in-array]
                     [in-array-indexes-clause  in-array-indexes]
                     [in-unsafe-array-indexes-clause  in-unsafe-array-indexes])
         in-array-axis
         array->array-list
         array-list->array)

(define-sequence-syntax in-array-clause
  (λ () #'in-array)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ arr-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js proc)
             (plet: (A) ([arr : (Array A)  arr-expr])
               (cond [(array? arr)
                      (define ds (array-shape arr))
                      (define dims (vector-length ds))
                      (define size (array-size arr))
                      (define proc (unsafe-array-proc arr))
                      (define: js : Indexes (make-vector dims 0))
                      (values ds size dims js proc)]
                     [else
                      (raise-argument-error 'in-array "Array" arr)]))])
           (void)
           ([j 0])
           (unsafe-fx< j size)
           ([(x)  (proc js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (unsafe-fx+ j 1))])])]
      [[_ clause] (raise-syntax-error 'in-array "expected (in-array <Array>)" #'clause #'clause)])))

(define-sequence-syntax in-array-indexes-clause
  (λ () #'in-array-indexes)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ ds-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js)
             (let*: ([ds : In-Indexes  ds-expr]
                     [ds : Indexes  (check-array-shape
                                     ds (λ () (raise-argument-error 'in-array-indexes "Indexes"
                                                                         ds)))])
               (define dims (vector-length ds))
               (define size (array-shape-size ds))
               (cond [(index? size)  (define: js : Indexes (make-vector dims 0))
                                     (values ds size dims js)]
                     [else  (error 'in-array-indexes
                                   "array size ~e (for shape ~e) is too large (is not an Index)"
                                   size ds)]))])
           (void)
           ([j 0])
           (unsafe-fx< j size)
           ([(x)  (vector-copy-all js)])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (unsafe-fx+ j 1))])])]
      [[_ clause]
       (raise-syntax-error 'in-array-indexes "expected (in-array-indexes <Indexes>)"
                           #'clause #'clause)])))

(define-sequence-syntax in-unsafe-array-indexes-clause
  (λ () #'in-array-indexes)
  (λ (stx)
    (syntax-case stx ()
      [[(x) (_ ds-expr)]
       (syntax/loc stx
         [(x)
          (:do-in
           ([(ds size dims js)
             (let: ([ds : Indexes  ds-expr])
               (define dims (vector-length ds))
               (define size (array-shape-size ds))
               (cond [(index? size)  (define: js : Indexes (make-vector dims 0))
                                     (values ds size dims js)]
                     [else  (error 'in-array-indexes
                                   "array size ~e (for shape ~e) is too large (is not an Index)"
                                   size ds)]))])
           (void)
           ([j 0])
           (unsafe-fx< j size)
           ([(x)  js])
           #true
           #true
           [(begin (next-indexes! ds dims js)
                   (unsafe-fx+ j 1))])])]
      [[_ clause]
       (raise-syntax-error 'in-array-indexes "expected (in-unsafe-array-indexes <Indexes>)"
                           #'clause #'clause)])))
