#lang racket/base

(provide FlArray
         unsafe-flarray
         (rename-out [flarray/syntax flarray])
         array->flarray
         flarray-data)

(module defs typed/racket/base
  
  (require "../../flonum.rkt"
           "../unsafe.rkt"
           "array-struct.rkt"
           "utils.rkt"
           "for-each.rkt")
  
  (provide (all-defined-out))
  
  (struct: (A) flarray Settable-Array ([data : FlVector])
    #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'flarray port mode)))
  
  (define-type FlArray (flarray Float))
  
  (: unsafe-flarray (Indexes FlVector -> FlArray))
  (define (unsafe-flarray ds vs)
    (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-flvector-ref vs j))))
    (define set-proc (make-unsafe-array-set-proc Float ds (λ (j v) (unsafe-flvector-set! vs j v))))
    (flarray ds (flvector-length vs) (box #t) void proc set-proc vs))
  
  (: unsafe-vector->flarray (Indexes (Vectorof Real) -> FlArray))
  (define (unsafe-vector->flarray ds vs)
    (define size (vector-length vs))
    (define xs
      (build-flvector size (λ: ([j : Index]) (real->double-flonum (unsafe-vector-ref vs j)))))
    (unsafe-flarray ds xs))
  
  (: array->flarray ((Array Real) -> FlArray))
  (define (array->flarray arr)
    (define ds (array-shape arr))
    (define size (array-size arr))
    (define proc (unsafe-array-proc arr))
    (define vs (make-flvector size))
    (for-each-array+data-index ds (λ (js j) (unsafe-flvector-set!
                                             vs j (real->double-flonum (proc js)))))
    (unsafe-flarray ds vs))
  
  )  ; module defs

(require 'defs
         typed/racket/base
         (for-syntax racket/base
                     syntax/parse)
         "array-syntax.rkt")

(define-syntax (flarray/syntax stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax flarray (inst vector Real) unsafe-vector->flarray e))]
    [_:id  (raise-syntax-error 'flarray "not allowed as an expression" stx)]))
