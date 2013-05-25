#lang racket/base

(provide FCArray
         unsafe-fcarray
         (rename-out [fcarray/syntax fcarray])
         array->fcarray
         fcarray-real-data
         fcarray-imag-data)

(module defs typed/racket/base
  
  (require "../../flonum.rkt"
           "../unsafe.rkt"
           "array-struct.rkt"
           "utils.rkt"
           "for-each.rkt")
  
  (provide (all-defined-out))
  
  (struct: (A) fcarray Settable-Array ([real-data : FlVector] [imag-data : FlVector])
    #:property prop:custom-write (λ (arr port mode) ((array-custom-printer) arr 'fcarray port mode)))
  
  (define-type FCArray (fcarray Float-Complex))
  
  (: unsafe-fcarray (Indexes FlVector FlVector -> FCArray))
  (define (unsafe-fcarray ds xs ys)
    (define proc
      (make-unsafe-array-proc
       ds (λ (j) (make-rectangular (unsafe-flvector-ref xs j) (unsafe-flvector-ref ys j)))))
    (define set-proc
      (make-unsafe-array-set-proc
       Float-Complex ds (λ (j v)
                          (unsafe-flvector-set! xs j (real-part v))
                          (unsafe-flvector-set! ys j (imag-part v)))))
    (fcarray ds (flvector-length xs) (box #t) void proc set-proc xs ys))
  
  (: unsafe-vector->fcarray (Indexes (Vectorof Number) -> FCArray))
  (define (unsafe-vector->fcarray ds zs)
    (define size (vector-length zs))
    (define xs (build-flvector size (λ: ([j : Index]) (fl (real-part (unsafe-vector-ref zs j))))))
    (define ys (build-flvector size (λ: ([j : Index]) (fl (imag-part (unsafe-vector-ref zs j))))))
    (unsafe-fcarray ds xs ys))
  
  (: array->fcarray ((Array Number) -> FCArray))
  (define (array->fcarray arr)
    (define ds (array-shape arr))
    (define size (array-size arr))
    (define proc (unsafe-array-proc arr))
    (define xs (make-flvector size))
    (define ys (make-flvector size))
    (for-each-array+data-index
     ds (λ (js j)
          (define z (proc js))
          (unsafe-flvector-set! xs j (fl (real-part z)))
          (unsafe-flvector-set! ys j (fl (imag-part z)))))
    (unsafe-fcarray ds xs ys))
  
  )  ; module defs

(require 'defs
         typed/racket/base
         (for-syntax racket/base
                     syntax/parse)
         "array-syntax.rkt")

(define-syntax (fcarray/syntax stx)
  (syntax-parse stx
    [(_ e:expr)
     (syntax/loc stx (array/syntax fcarray (inst vector Number) unsafe-vector->fcarray e))]
    [_:id  (raise-syntax-error 'fcarray "not allowed as an expression" stx)]))
