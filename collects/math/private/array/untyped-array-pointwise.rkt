#lang racket/base

(provide inline-array-map array-map)

(module syntax-defs racket/base
  (require (for-syntax racket/base)
           typed/racket/base
           "array-struct.rkt"
           "array-broadcast.rkt"
           "utils.rkt")
  
  (provide inline-array-map)
  
  (define-syntax-rule (ensure-array name arr-expr)
    (plet: (A) ([arr : (Array A)  arr-expr])
      (if (array? arr) arr (raise-argument-error name "Array" arr))))
  
  (define-syntax (inline-array-map stx)
    (syntax-case stx ()
      [(_ f)
       (syntax/loc stx
         (array-default-strict
          (unsafe-build-array #() (λ (js) (f)))))]
      [(_ f arr-expr)
       (syntax/loc stx
         (let ([arr  (ensure-array 'array-map arr-expr)])
           (define ds (array-shape arr))
           (define proc (unsafe-array-proc arr))
           (array-default-strict
            (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js)))))))]
      [(_ f arr-expr arr-exprs ...)
       (with-syntax ([(arrs ...)   (generate-temporaries #'(arr-exprs ...))]
                     [(procs ...)  (generate-temporaries #'(arr-exprs ...))])
         (syntax/loc stx
           (let ([arr   (ensure-array 'array-map arr-expr)]
                 [arrs  (ensure-array 'array-map arr-exprs)] ...)
             (define ds (array-shape-broadcast (list (array-shape arr) (array-shape arrs) ...)))
             (let ([arr   (array-broadcast arr ds)]
                   [arrs  (array-broadcast arrs ds)] ...)
               (define proc  (unsafe-array-proc arr))
               (define procs (unsafe-array-proc arrs)) ...
               (array-default-strict
                (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js) (procs js) ...))))))))])))

(require 'syntax-defs)

(module untyped-defs racket/base
  (require racket/contract
           "array-struct.rkt"
           "array-broadcast.rkt"
           "utils.rkt"
           (submod ".." syntax-defs))
  
  (provide (contract-out
            [array-map  (->i ([f (unconstrained-domain-> any/c)])
                             #:rest [xs (listof array?)]
                             #:pre/name (f xs)
                             "function has the wrong arity"
                             (procedure-arity-includes? f (length xs))
                             [_ array?])]))
  
  (define array-map
    (case-lambda
      [(f)  (inline-array-map f)]
      [(f arr)  (inline-array-map f arr)]
      [(f arr0 arr1)  (inline-array-map f arr0 arr1)]
      [(f arr0 arr1 . arrs)
       (define ds (array-shape-broadcast (list* (array-shape arr0)
                                                (array-shape arr1)
                                                (map array-shape arrs))))
       (let ([arr0  (array-broadcast arr0 ds)]
             [arr1  (array-broadcast arr1 ds)]
             [arrs  (map (λ (arr) (array-broadcast arr ds)) arrs)])
         (define g0 (unsafe-array-proc arr0))
         (define g1 (unsafe-array-proc arr1))
         (define gs (map unsafe-array-proc arrs))
         (array-default-strict
          (unsafe-build-array
           ds (λ (js) (apply f (g0 js) (g1 js) (map (λ (g) (g js)) gs))))))]))
  )

(require 'untyped-defs)
