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
      [(_ f)  (syntax/loc stx (unsafe-build-array #() (λ (js) (f))))]
      [(_ f arr-expr)
       (syntax/loc stx
         (let ([arr  (ensure-array 'array-map arr-expr)])
           (define ds (array-shape arr))
           (define proc (unsafe-array-proc arr))
           (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js))))))]
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
               (unsafe-build-array ds (λ: ([js : Indexes]) (f (proc js) (procs js) ...)))))))])))

(require 'syntax-defs)

(module untyped-defs typed/racket/base
  (require "array-struct.rkt"
           "array-broadcast.rkt"
           "utils.rkt"
           (submod ".." syntax-defs))
  
  (provide array-map)
  
  (: array-map (All (R A) (case-> ((-> R) -> (Array R))
                                  ((A -> R) (Array A) -> (Array R))
                                  ((A A A * -> R) (Array A) (Array A) (Array A) * -> (Array R)))))
  (define array-map
    (case-lambda:
      [([f : (-> R)])
       (inline-array-map f)]
      [([f : (A -> R)] [arr : (Array A)])
       (inline-array-map f arr)]
      [([f : (A A -> R)] [arr0 : (Array A)] [arr1 : (Array A)])
       (inline-array-map f arr0 arr1)]
      [([f : (A A A * -> R)] [arr0 : (Array A)] [arr1 : (Array A)] . [arrs : (Array A) *])
       (define ds (array-shape-broadcast (list* (array-shape arr0)
                                                (array-shape arr1)
                                                (map (inst array-shape A) arrs))))
       (let ([arr0  (array-broadcast arr0 ds)]
             [arr1  (array-broadcast arr1 ds)]
             [arrs  (map (λ: ([arr : (Array A)]) (array-broadcast arr ds)) arrs)])
         (define g0 (unsafe-array-proc arr0))
         (define g1 (unsafe-array-proc arr1))
         (define gs (map (inst unsafe-array-proc A) arrs))
         (unsafe-build-array
          ds (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                         (map (λ: ([g : (Indexes -> A)]) (g js)) gs)))))]))
  )

(require 'untyped-defs)
