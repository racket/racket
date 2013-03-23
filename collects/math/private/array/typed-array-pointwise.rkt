#lang typed/racket/base

(require "array-struct.rkt"
         "array-broadcast.rkt"
         "utils.rkt"
         (only-in "untyped-array-pointwise.rkt" inline-array-map))

(provide array-map)

(: array-map (All (R A B T ...)
                  (case-> ((-> R) -> (Array R))
                          ((A -> R) (Array A) -> (Array R))
                          ((A B T ... T -> R) (Array A) (Array B) (Array T) ... T -> (Array R)))))
(define array-map
  (case-lambda:
    [([f : (-> R)])
     (inline-array-map f)]
    [([f : (A -> R)] [arr : (Array A)])
     (inline-array-map f arr)]
    [([f : (A B -> R)] [arr0 : (Array A)] [arr1 : (Array B)])
     (inline-array-map f arr0 arr1)]
    [([f : (A B T ... T -> R)] [arr0 : (Array A)] [arr1 : (Array B)] . [arrs : (Array T) ... T])
     (define ds (array-shape-broadcast (list* (array-shape arr0)
                                              (array-shape arr1)
                                              (map array-shape arrs))))
     (let ([arr0  (array-broadcast arr0 ds)]
           [arr1  (array-broadcast arr1 ds)]
           [arrs  (map (plambda: (S) ([arr : (Array S)]) (array-broadcast arr ds)) arrs)])
       (define g0 (unsafe-array-proc arr0))
       (define g1 (unsafe-array-proc arr1))
       (define gs (map unsafe-array-proc arrs))
       (array-default-strict
        (unsafe-build-array
         ds (λ: ([js : Indexes]) (apply f (g0 js) (g1 js)
                                        (map (plambda: (S) ([g : (Indexes -> S)]) (g js)) gs))))))]))
