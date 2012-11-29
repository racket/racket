#lang typed/racket/base

(require "array-struct.rkt"
         "array-fold.rkt"
         "array-pointwise.rkt")

(provide array-count)

(: array-count
   (All (A B T ...)
        (case-> ((A -> Any) (Array A) -> Index)
                ((A B T ... T -> Any) (Array A) (Array B) (Array T) ... T -> Index))))
(define array-count
  (case-lambda:
    [([f : (A -> Any)] [arr0 : (Array A)])
     (assert (array-all-sum (inline-array-map (λ: ([a : A]) (if (f a) 1 0)) arr0) 0) index?)]
    [([f : (A B -> Any)] [arr0 : (Array A)] [arr1 : (Array B)])
     (assert
      (array-all-sum (inline-array-map (λ: ([a : A] [b : B]) (if (f a b) 1 0)) arr0 arr1) 0)
      index?)]
    [([f : (A B T ... T -> Any)] [arr0 : (Array A)] [arr1 : (Array B)] . [arrs : (Array T) ... T])
     (assert
      (array-all-sum (apply array-map
                            (λ: ([a : A] [b : B] . [ts : T ... T]) (if (apply f a b ts) 1 0))
                            arr0 arr1 arrs)
                     0)
      index?)]))
