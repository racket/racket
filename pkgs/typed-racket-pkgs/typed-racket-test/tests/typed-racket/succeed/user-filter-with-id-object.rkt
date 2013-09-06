#lang typed/racket

;; This test makes sure that a user written filter
;; can reference an identifier object in addition to
;; an integer object.

(ann (λ (x)
       (define f
         (ann (λ (y) (exact-integer? x))
              ;; note the filters
              (Any -> Boolean : #:+ (Integer @ x) #:- (! Integer @ x))))
       (if (f 'dummy)
           (add1 x)
           2))
     (Any -> Integer))

