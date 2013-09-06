#;
(exn-pred #rx"Filter proposition's object index 3 is larger than argument length 1")
#lang typed/racket

;; This test ensures that a filter object like '3' is
;; invalid when the function type only has 1 argument.

(ann (λ (x)
       (define f
         (ann (λ (y) (exact-integer? x))
              (Any -> Boolean : #:+ (Integer @ 3) #:- (! Integer @ x))))
       (if (f 'dummy)
           (add1 x)
           2))
     (Any -> Integer))

