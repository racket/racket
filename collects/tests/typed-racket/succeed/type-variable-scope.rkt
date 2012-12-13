#lang typed/racket

;; Testing type variable scope
;;
;; These should all succeed

(: f (All (b) (b -> b)))
(define f
  (plambda: (a) ([x : a]) x))

(: g (All (a) (a -> a)))
(define g
  (plambda: (b) ([x : b]) x))

(: h (All (x y) (x -> x)))
(define h
  (plambda: (a b) ([x : a]) x))

(: i (All (x) (x -> x)))
(define i
  (plambda: (b) ([x : b])
    (plambda: (b) ([y : b])
      y)
    x))
