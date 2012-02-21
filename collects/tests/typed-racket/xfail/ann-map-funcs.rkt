#lang typed/racket

;; This test is similar to unholy-terror.
;; Recent changes to the numeric tower makes this generate a ridiculous amount
;; of inference constraints.
;; Changes to the inference engine breaks inference on this test, but unlike
;; unholy terror, this one fails quite fast, so it's not as bad.
;; Reverting these changes would make inference work again, given enough time
;; and memory (on the order of hours, and gigabytes of memory).
;; For future reference, ~600k inference constraints were generated (due to a
;; repeated cross product) before I killed it.

(: map-with-funcs (All (b a ...) ((a ... a -> b) * -> (a ... a -> (Listof b)))))

(define (map-with-funcs . fs)
 (lambda as
   (map (lambda: ([f : (a ... a -> b)])
          (apply f as))
        fs)))

(ann (map-with-funcs + - * /) (Number Number * -> (Listof Number)))
