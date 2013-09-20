#;
(exn:pred #rx"Argument to Struct must be a structure")
#lang typed/racket

;; Make sure `Struct` constructor rejects bad arguments
(: x (Struct Integer))
(define x 3)

