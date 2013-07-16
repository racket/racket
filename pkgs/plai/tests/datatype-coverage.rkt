#lang plai
;; RE: PR10485
(define-type toy
  [airplane (lift number?) (name string?)])

(type-case toy (airplane 3412 "the bat!")
  [airplane (lift name) (+ lift 13)])

;; The body of this function should be the only red in the file
(define (f x)
  (+ 1 1))
