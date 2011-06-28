#lang typed/scheme

(define-struct: foo [(bar : Integer)])

(: f (foo -> Integer))
(define (f x)
  (if (zero? (foo-bar x))
      (error 'f "Nooooooo!")
      (foo-bar x)))
