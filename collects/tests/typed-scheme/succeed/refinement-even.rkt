#lang typed-scheme

(declare-refinement even?)
(define-type-alias Even (Refinement even?))

(: x Integer)
(define x 4)

(: y Even)
(define y (if (even? x) x (error 'bad)))

(: f (Even -> String))
(define (f e) (format "~a" e))

(f y)
