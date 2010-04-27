#lang typed-scheme

(: cross1 ((Listof Number) -> (Listof Number)))
(define (cross1 m)
  (map (lambda: ([m1 : Number]) #{(error 'bad) :: Number}) m))
