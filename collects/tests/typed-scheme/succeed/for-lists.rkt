#lang typed-scheme

(: f ((Listof Number) -> (Listof Number)))
(define (f x)
  (for/lists (#{y : (Listof Number)}) ([e (in-list x)])
             e))

(for/lists: ([a : (Listof Number)])
  ([e (in-list '(1 2 3))])
  e)
