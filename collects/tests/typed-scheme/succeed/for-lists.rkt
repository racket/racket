#lang typed-scheme

(: f ((Listof Number) -> (Listof Number)))
(define (f x)
  (for/lists (#{y : (Listof Number)}) ([e (in-list x)])
             e))
