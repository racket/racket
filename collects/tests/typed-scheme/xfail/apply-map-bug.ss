
#lang typed-scheme

(: add-lists ((Listof Number) (Listof Number) * -> (Listof Number)))
(define (add-lists lst . lsts)
 (apply map #{+ :: (Number Number * -> Number)} lst lsts))

(add-lists '(1 2 3) '(4 5 6) '(7 8 9))
