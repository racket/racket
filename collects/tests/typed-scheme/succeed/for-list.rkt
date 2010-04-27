#lang typed/scheme

(ann (for/list ([z #"foobar"]) (add1 z)) (Listof Integer))

(: explode (String -> (Listof Char)))
(define (explode s)
  (for/list ([i s]) i))

