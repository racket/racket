#lang typed/scheme

(ann (for/list ([z #"foobar"]) (add1 z)) (Listof Integer))

(: explode (String -> (Listof Char)))
(define (explode s)
  (for/list ([i s]) i))

(ann 
  (for*/list: : (Listof Natural)
    ((x : Natural (list 1 2 4))
     (y : Natural (list 2 3 4)))
    (+ x y))
  (Listof Natural))
