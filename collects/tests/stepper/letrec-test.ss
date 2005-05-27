(define a 15)

(define (test c)
  (letrec ([a (+ c 13)]
           [b (+ a 90)])
    (+ a b)))

(test 13)
(test 14)
