#lang typed/racket

(let: ((x : Integer 3)
       (y 4))
      (+ x y))

(let: ((x 3) (y 4))
      (+ x y))

(let*: ((x 3)
        (y : Integer (+ x 1)))
       (+ x y))

(letrec: ((x 3)
          (y : (Integer -> Integer) (lambda (x) (y x))))
         x)
