#lang typed/racket

;; Test imports from racket/function

(identity 5)
(identity (lambda (x) "test"))

((const 'foo) 1 2 3)

(filter (negate symbol?) '(1 a 2 b 3 c))
(map (negate equal?) '(1 2 3) '(1 1 1))

((curry (lambda (x y) (cons x y)) 5) '())
