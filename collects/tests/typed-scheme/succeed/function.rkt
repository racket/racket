#lang typed/racket

;; Test imports from racket/function

(identity 5)
(identity (lambda (x) "test"))

((const 'foo) 1 2 3)

((curry (lambda (x y) (cons x y)) 5) '())
