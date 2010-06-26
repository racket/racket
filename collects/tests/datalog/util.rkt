#lang racket
(require rackunit
         datalog/ast)

(provide test-literal test-clause)

(define (test-literal str l1 l2)
  (test-case 
   str (check literal-equal? l1 l2)))
(define (test-clause str c1 c2)
  (test-case 
   str (check clause-equal? c1 c2)))
