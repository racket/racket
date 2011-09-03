#;
(exn-pred 2)
#lang typed/racket
(: bar : (String -> String))
(: bar : (Number -> Number))
(define (bar x)
  (+ x 1))


(define: (foo) : Number
  (: bar : (Number -> Number))
  (define: (bar [x : Number]) : Number
    (+ x 1))
  (bar 5))


(: baz Number)
(define: baz : Number 7)
