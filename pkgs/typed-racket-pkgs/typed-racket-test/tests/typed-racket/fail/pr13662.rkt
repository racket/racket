#;
(exn-pred #rx"contract violation" #rx"blaming: untyped")
#lang racket/load

(module typed typed/racket
  (provide b g)

  (: b (Boxof (All (a) (a -> a))))
  (define b ((inst box (All (a) (a -> a))) (lambda (a) a)))
  
  (: g (Integer -> Integer))
  (define (g x)
    ((unbox b) x)))

(module untyped racket
  (require 'typed)
  (set-box! b (lambda (x) "hello"))
  (g 5))

(require 'untyped)
