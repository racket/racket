#;
(exn-pred 2)
#lang typed/racket

(: f (Symbol Symbol * -> Symbol))
(: g (All (A ...) (Symbol A ... A -> Symbol)))
(define (f x y) x)
(define (g x y) x)
