#lang typed-scheme

(: f (All (a) (a -> a)))
(define (f x)
 (: g (All (b) (a (Listof a) -> (Listof a))))
 (define (g x y) y)
 (g "foo" (list "foo")))

(f 3)
