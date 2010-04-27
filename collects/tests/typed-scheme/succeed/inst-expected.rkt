#lang typed-scheme

(: id (All (a) (a -> a)))
(define (id x) x)

(: f (String -> String))
(define f #{id @ String})
