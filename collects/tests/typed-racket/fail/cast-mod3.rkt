#;
(exn-pred exn:fail:syntax? #rx".*free variables.*")

#lang typed/racket/base

(: f (All (a) (Number -> a)))
(define (f x) (cast x a))
