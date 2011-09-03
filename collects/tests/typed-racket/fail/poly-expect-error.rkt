#;
(exn-pred "Boolean")
#lang typed-scheme

(: f (All (A) (case-lambda (String -> Boolean) (A -> Boolean))))
(define (f x) #t)

(: x Number)
(define x (f 3))
