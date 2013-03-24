#;
(exn-pred 1)
#lang typed/racket


(: f (case->
       (Symbol * -> String)))
(define f
  (case-lambda
    ((x . w) "hello")))

(f 'x 'y)
