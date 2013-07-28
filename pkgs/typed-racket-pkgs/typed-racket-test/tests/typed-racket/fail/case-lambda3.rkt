#;
(exn-pred 1)
#lang typed/racket


(: f (case->
       (String -> String)
       (String String -> String)
       (String Symbol * -> String)))
(define f
  (case-lambda
    ((x) x)
    ((x y) y)
    ((x . w) x)))

(f "x" 'y)
