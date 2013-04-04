#lang typed/racket
(: foo
   (case-> 
     (Number -> Number)
     (Number String -> Number)
     (Number String String * -> Number)))
(define (foo x (s1 "") . s) x)
