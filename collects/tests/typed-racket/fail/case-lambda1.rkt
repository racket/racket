#;
(exn-pred 2)
#lang typed/racket
(: f (case->
  (Symbol Symbol *  -> Integer)
  (Symbol * -> Symbol)))
(define f (case-lambda
            ((x . y) 4)
             (w 'x)
        ))

((ann f (Symbol * -> Symbol)) 'x)
