#lang typed-scheme

(: z (All (A) (A -> ((Listof A) A -> (Listof A)))))
(define (z _) (lambda (x y) (cons y x)))
(define zz (z cons))
