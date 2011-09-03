#lang typed/racket
(define-type T (All [X Y ...] String))
(: f (All [A] (T -> Any)))
(define f void)
