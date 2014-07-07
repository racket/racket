#lang typed/racket
(provide f)
(define-type C (Pair Number (Pair Number C)))
(: f (C -> Boolean))
(define (f x) (list? x))
