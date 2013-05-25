#lang typed/racket

(: sum ((Listof Flonum) -> Flonum))
(define (sum ws) (apply + ws))

(ann (sum '()) Flonum)
