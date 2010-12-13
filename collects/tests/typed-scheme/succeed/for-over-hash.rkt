#lang typed/racket

(: v : (Listof Number))
(define v (for/list ([(k v) (make-hash (list (cons 1 2) (cons 3 4)))])
            (+ k v)))
