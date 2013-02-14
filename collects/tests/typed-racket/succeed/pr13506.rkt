#lang typed/racket

(: fast-sequence->list (All (A) ((Sequenceof A) -> (Listof A))))
(define (fast-sequence->list xs)
  (cond [(list? xs)  xs]
        [(vector? xs)  (vector->list xs)]
        [else  (sequence->list xs)]))
