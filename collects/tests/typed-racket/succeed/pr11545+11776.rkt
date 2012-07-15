#lang typed/racket

(: stuff (All [X ...] (X ... X -> (Listof Any))))
(define (stuff . xs) xs)

(: thing (-> (Listof Any)))
(define (thing)
  ((inst stuff)))

(inst values Any)
