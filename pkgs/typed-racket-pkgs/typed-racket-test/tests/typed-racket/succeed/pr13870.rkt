#lang typed/racket/base

;; Test for PR 13870

(require racket/match)

(: sum ((Listof Integer) -> Integer))
(define (sum l)
  (match l
    [(list) 0]
    [(list x xs ...) (+ x (sum xs))]))

(sum (list 1 2 3))
