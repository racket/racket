#lang typed-scheme

(require scheme/match)

(: post-eval : -> Number)
;; evaluates a postfix sequence of items, using a stack
(define (post-eval)
  (match '(1 2)
    [(list (? number? #{stack : (Listof Number)}) ...) 3]))
