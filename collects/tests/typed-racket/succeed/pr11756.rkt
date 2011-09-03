#lang typed/racket/base

(: test : (All (X) X -> (All (Y) Y -> X)))
(define (test n)
  (: helper : (All (Z) Z -> X))
  (define (helper m) n)
  helper)

; if the below is commented out the code runs without errors
(provide test)
