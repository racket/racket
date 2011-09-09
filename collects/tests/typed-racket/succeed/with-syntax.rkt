#lang typed/racket
(require racket/syntax)

(: f : -> Syntax)
(define (f)
  (with-syntax* ([(x ...) (list 1 2 3)])
    #`(#,(syntax +) x ...)))
