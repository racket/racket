#lang racket/load

;; Test that variable redefinition works at the top-level

(require typed/racket)
(: x Integer)
(define x 3)
(define x 5)

