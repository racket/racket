#lang racket/base

(define unexported-five 5)
(define s (quote-syntax unexported-five))

(eval-syntax s)
