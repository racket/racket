#lang racket
(require "ex-matthias-a.rkt")

(define memory (new memory%))

(define a (send memory malloc))
(send memory free a) (displayln `(freeing ,a))
(send memory free a) (displayln `(freeing ,a))
(send memory free a) (displayln `(freeing ,a))