#lang typed/scheme
(: lon? (Any -> Boolean : (Listof Number)))
(define (lon? x)
  (and (list? x) (andmap number? x)))
