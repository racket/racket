#lang racket/load

(module m1 racket
  (define (f x y) (equal? x y))
  (provide f))

(module m2 typed/racket
  (require/typed 'm1 [f (Any Any -> Boolean)])
  (f (vector 1 2) (vector 1 2)))

(require 'm2)
