#lang scheme/load

(module bar scheme
  (define (f x) x)
  (provide f))

(module foo typed/scheme
  (require/typed 'bar (f (Node -> Integer)))
  (define-struct: node ({x : Integer}))
  (define-type-alias Node node))

(require 'foo)
