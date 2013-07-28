#lang racket/load

(module a typed/racket
  (provide foo)
  (: foo
     (case-> 
       (Number String * -> Number)
       (Number String String * -> Number)))
  (define (foo x . args) x))

(require 'a)
(foo 3 "x")
