#lang racket/load

(module a typed/racket
  (struct: (V) foo ((v : V)))
  (provide foo))

(module b racket
  (require 'a)

  (foo 2))

(require 'b)
