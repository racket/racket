#lang racket/load
(module a typed/racket
  (define (foo x) (list x))
  (provide
    (rename-out (foo foo2))
    foo))

(module b racket
  (require 'a)
  (foo 2)
  (foo2 3))

(require 'b)
