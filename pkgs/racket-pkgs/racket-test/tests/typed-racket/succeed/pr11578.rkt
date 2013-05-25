#lang racket/load

(module a typed/racket/base
  (provide foo)
    (struct: foo ()))

(module b racket/base
  (require 'a)
    (foo))


(require 'b)
