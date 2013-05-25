#lang racket/load
(module a typed/racket
  (struct: S ((x : Integer)) #:transparent)
    (provide (all-defined-out)))

(module b typed/racket/no-check
  (require 'a)
    (S 5))

(require 'b)
