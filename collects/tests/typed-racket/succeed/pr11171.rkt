#lang racket/load

(module UNTYPED racket/base
  (struct IntTree
    (elem left right))

  (provide (struct-out IntTree)))

(module TYPED typed/racket
  (require/typed 'UNTYPED
                 [struct IntTree
                   ([elem  : Integer]
                    [left  : IntTree]
                    [right : IntTree])]))
