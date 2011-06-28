#lang racket/load

(module A racket
  (define-struct point [x y])
    (point 1 2)
      (provide (all-defined-out)))

(module B typed/racket
  (require/typed 'A
      [struct point ([x : Integer] [y : Integer])])
      (point 1 2)
      (struct: pt ([x : Integer] [y : Integer]))
        (pt 1 2))

(require 'A)
(require 'B)
