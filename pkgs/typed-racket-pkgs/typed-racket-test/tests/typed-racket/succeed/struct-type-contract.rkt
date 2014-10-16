#lang racket/load

(module m1 typed/racket
  (struct x ())
  (define: y : Struct-TypeTop struct:x)
  (provide struct:x y))

(module m2 racket
  (require 'm1)
  struct:x y)

(require 'm2)
