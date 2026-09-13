#lang racket/base

(module sub racket/base
  (provide five)
  (define five 5))

(require (submod "." sub))

five
