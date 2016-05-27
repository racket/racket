#lang racket/base

(module+ main
  12)

(module submod racket/base
  11)

10
(require (submod "embed-me27.rkt" other-submod))
