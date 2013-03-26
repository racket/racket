#lang racket/base

(module defs typed/racket/base
  (provide foo)
  (: foo Integer)
  (define foo 4)
  )

(module private-defs typed/racket/base
  (require (submod ".." defs))
  (provide foo)
  )

(require 'private-defs)
foo
