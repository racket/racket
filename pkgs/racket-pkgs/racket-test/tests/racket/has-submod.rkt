#lang racket/base

(module+ main
  (require "for-submod.rkt")
  (provide has-submod)
  (define has-submod for-submod))
