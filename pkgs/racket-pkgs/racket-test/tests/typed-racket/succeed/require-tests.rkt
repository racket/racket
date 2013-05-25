#lang scheme/load
#reader typed-racket/typed-reader
(module bang-tests typed-scheme
  (define #{x : Number} 1)
  (provide x)
  )

(module trequire typed-scheme
  (require 'bang-tests)
  (define: y : Number x)
  (display y))
