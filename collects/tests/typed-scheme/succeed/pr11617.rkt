#lang racket/load

(module A typed/racket
  (provide (all-defined-out))
  (struct: one ([thing : Any]))
  (define-match-expander uno
    (syntax-rules ()
      [(_ x) (one x)]))
  (match (one 2)
    [(uno dos) dos]))

(require 'A)

(module B typed/racket
  (require 'A)
  (match (one 2)
    [(uno dos) dos]))

(require 'B)
