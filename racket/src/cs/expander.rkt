#lang racket/base
(require '#%paramz
         (only-in '#%kernel prop:method-arity-error)
         '#%linklet
         racket/unsafe/ops
         racket/fixnum
         racket/flonum
         racket/include)

(define-syntax-rule (linklet () ([int-id ext-id] ...) body ...)
  (begin
    (provide (rename-out [int-id ext-id] ...))
    body ...))

(include "expander.rktl")
