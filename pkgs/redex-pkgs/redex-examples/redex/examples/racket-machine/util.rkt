#lang racket

(require "grammar.rkt"
         redex/reduction-semantics)

(provide (all-defined-out))

(define-language any-L)

(define-metafunction any-L
  [(count-up number)
   ,(build-list (term number) (λ (x) x))])

(define-metafunction any-L
  concat : (any ...) ... -> (any ...)
  [(concat any ...) ,(apply append (term (any ...)))])
