#lang racket/base
(require "nfa-ep.rkt"
         (for-syntax racket/base))

(define-syntax (nfa* stx)
  (syntax-case stx ()
   [(_ starts (accepting-rule ...) (non-accepting-rule ...))
    (with-syntax
        ([([accepting-state . _] ...) #'(accepting-rule ...)])
      (quasisyntax/loc stx
        (nfa/ep starts (accepting-state ...)
                accepting-rule ...
                non-accepting-rule ...)))]))

(provide nfa*
         epsilon)
