#lang racket/base
(require (submod "residual.rkt" ct))

(provide pattern-expander)

(define pattern-expander
  (let ()
    (struct pattern-expander (proc) #:transparent
      #:omit-define-syntaxes
      #:property prop:pattern-expander (λ (this) (pattern-expander-proc this)))
    pattern-expander))
