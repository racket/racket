#lang racket/base
(require (only-in "pattern-expander-prop.rkt" prop:pattern-expander))
(provide pattern-expander)

(struct pattern-expander (proc) #:transparent
        #:omit-define-syntaxes  ;; don't give indirect access to proc via match
        #:property prop:pattern-expander
        (λ (this) (pattern-expander-proc this))) ; needs to be wrapped in (λ (this) (_ this))
