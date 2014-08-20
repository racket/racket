#lang racket/base

(provide prop:pattern-expander
         pattern-expander
         pattern-expander?
         pattern-expander-proc
         syntax-local-syntax-parse-pattern-introduce
         )

(require "pattern-expander-prop.rkt")

(module pattern-expander-struct racket/base
  (require racket/contract/base)
  
  (require (only-in "pattern-expander-prop.rkt" prop:pattern-expander))
  
  (struct pattern-expander (proc) #:transparent
    #:property prop:pattern-expander
    (λ (this) (pattern-expander-proc this))) ; needs to be wrapped in (λ (this) (_ this))
  
  (provide (contract-out
            [struct pattern-expander ([proc (-> syntax? syntax?)])]
            )))

(require (only-in 'pattern-expander-struct pattern-expander))
