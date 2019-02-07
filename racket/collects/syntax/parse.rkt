#lang racket/base
(require (for-syntax racket/base)
         racket/contract/base
         "parse/pre.rkt"
         "parse/experimental/provide.rkt"
         "parse/experimental/contract.rkt")
(provide (except-out (all-from-out "parse/pre.rkt")
                     static)
         expr/c)
(provide-syntax-class/contract
 [static (syntax-class/c [(-> any/c any/c) (or/c string? symbol? #f)])])

(begin-for-syntax
  (require racket/contract/base
           syntax/parse/private/residual-ct)
  (provide pattern-expander?
           (contract-out
            [prop:syntax-class
             (struct-type-property/c (or/c identifier? (-> any/c identifier?)))]
            [pattern-expander
             (-> (-> syntax? syntax?) pattern-expander?)]
            [prop:pattern-expander
             (struct-type-property/c (-> pattern-expander? (-> syntax? syntax?)))]
            [syntax-local-syntax-parse-pattern-introduce
             (-> syntax? syntax?)]))

  (define pattern-expander
    (let ()
      (struct pattern-expander (proc) #:transparent
        #:omit-define-syntaxes
        #:property prop:pattern-expander (λ (this) (pattern-expander-proc this)))
      pattern-expander)))
