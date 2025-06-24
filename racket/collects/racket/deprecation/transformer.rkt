#lang racket/base


(provide deprecated-alias
         deprecated-alias-target
         deprecated-alias?)


(struct deprecated-alias (target)
  #:transparent
  #:guard (λ (name target)
            (unless (identifier? target)
              (raise-argument-error name "identifier?" target))
            (syntax-property target 'not-free-identifier=? #true))
  #:property prop:rename-transformer 0
  #:omit-define-syntaxes)
