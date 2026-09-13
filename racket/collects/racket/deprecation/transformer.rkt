#lang racket/base


(provide deprecated-alias
         deprecated-alias-target
         deprecated-alias?)


(struct deprecated-alias (target)
  #:transparent

  #:guard (λ (target name)
            (unless (identifier? target)
              (raise-argument-error name "identifier?" target))
            target)

  #:property prop:rename-transformer
  (λ (this) (syntax-property (deprecated-alias-target this) 'not-free-identifier=? #true))

  #:omit-define-syntaxes)
