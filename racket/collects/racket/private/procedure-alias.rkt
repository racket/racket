(module procedure-alias '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%require "kw-prop-key.rkt")
  
  (#%provide syntax-procedure-alias-property alias-of)
  (define-values (syntax-procedure-alias-property)
    (lambda (stx)
      (if (syntax? stx)
          (void)
          (raise-argument-error 'syntax-procedure-alias "syntax?" stx))
      (syntax-property stx alias-of))))
