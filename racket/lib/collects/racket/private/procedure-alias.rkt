(module procedure-alias '#%kernel
  (#%require "define.rkt"
             "small-scheme.rkt"
             "more-scheme.rkt"
             (for-syntax '#%kernel
                         "stx.rkt"
                         "small-scheme.rkt"
                         "stxcase-scheme.rkt"
                         "name.rkt"
                         "norm-define.rkt"
                         "qqstx.rkt"
                         "sort.rkt"))
  
  (#%provide syntax-procedure-alias-property (protect alias-of))
  (define alias-of (gensym 'alias-of))
  (define (syntax-procedure-alias-property stx)
    (unless (syntax? stx)
      (raise-argument-error 'syntax-procedure-alias "syntax?" stx))
    (syntax-property stx alias-of)))
