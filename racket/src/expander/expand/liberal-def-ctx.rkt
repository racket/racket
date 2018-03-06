#lang racket/base

(provide prop:liberal-define-context
         (rename-out [has-liberal-define-context-property? liberal-define-context?])
         make-liberal-define-context)

(define-values (prop:liberal-define-context has-liberal-define-context-property? liberal-define-context-value)
  (make-struct-type-property 'liberal-define-context))

(struct liberal-define-context ()
  #:transparent
  #:property prop:liberal-define-context #t
  #:constructor-name make-liberal-define-context)



