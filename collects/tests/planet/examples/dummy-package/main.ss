#lang scheme

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ term ...)
     (syntax/loc stx
       (#%module-begin
        (define result '(term ...))
        (provide result)))]))

(provide (rename-out [module-begin #%module-begin]))
