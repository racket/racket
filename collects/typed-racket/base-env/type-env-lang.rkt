#lang racket/base

(require (for-syntax racket/base syntax/parse))

(define-syntax (#%module-begin stx)
  (syntax-parse stx #:literals (require provide)
    [(mb (require . args) ... [nm:id ty] ...)
     #'(#%plain-module-begin
        (begin          
          (define-syntax (nm stx)            
            (raise-syntax-error 
             'type-check "type name used out of context" stx))
          ...
          (provide nm) ...
          (begin-for-syntax
            (module* #%type-decl #f
              (require 
               (only-in typed-racket/env/init-envs initialize-type-name-env))
              (require . args) ...
              (initialize-type-name-env
               (list (list #'nm ty) ...))))))]))

(provide #%module-begin require
         (all-from-out racket/base)
         (for-syntax (all-from-out racket/base)))
