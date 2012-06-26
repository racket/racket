#lang racket/base

(require "../utils/utils.rkt"                  
         (for-syntax "../env/global-env.rkt"
                     racket/base syntax/parse
                     (except-in (rep filter-rep type-rep) make-arr)
                     (rename-in (types numeric-tower abbrev convenience))))

(define-syntax (#%module-begin stx)
  (syntax-parse stx #:literals (require provide)
    [(mb (require . args) ... (provide . args2) ... [nm:id ty] ...)
     #'(#%plain-module-begin
        (begin
          (require . args) ...
          (provide . args2) ...
          (define-syntax (nm stx)            
            (raise-syntax-error 'type-check "type name used out of context" stx))
          ...
          (provide nm) ...
          (begin-for-syntax
           ((dynamic-require 'typed-racket/env/init-envs 
                             'initialize-type-name-env)
            (list (list #'nm ty) ...)))))]))

(provide #%module-begin
         require
         (all-from-out racket/base)
         (for-syntax          
          (rep-out type-rep)
          (types-out numeric-tower abbrev convenience)
          (all-from-out racket/base)))
