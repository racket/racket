#lang scheme/base

(require "../utils/utils.rkt")

(require (for-syntax (env init-envs)
                     scheme/base
                     (except-in (rep filter-rep type-rep) make-arr)
                     (rename-in (types union convenience) [make-arr* make-arr])))

(define-syntax (#%module-begin stx)
  (syntax-case stx (require)
    [(mb (require . args) [nm ty] ...)
     (begin
       (unless (andmap identifier? (syntax->list #'(nm ...)))
         (raise-syntax-error #f "not all ids"))
       #'(#%plain-module-begin
          (begin
            (require . args)
            (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
            (provide nm) ...
            ;(define-syntax provider (lambda (stx) #'(begin (provide nm) ...)))
            ;(provide provider)            
            (begin-for-syntax 
              ;(printf "running base-types\n")
              (initialize-type-name-env
               (list (list #'nm ty) ...))))))]
    [(mb . rest)
     #'(mb (require) . rest)]))

(provide #%module-begin
         require
         (all-from-out scheme/base)
         (for-syntax
          (types-out convenience union)
          (rep-out type-rep)
          (all-from-out scheme/base)))
