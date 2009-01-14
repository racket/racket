#lang scheme/base

(require "../utils/utils.ss")

(require (for-syntax (private type-effect-convenience)
                     (env init-envs)
                     scheme/base
                     (except-in (rep effect-rep type-rep) make-arr)
                     (except-in "../rep/type-rep.ss" make-arr)
                     "type-effect-convenience.ss"
                     (only-in "type-effect-convenience.ss" [make-arr* make-arr])
                     "union.ss"))

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
              ;(printf "running base-types~n")
              (initialize-type-name-env
               (list (list #'nm ty) ...))))))]
    [(mb . rest)
     #'(mb (require) . rest)]))

(provide #%module-begin
         require
         (all-from-out scheme/base)
         (for-syntax
          (all-from-out scheme/base
                        "type-effect-convenience.ss"
                        "../rep/type-rep.ss"
                        "union.ss")))
