#lang scheme/base

(require (rename-in "../utils/utils.ss" [infer r:infer]))

(require (for-syntax (private type-effect-convenience)
                     (env init-envs)
                     scheme/base
                     (r:infer infer)
                     (only-in (r:infer infer-dummy) infer-param)
                     (except-in (rep effect-rep type-rep) make-arr)
                     "type-effect-convenience.ss"
                     (only-in "type-effect-convenience.ss" [make-arr* make-arr])
                     "union.ss"))

(define-syntax (#%module-begin stx)
  (syntax-case stx (require)
    [(mb (require . args) [id ty] ...)
     (begin
       (unless (andmap identifier? (syntax->list #'(id ...)))
         (raise-syntax-error #f "not all ids"))
       #'(#%plain-module-begin
          (begin
            (require . args)
            (define-for-syntax e
              (parameterize ([infer-param infer])
                (make-env [id ty] ...)))
            (begin-for-syntax
              (initialize-type-env e)))))]
    [(mb . rest)
     #'(mb (require) . rest)]))

(provide #%module-begin
         require
         (all-from-out scheme/base)
         (for-syntax
          (all-from-out scheme/base
                        "type-effect-convenience.ss"                        
                        "union.ss")))
