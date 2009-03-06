#lang scheme/base

(require (rename-in "../utils/utils.ss" [infer r:infer]))

(require (for-syntax (utils tc-utils)
                     (env init-envs)
                     scheme/base
                     (r:infer infer)
                     (only-in (r:infer infer-dummy) infer-param)
                     (except-in (rep object-rep filter-rep type-rep) make-arr)
                     (types convenience union)
                     (only-in (types convenience) [make-arr* make-arr])))

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
         types rep private utils
         (for-syntax          
          (types-out convenience union)
          (all-from-out scheme/base)))
