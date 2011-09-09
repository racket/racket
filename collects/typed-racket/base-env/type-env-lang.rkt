#lang racket/base

(require "../utils/utils.rkt"
         (for-syntax (env init-envs)
                     racket/base syntax/parse
                     (except-in (rep filter-rep type-rep) make-arr)
                     (rename-in (types union convenience) [make-arr* make-arr])))

(define-syntax (#%module-begin stx)
  (syntax-parse stx #:literals (require provide)
    [(mb (require . args) ... (provide . args2) ... [nm ty] ...)
     (unless (andmap identifier? (syntax->list #'(nm ...)))
       (raise-syntax-error #f "not all ids"))
     #'(#%plain-module-begin
        (begin
          (require . args) ...
          (provide . args2) ...
          (define-syntax nm (lambda (stx) (raise-syntax-error 'type-check "type name used out of context" stx))) ...
          (provide nm) ...
          (begin-for-syntax
           (initialize-type-name-env
            (list (list #'nm ty) ...)))))]))

(provide #%module-begin
         require
         (all-from-out racket/base)
         (for-syntax
          (types-out convenience union)
          (rep-out type-rep)
          (all-from-out racket/base)))
