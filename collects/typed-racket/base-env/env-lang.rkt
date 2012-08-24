#lang racket/base

(require "../utils/utils.rkt")

(require (for-syntax racket/base syntax/parse)
         (utils tc-utils)
         (env init-envs)
         (types abbrev numeric-tower union filter-ops)
         (rep object-rep filter-rep type-rep))

(define-syntax (-#%module-begin stx)
  (define-syntax-class clause
    #:description "[id type]"
    (pattern [id:identifier ty]))
  (syntax-parse stx #:literals (require begin)
    [(mb (~optional (~and extra (~or (begin . _) (require . args))))
         ~! :clause ...)
     #'(#%plain-module-begin
        (begin
          extra
          (define e
            (make-env [id (λ () ty)] ...))
          (define (init)
           (initialize-type-env e))
          (provide init)))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (except-out (all-from-out racket/base) #%module-begin)
         types rep private utils
         (types-out abbrev numeric-tower union filter-ops))
