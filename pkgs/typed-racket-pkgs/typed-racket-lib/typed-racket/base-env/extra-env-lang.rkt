#lang racket/base

;; This module defines a #lang for use in defining extra base
;; type environments that will only be included on a `require`
;; (unlike the monolithic base type environment in base-env.rkt)
;;
;; Also see env-lang.rkt

(require "../utils/utils.rkt"
         (for-syntax (private parse-type))
         (for-syntax racket/base syntax/parse)
         (types abbrev numeric-tower union filter-ops)
         (for-syntax (types abbrev numeric-tower union filter-ops)))

(provide (rename-out [-#%module-begin #%module-begin])
         require
         (for-syntax parse-type) ; to allow resolution of Name types
         (except-out (all-from-out racket/base) #%module-begin)
         (for-syntax (except-out (all-from-out racket/base) #%module-begin))
         types rep private utils
         (for-syntax (types-out abbrev numeric-tower union filter-ops)))

(define-syntax (-#%module-begin stx)
  (define-syntax-class clause
    #:description "[id type]"
    (pattern [id:identifier ty]
             #:with register #'(register-type (quote-syntax id) ty)))
  (syntax-parse stx #:literals (require provide begin)
    [(mb (~optional
          (~and extra (~or (begin . _)
                           (require . args)
                           (provide . args)))
          #:defaults ([extra #'(void)]))
         ~! binding:clause ...)
     #'(#%plain-module-begin
        extra
        (require (for-syntax typed-racket/env/env-req))
        (begin-for-syntax
         (module* #%type-decl #f
           (#%plain-module-begin ;; avoid top-level printing and config
            (require typed-racket/types/numeric-tower typed-racket/env/type-name-env
                     typed-racket/env/global-env typed-racket/env/type-alias-env
                     typed-racket/types/struct-table typed-racket/types/abbrev
                     (rename-in racket/private/sort [sort raw-sort]))
            ;; FIXME: add a switch to turn contracts on for testing
            binding.register ...)))
        (begin-for-syntax (add-mod! (variable-reference->module-path-index
                                     (#%variable-reference))))
        (provide binding.id ...))]
    [(mb . rest)
     #'(mb (begin) . rest)]))

