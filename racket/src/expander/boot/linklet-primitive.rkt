#lang racket/base
(require (for-syntax racket/base)
         (except-in "../host/linklet.rkt"
                    ;; replaced by "linklet-api.rkt"
                    linklet?
                    recompile-linklet
                    eval-linklet
                    instantiate-linklet
                    linklet-import-variables
                    linklet-export-variables)
         "../compile/linklet-api.rkt"
         "../common/reflect-hash.rkt"
         "../run/linklet-operation.rkt")

(provide linklet-primitives
         linklet-expander-primitives)

(define-syntax (reflect-hash-except stx)
  (syntax-case stx ()
    [(form id ...)
     #`(reflect-hash #,@(for/list ([id (in-list (syntax->list #'(id ...)))]
                                   #:unless (memq (syntax-e id)
                                                  '(linklet?
                                                    recompile-linklet
                                                    eval-linklet
                                                    instantiate-linklet
                                                    linklet-import-variables
                                                    linklet-export-variables)))
                          id))]))

(define linklet-primitives
  (linklet-operations=> reflect-hash-except))

(define linklet-expander-primitives
  (reflect-hash linklet-directory?
                linklet-directory->hash
                hash->linklet-directory
                linklet-bundle?
                linklet-bundle->hash
                hash->linklet-bundle
                linklet-body-reserved-symbol?

                ;; These replace host primitives:
                linklet?
                recompile-linklet
                eval-linklet
                instantiate-linklet
                linklet-import-variables
                linklet-export-variables))
