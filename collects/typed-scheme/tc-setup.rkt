#lang racket/base

(require (rename-in "utils/utils.rkt" [infer r:infer])
         (except-in syntax/parse id)
         unstable/mutated-vars
         scheme/base
         (private type-contract)
         (types utils convenience)
         (typecheck typechecker provide-handling tc-toplevel)
         (env tvar-env type-name-env type-alias-env)
         (r:infer infer)
         (utils tc-utils)
         (rep type-rep)
         (except-in (utils utils) infer)
         (only-in (r:infer infer-dummy) infer-param)
         racket/match
         (for-syntax racket/base)
         (for-template racket/base))

(provide tc-setup invis-kw)

(define-syntax-class invis-kw
  #:literals (define-values define-syntaxes #%require #%provide begin)
  (pattern (~or define-values define-syntaxes #%require #%provide begin)))

(define-syntax-rule (tc-setup orig-stx stx expand-ctxt fully-expanded-stx checker result . body)
  (let ()
    (set-box! typed-context? #t)
    (start-timing (syntax-property stx 'enclosing-module-name))
    (with-handlers
        ([(lambda (e) (and #f (exn:fail? e) (not (exn:fail:syntax? e))))
          (lambda (e) (tc-error "Internal Typed Racket Error : ~a" e))])
      (parameterize (;; enable fancy printing?
                     [custom-printer #t]
                     ;; a cheat to avoid units
                     [infer-param infer]
                     ;; do we report multiple errors
                     [delay-errors? #t]
                     ;; this parameter is for parsing types
                     [current-tvars initial-tvar-env]
                     ;; this parameter is just for printing types
                     ;; this is a parameter to avoid dependency issues
                     [current-type-names
                      (lambda ()
                        (append 
                         (type-name-env-map (lambda (id ty)
                                              (cons (syntax-e id) ty)))
                         (type-alias-env-map (lambda (id ty)
                                               (cons (syntax-e id) ty)))))]
                     ;; reinitialize seen type variables
                     [type-name-references null])
        (do-time "Initialized Envs")
        (let ([fully-expanded-stx (local-expand stx expand-ctxt null)])
          (do-time "Local Expand Done")
          (parameterize ([mutated-vars (find-mutated-vars fully-expanded-stx)]
                         [orig-module-stx (or (orig-module-stx) orig-stx)]
                         [expanded-module-stx fully-expanded-stx])
            (let ([result (checker fully-expanded-stx)])
              (do-time "Typechecking Done")
              . body)))))))
