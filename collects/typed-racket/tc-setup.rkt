#lang racket/base

(require "utils/utils.rkt"
         (except-in syntax/parse id)
         racket/pretty racket/promise
         (private type-contract)
         (types utils)
         (typecheck typechecker provide-handling tc-toplevel)
         (env tvar-env type-name-env type-alias-env env-req mvar-env)
         (utils tc-utils disarm mutated-vars debug)
         (rep type-rep)
         (for-syntax racket/base)
         (for-template racket/base))

(provide tc-setup invis-kw maybe-optimize)

(define-syntax-class invis-kw
  #:literals (define-values define-syntaxes #%require #%provide begin)
  (pattern (~or define-values define-syntaxes #%require #%provide begin)))

(define (maybe-optimize body)
  ;; do we optimize?
  (if (optimize?)
      (let ([optimize-top
             (begin0 (dynamic-require 'typed-racket/optimizer/optimizer
                                      'optimize-top)
               (do-time "Loading optimizer"))])
        (begin0 (map optimize-top (syntax->list body))
          (do-time "Optimized")))
      body))

(define-syntax-rule (tc-setup orig-stx stx expand-ctxt fully-expanded-stx init checker pre-result post-result . body)
  (let ()
    (set-box! typed-context? #t)
    ;(start-timing (syntax-property stx 'enclosing-module-name))
    (with-handlers
        (#;[(λ (e) (and (exn:fail? e) (not (exn:fail:syntax? e)) (not (exn:fail:filesystem? e))))
          (λ (e) (tc-error "Internal Typed Racket Error : ~a" e))])
      (parameterize (;; do we report multiple errors
                     [delay-errors? #t]
                     ;; do we print the fully-expanded syntax?
                     [print-syntax? #f]
                     ;; this parameter is just for printing types
                     ;; this is a parameter to avoid dependency issues
                     [current-type-names
                      (lazy
                        (append
                         (type-name-env-map (lambda (id ty)
                                              (cons (syntax-e id) ty)))
                         (type-alias-env-map (lambda (id ty)
                                               (cons (syntax-e id) ty)))))]
                     ;; reinitialize disappeared uses
                     [disappeared-use-todo      null]
                     [disappeared-bindings-todo null])
        (define fully-expanded-stx (disarm* (local-expand stx expand-ctxt (list #'module*))))
        (when (show-input?)
          (pretty-print (syntax->datum fully-expanded-stx)))
        (do-time "Local Expand Done")
        (init)
        (do-time "Initialized Envs")
        (find-mutated-vars fully-expanded-stx mvar-env)
        (parameterize ([orig-module-stx (or (orig-module-stx) orig-stx)]
                       [expanded-module-stx fully-expanded-stx]
                       [debugging? #f])
          (do-time "Starting `checker'")
          (define-values (pre-result post-result) (checker fully-expanded-stx))
          (do-time "Typechecking Done")
          (let () . body))))))
