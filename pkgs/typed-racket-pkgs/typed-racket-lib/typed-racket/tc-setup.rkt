#lang racket/base

(require "utils/utils.rkt"
         (except-in syntax/parse id) syntax/stx
         racket/pretty racket/promise racket/lazy-require
         (env type-name-env type-alias-env mvar-env)
         (utils tc-utils disarm mutated-vars)
         (for-syntax racket/base)
         (for-template racket/base))
(lazy-require [typed-racket/optimizer/optimizer (optimize-top)])

(provide tc-setup invis-kw maybe-optimize init-current-type-names)

(define-syntax-class invis-kw
  #:literals (define-values define-syntaxes #%require
              #%provide #%declare begin begin-for-syntax)
  (pattern (~or define-values define-syntaxes #%require
                #%provide #%declare begin begin-for-syntax)))

(define (maybe-optimize body)
  ;; do we optimize?
  (if (optimize?)
      (begin
        (do-time "Starting optimizer")
        (begin0 (stx-map optimize-top body)
          (do-time "Optimized")))
      body))

;; -> Promise<Dict<Name, Type>>
;; initialize the type names for printing
(define (init-current-type-names)
  (lazy
   (append
    (type-name-env-map (lambda (id ty)
                         (cons (syntax-e id) ty)))
    (type-alias-env-map (lambda (id ty)
                          (cons (syntax-e id) ty))))))

(define-syntax-rule (tc-setup orig-stx stx expand-ctxt fully-expanded-stx init checker pre-result post-result . body)
  (tc-setup/proc orig-stx stx expand-ctxt init checker
                 (λ (fully-expanded-stx pre-result post-result) 
                   . 
                   body)))

(define (tc-setup/proc orig-stx stx expand-ctxt init checker f)
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
                   [current-type-names (init-current-type-names)]
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
                     [expanded-module-stx fully-expanded-stx])
        (do-time "Starting `checker'")
        (define-values (pre-result post-result) (checker fully-expanded-stx))
        (do-time "Typechecking Done")
        (f fully-expanded-stx pre-result post-result)))))
