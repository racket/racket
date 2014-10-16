#lang racket/base

(require "utils/utils.rkt"
         (except-in syntax/parse id) syntax/stx
         racket/pretty racket/promise racket/lazy-require
         racket/syntax
         (env type-name-env type-alias-env mvar-env)
         (utils tc-utils disarm mutated-vars lift)
         "standard-inits.rkt"
         (for-syntax racket/base)
         (for-template racket/base))
(lazy-require [typed-racket/optimizer/optimizer (optimize-top)])
(lazy-require [typed-racket/typecheck/tc-toplevel (tc-toplevel-form tc-module)])

(provide maybe-optimize init-current-type-names
         tc-module/full
         tc-toplevel/full)

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

(define-logger online-check-syntax)

(define (tc-setup orig-stx stx expand-ctxt do-expand checker k)
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
      (define fully-expanded-stx (disarm* (do-expand stx expand-ctxt (list #'module*))))
      (when (show-input?)
        (pretty-print (syntax->datum fully-expanded-stx)))
      (do-time "Local Expand Done")
      (let ([exprs (syntax->list (syntax-local-introduce fully-expanded-stx))])
        (when (pair? exprs)
          (log-message online-check-syntax-logger
                       'info
                       "TR's expanded syntax objects; this message is ignored"
                       (cdr exprs))))
      ;; We do standard inits here because it is costly (~250 msec), and we want
      ;; expansion errors to happen with out paying that cost
      (do-standard-inits)
      (do-time "Initialized Envs")
      (find-mutated-vars fully-expanded-stx mvar-env)
      (parameterize ([orig-module-stx (or (orig-module-stx) orig-stx)]
                     [expanded-module-stx fully-expanded-stx])
        (do-time "Starting `checker'")
        (call-with-values (λ () (checker fully-expanded-stx))
          (λ results
            (do-time "Typechecking Done")
            (apply k fully-expanded-stx results)))))))

(define (tc-toplevel/full orig-stx stx k)
  (tc-setup orig-stx stx 'top-level local-expand/capture* tc-toplevel-form k))

(define (tc-module/full orig-stx stx k)
  (tc-setup orig-stx stx 'module-begin local-expand tc-module k))
