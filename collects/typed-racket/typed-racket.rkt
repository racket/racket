#lang racket/base

(require
 (for-syntax racket/base racket/lazy-require "env/env-req.rkt")
 (for-syntax "utils/timing.rkt") ;; only for timing/debugging
 ;; the below requires are needed since they provide identifiers
 ;; that may appear in the residual program
 ;; TODO: figure out why this are needed here and not somewhere else
 "utils/utils.rkt"
 (for-syntax "utils/utils.rkt")
 "utils/any-wrap.rkt" unstable/contract racket/contract/parametric)

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         with-type
         (for-syntax do-standard-inits))

(begin-for-syntax
  (lazy-require
    [typed-racket/base-env/base-env ((init init-base-env))]
    [typed-racket/base-env/base-env-numeric ((init init-base-env-numeric))]
    [typed-racket/base-env/base-structs (initialize-structs)]
    [typed-racket/base-env/base-env-indexing (initialize-indexing)]
    [typed-racket/base-env/base-special-env (initialize-special)]
    [typed-racket/base-env/base-contracted (initialize-contracted)]
    [(submod typed-racket/base-env/base-types initialize) (initialize-type-names)]))

(define-for-syntax initialized #f)
(define-for-syntax (do-standard-inits)
  (unless initialized
    (do-time "Starting initialization")
    (initialize-structs)
    (do-time "Finshed base-structs")
    (initialize-indexing)
    (do-time "Finshed base-env-indexing")
    (init-base-env)
    (do-time "Finshed base-env")
    (init-base-env-numeric)
    (do-time "Finshed base-env-numeric")
    (initialize-special)
    (do-time "Finished base-special-env")
    (initialize-contracted)
    (do-time "Finished base-contracted")
    (initialize-type-names)
    (do-time "Finished base-types")
    (set! initialized #t))
  (do-requires))

(define-syntax-rule (drivers [name sym] ...)
  (begin
    (begin-for-syntax
      (lazy-require (typed-racket/core (sym ...))))
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))      
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (sym stx do-standard-inits)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
