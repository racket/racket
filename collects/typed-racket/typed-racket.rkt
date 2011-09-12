#lang racket/base

(require
 (for-syntax racket/base "utils/utils.rkt") ;; only for timing/debugging
 ;; the below requires are needed since they provide identifiers
 ;; that may appear in the residual program
 (for-syntax "typecheck/renamer.rkt" "types/type-table.rkt")
 "utils/any-wrap.rkt" unstable/contract)

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type
         (for-syntax do-standard-inits))

(define-for-syntax initialized #f)
(define-for-syntax (do-standard-inits)
  (unless initialized
    (do-time "Starting initialization")
    ((dynamic-require 'typed-racket/base-env/base-structs 'initialize-structs))
    (do-time "Finshed base-structs")
    ((dynamic-require 'typed-racket/base-env/base-env-indexing 'initialize-indexing))
    (do-time "Finshed base-env-indexing")
    ((dynamic-require 'typed-racket/base-env/base-env 'init))
    (do-time "Finshed base-env")
    ((dynamic-require 'typed-racket/base-env/base-env-numeric 'init))
    (do-time "Finshed base-env-numeric")
    ((dynamic-require 'typed-racket/base-env/base-special-env 'initialize-special))
    (do-time "Finished base-special-env")
    (set! initialized #t)))

(define-syntax-rule (drivers [name sym] ...)
  (begin
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))      
      (define f (dynamic-require 'typed-racket/core 'sym))
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (f stx do-standard-inits)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
