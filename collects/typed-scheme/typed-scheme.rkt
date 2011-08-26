#lang racket/base

(require (for-syntax racket/base
                     "utils/utils.rkt" ;; only for timing/debugging
                     ;; these requires are needed since their code
                     ;; appears in the residual program
                     "typecheck/renamer.rkt" "types/type-table.rkt"))

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
    ((dynamic-require 'typed-scheme/base-env/base-structs 'initialize-structs))
    (do-time "Finshed base-structs")
    ((dynamic-require 'typed-scheme/base-env/base-env-indexing 'initialize-indexing))
    (do-time "Finshed base-env-indexing")
    ((dynamic-require 'typed-scheme/base-env/base-env 'init))
    (do-time "Finshed base-env")
    ((dynamic-require 'typed-scheme/base-env/base-env-numeric 'init))
    (do-time "Finshed base-env-numeric")
    ((dynamic-require 'typed-scheme/base-env/base-special-env 'initialize-special))    
    (do-time "Finished base-special-env")
    (set! initialized #t)))

(define-syntax-rule (drivers [name sym] ...)
  (begin
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))
      (do-standard-inits)
      (define f (dynamic-require 'typed-scheme/core 'sym))
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (f stx)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
