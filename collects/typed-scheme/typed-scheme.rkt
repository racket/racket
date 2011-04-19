#lang racket/base

(require (for-syntax racket/base
                     ;; these requires are needed since their code
                     ;; appears in the residual program
                     "typecheck/renamer.rkt" "types/type-table.rkt" profile)
         "private/base-special-env.rkt" )

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type
         (for-syntax do-standard-inits))

(define-for-syntax (do-standard-inits)
  (initialize-special)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/private/base-env-indexing 'initialize-indexing))
  ((dynamic-require 'typed-scheme/private/base-env 'init))
  ((dynamic-require 'typed-scheme/private/base-env-numeric 'init)))

(define-syntax-rule (drivers [name sym] ...)
  (begin 
    (define-syntax (name stx)
      (do-standard-inits)
      ((dynamic-require 'typed-scheme/core 'sym) stx))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
