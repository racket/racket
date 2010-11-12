#lang racket/base

(require (for-syntax racket/base 
                     ;; these requires are needed since their code
                     ;; appears in the residual program
                     "typecheck/renamer.rkt" "types/type-table.rkt")         
         "private/base-special-env.rkt")

(begin-for-syntax )

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type)

(define-syntax (module-begin stx)
  (initialize-special)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/private/base-env-indexing 'initialize-indexing))
  ((dynamic-require 'typed-scheme/private/base-env 'init))
  ((dynamic-require 'typed-scheme/private/base-env-numeric 'init))
  ((dynamic-require 'typed-scheme/core 'mb-core) stx))

(define-syntax (top-interaction stx)
  (initialize-special)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/private/base-env-indexing 'initialize-indexing))
  ((dynamic-require 'typed-scheme/private/base-env 'init))
  ((dynamic-require 'typed-scheme/private/base-env-numeric 'init))
  ((dynamic-require 'typed-scheme/core 'ti-core) stx))

(define-syntax (with-type stx)
  (initialize-special)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/private/base-env-indexing 'initialize-indexing))
  ((dynamic-require 'typed-scheme/private/base-env 'init))
  ((dynamic-require 'typed-scheme/private/base-env-numeric 'init))
  ((dynamic-require 'typed-scheme/core 'wt-core) stx))



