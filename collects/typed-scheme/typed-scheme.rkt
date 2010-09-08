#lang racket/base

(require (for-syntax racket/base "typecheck/renamer.rkt")
         "private/base-special-env.rkt"
         "private/base-env.rkt"
         "private/base-env-numeric.rkt")

(begin-for-syntax (initialize-special))

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type)

(define-syntax (module-begin stx)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/private/base-env-indexing 'initialize-indexing))
  ((dynamic-require 'typed-scheme/core 'mb-core) stx))

(define-syntax (top-interaction stx)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/core 'ti-core) stx))

(define-syntax (with-type stx)
  ((dynamic-require 'typed-scheme/private/base-structs 'initialize-structs))
  ((dynamic-require 'typed-scheme/core 'wt-core) stx))



