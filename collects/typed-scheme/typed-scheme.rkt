#lang racket/base

(require (for-syntax racket/base "typecheck/renamer.rkt"))

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction]
                     [#%plain-lambda lambda]
                     [#%app #%app]
                     [require require])
         with-type)

(define-syntax (module-begin stx)
  (dynamic-require 'typed-scheme/private/base-env #f)
  (dynamic-require 'typed-scheme/private/base-env-numeric #f)
  (dynamic-require 'typed-scheme/private/base-env-indexing #f)
  ((dynamic-require 'typed-scheme/core 'mb-core) stx))

(define-syntax (top-interaction stx)
  ((dynamic-require 'typed-scheme/core 'ti-core) stx))

(define-syntax (with-type stx)
  ((dynamic-require 'typed-scheme/core 'wt-core) stx))



