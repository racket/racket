#lang racket/base

(require
 (for-syntax racket/base racket/lazy-require
             "standard-inits.rkt")
 (for-syntax "utils/timing.rkt") ;; only for timing/debugging
 ;; the below requires are needed since they provide identifiers
 ;; that may appear in the residual program
 ;; TODO: figure out why this are needed here and not somewhere else
 (submod "private/type-contract.rkt" predicates)
 "utils/utils.rkt"
 (for-syntax "utils/utils.rkt")
 "utils/any-wrap.rkt" "utils/struct-type-c.rkt" unstable/contract racket/contract/parametric)

(provide (rename-out [module-begin #%module-begin]
                     [top-interaction #%top-interaction])
         with-type
         (for-syntax do-standard-inits))


(define-syntax-rule (drivers [name sym] ...)
  (begin
    (begin-for-syntax
      (lazy-require (typed-racket/core (sym ...))))
    (define-syntax (name stx)
      (do-time (format "Calling ~a driver" 'name))      
      (do-time (format "Loaded core ~a" 'sym))
      (begin0 (sym stx)
              (do-time "Finished, returning to Racket")))
    ...))

(drivers [module-begin mb-core] [top-interaction ti-core] [with-type wt-core])
