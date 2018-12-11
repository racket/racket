#lang racket/base
(require "../compile/compiled-in-memory.rkt"
         "../host/linklet.rkt"
         "../common/contract.rkt"
         "module.rkt"
         "../namespace/provided.rkt"
         "../namespace/provide-for-api.rkt"
         "reflect-compiled.rkt"
         "reflect-name.rkt"
         "reflect-submodule.rkt")

(provide compiled-expression?

         compiled-module-expression?
         module-compiled-name
         module-compiled-submodules
         module-compiled-language-info
         module-compiled-imports
         module-compiled-exports
         module-compiled-indirect-exports
         module-compiled-cross-phase-persistent?)

;; The representation of a module with its submodules is designed to
;; make reading an individual submodule (with its submodule path
;; intact) fast and convenient --- but it makes adjusting the name
;; inconvenient, because each linklet bundle for a module encodes its
;; full submodule path. The extra layer of `compiled-in-memory`
;; support for sharing and fast compile-then-eval cycles is another
;; layer of inconvenience.

(define/who (module-compiled-language-info c)
  (check who compiled-module-expression? c)  
  (define h (compiled-module->h c))
  (hash-ref h 'language-info #f))

(define/who (module-compiled-imports c)
  (check who compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (instance-variable-value inst 'requires))

(define/who (module-compiled-exports c)
  (check who compiled-module-expression? c)
  (define inst (compiled-module->declaration-instance c))
  (provides->api-provides (instance-variable-value inst 'provides)
                          (instance-variable-value inst 'self-mpi)))

(define/who (module-compiled-indirect-exports c)
  (check who compiled-module-expression? c)
  (define-values (h inst) (compiled-module->h+declaration-instance c))
  (define min-phase (hash-ref h 'min-phase 0))
  (define max-phase (hash-ref h 'max-phase 0))
  (variables->api-nonprovides (instance-variable-value inst 'provides)
                              (for/hash ([phase-level (in-range min-phase (add1 max-phase))])
                                (define linklet (hash-ref h phase-level #f))
                                (values phase-level
                                        (if linklet
                                            (linklet-export-variables linklet)
                                            null)))))

(define/who (module-compiled-cross-phase-persistent? c)
  (check who compiled-module-expression? c)
  (define h (compiled-module->h c))
  (hash-ref h 'cross-phase-persistent? #f))
