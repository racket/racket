#lang racket/base
(require "../namespace/namespace.rkt")

(provide (struct-out compile-context)
         make-compile-context)

(struct compile-context (namespace   ; compile-time namespace
                         phase       ; phase (top level) or phase level (within a module)
                         self        ; to detect bindings within the same namespace
                         module-self ; if non-#f, same as `self` and compiling the body of a module
                         full-module-name ; set to a symbol or symbol list if `self` is non-#f
                         lazy-syntax-literals? ; #t (for modules) => deserialize and shift syntax on demand
                         header)     ; accumulates initialization and other parts shared among expressions
  #:authentic)

(define (make-compile-context #:namespace [namespace (current-namespace)]
                              #:phase [phase (namespace-phase namespace)]
                              #:self [self (namespace-self-mpi namespace)]
                              #:module-self [module-self #f]
                              #:full-module-name [full-module-name #f]
                              #:lazy-syntax-literals? [lazy-syntax-literals? (and module-self #t)])
  (when (and module-self (not full-module-name))
    (error "internal error: module-self provided without full name"))
  (compile-context namespace 
                   phase
                   self
                   module-self
                   full-module-name
                   lazy-syntax-literals?
                   #f))
