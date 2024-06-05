#lang racket/base
(require "binding.rkt"
         "name.rkt")

(provide provides-to-names)

(define (provides-to-names provides names)
  (for/hasheqv ([(root-phase binds) (in-hash provides)])
    (values root-phase
            (for/list ([bind (in-hash-values binds)]
                       #:unless (binding-syntax? bind)
                       #:do [(define-values (sym path/submod phase) (binding-sym-path/submod-phase bind))
                             (define name
                               ;; If we don't find the name, then assume it's from a globally
                               ;; excluded module, so there's no name mapping
                               (maybe-find-name names (cons path/submod phase) sym))]
                       #:when name)
              name))))
