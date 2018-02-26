#lang racket/base
(require "../syntax/module-binding.rkt"
         "../syntax/error.rkt"
         "../common/phase.rkt"
         "../common/module-path.rkt"
         "../namespace/module.rkt")

(provide binding->module-instance)

;; Locate a module instance for a binding
(define (binding->module-instance b ns phase id)
  (define at-phase (phase- phase (module-binding-phase b)))
  (define mi
    (namespace->module-instance ns
                                (module-path-index-resolve (module-binding-module b))
                                at-phase
                                #:check-available-at-phase-level (module-binding-phase b)
                                #:unavailable-callback (lambda (mi) 'unavailable)))
  (when (eq? mi 'unavailable)
    (raise-syntax-error
     #f
     (format (string-append "module mismatch;\n"
                            " attempted to use a module that is not available\n"
                            "  possible cause:\n"
                            "   using (dynamic-require .... #f)\n"
                            "   but need (dynamic-require .... 0)\n"
                            "  module: ~s\n"
                            "  phase: ~s")
             (module-binding-module b)
             (phase+ at-phase (module-binding-phase b)))
     id))
  (unless mi
    (error 'expand
           (string-append "namespace mismatch; cannot locate module instance\n"
                          "  module: ~s\n"
                          "  use phase: ~a\n"
                          "  definition phase: ~a\n"
                          "  for identifier: ~s")
           (module-binding-module b)
           phase
           (module-binding-phase b)
           id))
  mi)
