#lang racket/base
(require "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "context.rkt")

(provide prepare-next-phase-namespace)

(define (prepare-next-phase-namespace ctx)
  (define phase (add1 (expand-context-phase ctx)))
  (define ns (namespace->namespace-at-phase (expand-context-namespace ctx)
                                            phase))
  (namespace-visit-available-modules! ns phase))
