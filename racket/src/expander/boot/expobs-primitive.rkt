#lang racket/base
(require "../expand/context.rkt"
         "../expand/syntax-local.rkt")

(provide expobs-primitives)

(define expobs-primitives
  (hasheq 'current-expand-observe current-expand-observe
          'syntax-local-expand-observer syntax-local-expand-observer))
