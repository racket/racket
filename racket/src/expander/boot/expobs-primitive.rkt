#lang racket/base
(require "../expand/context.rkt")

(provide expobs-primitives)

(define expobs-primitives
  (hasheq 'current-expand-observe current-expand-observe))
