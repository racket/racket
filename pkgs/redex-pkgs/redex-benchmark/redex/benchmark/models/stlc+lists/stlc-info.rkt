#lang racket/base

(require racket/runtime-path
         "../util/info-util.rkt")

(provide all-mods)

(define-runtime-path here ".")

(define (all-mods)
  (make-all-mods here "stlc" (make-path-root 'stlc+lists)))
