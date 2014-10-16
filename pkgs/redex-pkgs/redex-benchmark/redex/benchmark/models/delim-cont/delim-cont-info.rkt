#lang racket/base

(require racket/runtime-path
         "../util/info-util.rkt")

(provide all-mods)

(define-runtime-path here ".")

(define (all-mods)
  (make-all-mods here "delim-cont" (make-path-root 'delim-cont)))
