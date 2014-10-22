#lang racket/base

(require racket/runtime-path
         "../util/info-util.rkt")

(provide (all-defined-out))

(define name "stlc")
(define fname (make-path-root 'stlc+lists))

(define-runtime-path here ".")

(define (all-mods)
  (all-mods/type 'typed here name fname))