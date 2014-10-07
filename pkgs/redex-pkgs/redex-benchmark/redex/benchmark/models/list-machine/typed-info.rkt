#lang racket/base

(require racket/runtime-path
         "../util/info-util.rkt")

(provide (all-defined-out))

(define name "list-machine")
(define fname (make-path-root 'list-machine))

(define-runtime-path here ".")

(define (all-mods)
  (all-mods/type 'typed here name fname))