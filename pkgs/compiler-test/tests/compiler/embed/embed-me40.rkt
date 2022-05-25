#lang racket/base
(require racket/runtime-path
         (for-syntax racket/base)
         "embed-me40b.rkt")

(define-runtime-module-path-index bp
  (build-path "embed-me40b.rkt"))

(eq? b
     (dynamic-require bp 'b))
