#lang racket/base

(require racket/unit
         "uri-codec-sig.rkt" "uri-codec.rkt")

(define-unit-from-context uri-codec@ uri-codec^)

(provide uri-codec@)
