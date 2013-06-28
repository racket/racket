#lang racket/base

(require racket/unit
         "uri-codec-sig.rkt" net/uri-codec)

(define-unit-from-context uri-codec@ uri-codec^)

(provide uri-codec@)
