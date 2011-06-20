#lang racket/base
(require racket/unit "uri-codec-sig.rkt" "uri-codec-unit.rkt")

(provide-signature-elements uri-codec^)

(define-values/invoke-unit/infer uri-codec@)
