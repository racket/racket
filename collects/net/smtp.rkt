#lang racket/base
(require racket/unit "smtp-sig.rkt" "smtp-unit.rkt")

(define-values/invoke-unit/infer smtp@)

(provide-signature-elements smtp^)
