#lang racket/base
(require racket/unit "dns-sig.rkt" "dns-unit.rkt")

(define-values/invoke-unit/infer dns@)

(provide-signature-elements dns^)
