#lang racket/base
(require racket/unit "base64-sig.rkt" "base64-unit.rkt")

(define-values/invoke-unit/infer base64@)

(provide-signature-elements base64^)
