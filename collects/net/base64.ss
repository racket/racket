#lang scheme/base
(require scheme/unit "base64-sig.ss" "base64-unit.ss")

(define-values/invoke-unit/infer base64@)

(provide-signature-elements base64^)
