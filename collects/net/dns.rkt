#lang scheme/base
(require scheme/unit "dns-sig.ss" "dns-unit.ss")

(define-values/invoke-unit/infer dns@)

(provide-signature-elements dns^)
