#lang scheme/base
(require scheme/unit "cookie-sig.ss" "cookie-unit.ss")

(provide-signature-elements cookie^)

(define-values/invoke-unit/infer cookie@)
