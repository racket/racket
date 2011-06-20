#lang racket/base
(require racket/unit "cookie-sig.rkt" "cookie-unit.rkt")

(provide-signature-elements cookie^)

(define-values/invoke-unit/infer cookie@)
