#lang scheme/base
(require scheme/unit "head-sig.ss" "head-unit.ss")

(define-values/invoke-unit/infer head@)

(provide-signature-elements head^)
