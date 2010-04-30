#lang scheme/base
(require scheme/unit "nntp-sig.ss" "nntp-unit.ss")

(define-values/invoke-unit/infer nntp@)

(provide-signature-elements nntp^)
