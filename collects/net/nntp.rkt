#lang racket/base
(require racket/unit "nntp-sig.rkt" "nntp-unit.rkt")

(define-values/invoke-unit/infer nntp@)

(provide-signature-elements nntp^)
