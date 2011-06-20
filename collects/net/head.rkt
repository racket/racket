#lang racket/base
(require racket/unit "head-sig.rkt" "head-unit.rkt")

(define-values/invoke-unit/infer head@)

(provide-signature-elements head^)
