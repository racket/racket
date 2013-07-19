#lang scheme/base

(require scheme/unit
         "launcher-sig.rkt"
         "launcher-unit.rkt")

(define-values/invoke-unit/infer launcher@)

(provide-signature-elements launcher^)
