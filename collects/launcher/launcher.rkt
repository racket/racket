#lang scheme/base

(require scheme/unit
         "launcher-sig.ss"
         "launcher-unit.ss")

(define-values/invoke-unit/infer launcher@)

(provide-signature-elements launcher^)
