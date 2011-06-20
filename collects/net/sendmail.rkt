#lang racket/base
(require racket/unit "sendmail-sig.rkt" "sendmail-unit.rkt")

(define-values/invoke-unit/infer sendmail@)

(provide-signature-elements sendmail^)
