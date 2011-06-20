#lang racket/base
(require racket/unit "cgi-sig.rkt" "cgi-unit.rkt")

(define-values/invoke-unit/infer cgi@)

(provide-signature-elements cgi^)
