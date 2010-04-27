#lang scheme/base
(require scheme/unit "cgi-sig.ss" "cgi-unit.ss")

(define-values/invoke-unit/infer cgi@)

(provide-signature-elements cgi^)
