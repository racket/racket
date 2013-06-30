#lang racket/base
(require racket/unit)

(require "link-sig.rkt"
         "link-unit.rkt")

(define-values/invoke-unit/infer dynext:link@)

(provide-signature-elements dynext:link^)
