#lang racket/base
(require racket/unit)

(require "file-sig.rkt"
         "file-unit.rkt")

(define-values/invoke-unit/infer dynext:file@)

(provide-signature-elements dynext:file^)
