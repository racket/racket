#lang racket/base

(require racket/unit
         "dns-sig.rkt" "dns.rkt")

(define-unit-from-context dns@ dns^)

(provide dns@)
