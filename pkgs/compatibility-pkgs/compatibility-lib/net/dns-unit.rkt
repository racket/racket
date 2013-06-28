#lang racket/base

(require racket/unit
         "dns-sig.rkt" net/dns)

(define-unit-from-context dns@ dns^)

(provide dns@)
