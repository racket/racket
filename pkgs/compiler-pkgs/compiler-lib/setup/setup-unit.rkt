#lang racket/base

(require racket/unit setup/setup-core)

(provide setup@)
(define-unit setup@
  (import)
  (export)
  (setup-core))
