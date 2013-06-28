#lang racket/base

(require racket/unit
         "smtp-sig.rkt" net/smtp)

(define-unit-from-context smtp@ smtp^)

(provide smtp@)
