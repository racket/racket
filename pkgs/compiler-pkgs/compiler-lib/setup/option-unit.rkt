#lang racket/base
(require racket/unit setup/option "option-sig.rkt")

(provide setup:option@ set-flag-params)

(define-unit-from-context setup:option@ setup-option^)