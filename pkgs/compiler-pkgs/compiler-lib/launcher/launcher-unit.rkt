#lang racket/base

(require racket/unit "launcher-sig.rkt" launcher/launcher)

(provide launcher@)

(define-unit-from-context launcher@ launcher^)
