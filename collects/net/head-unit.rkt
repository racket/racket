#lang racket/base

(require racket/unit
         "head-sig.rkt" "head.rkt")

(define-unit-from-context head@ head^)

(provide head@)
