#lang racket/base

(require racket/unit
         "head-sig.rkt" net/head)

(define-unit-from-context head@ head^)

(provide head@)
