#lang racket/base
(require racket/unit
         "cookie-sig.rkt"
         "cookie.rkt")

(define-unit-from-context cookie@ cookie^)

(provide cookie@)
