#lang racket/base
(require racket/unit
         "cookie-sig.rkt"
         net/cookie)

(define-unit-from-context cookie@ cookie^)

(provide cookie@)
