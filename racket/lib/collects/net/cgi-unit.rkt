#lang racket/base

(require racket/unit
         "cgi-sig.rkt" "cgi.rkt")

(define-unit-from-context cgi@ cgi^)

(provide cgi@)
