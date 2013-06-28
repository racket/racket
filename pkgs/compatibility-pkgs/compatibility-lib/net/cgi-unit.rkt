#lang racket/base

(require racket/unit
         "cgi-sig.rkt" net/cgi)

(define-unit-from-context cgi@ cgi^)

(provide cgi@)
