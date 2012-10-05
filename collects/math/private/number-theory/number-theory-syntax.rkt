#lang racket/base

(require "modular-arithmetic.rkt")

(provide with-modulus)

(define-syntax-rule (with-modulus n body ...)
  (parameterize ([current-modulus  n])
    body ...))
