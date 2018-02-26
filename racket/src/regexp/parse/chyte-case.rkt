#lang racket/base
(require "chyte.rkt")

;; Dispatch on chytes with character patterns

(provide chyte-case
         chyte-case/eos)

(define-syntax-rule (chyte-case c clause ...)
  (case (integer->char c)
    clause ...))

(define-syntax-rule (chyte-case/eos s-expr pos-expr clause ...)
  (let ([pos pos-expr]
        [s s-expr])
    (case (if (= pos (chytes-length s)) 'eos (chytes-ref/char s pos))
      clause ...)))
