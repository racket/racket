#lang racket/base

(provide with-holding)

(define-syntax-rule (with-holding v expr)
  (let ([val v])
    (begin0
     expr
     (done-with val))))

;; Ensure no inline:
(define done-with #f)
(set! done-with void)
