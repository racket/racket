#lang racket/base

(provide append/tail-on-null)

(define-syntax-rule (append/tail-on-null e0 ... e)
  (let ([finish (lambda () e)])
    (define l (append e0 ...))
    (if (null? l)
        (finish)
        (append l (finish)))))
