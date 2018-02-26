#lang racket/base

(provide regexp-error
         regexp-error-tag)

(define regexp-error-tag (make-continuation-prompt-tag 'regexp-error))

(define (regexp-error fmt . args)
  (abort-current-continuation regexp-error-tag (apply format fmt args)))
