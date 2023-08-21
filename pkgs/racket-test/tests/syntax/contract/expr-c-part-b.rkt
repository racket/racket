#lang racket/base
(require (for-syntax racket/base))

(define-syntax (require-a-via-path stx)
  #`(#%require #,(datum->syntax stx (string->path "expr-c-part-a.rkt"))))

(require-a-via-path)
(unless (equal? (m "hello") "hello")
  (error "test failed"))

