#lang racket/base

#;(require (for-syntax racket/contract))

(define-syntax-rule (provide/contract* [id ctrct] ...)
  #;(provide/contract [id ctrct] ...)
  (provide id ...))

(provide provide/contract*)
