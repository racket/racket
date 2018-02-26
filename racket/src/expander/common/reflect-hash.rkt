#lang racket/base

(provide reflect-hash)

(define-syntax-rule (reflect-hash id ...)
  (reflect-hash* ['id id] ...))

(define-syntax-rule (reflect-hash* [e ...] ...)
  (hasheq e ... ...))
