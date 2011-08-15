#lang racket/base

(require (prefix-in racket: racket/base))

;; require's and provide's a module
(define-syntax-rule (provide-module module ...)
  (begin
    (begin
      (racket:require module)
      (racket:provide [all-from-out module]))
    ...))

(provide-module "core/main.rkt"
                "private/common.rkt"
                ;;"private/struct.honu"
                ;;"private/function.honu"
                ;;"private/common.honu"
                )

(provide sqr sqrt sin max)
