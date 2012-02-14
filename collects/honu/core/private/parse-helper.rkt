#lang racket/base

(require (for-syntax racket/base
                     "parse2.rkt"))

(provide (all-defined-out))

(define-syntax (do-parse-rest-macro stx)
  (syntax-case stx ()
    [(_ stuff ...)
     (do-parse-rest #'(stuff ...) #'do-parse-rest-macro)]))
