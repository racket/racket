#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     "exptime/util.rkt"))

(provide (for-syntax (all-from-out "exptime/util.rkt"))
         define-syntax/err-param)

(define-syntax define-syntax/err-param
  (syntax-rules ()
    [(_ (name arg) body ...)
     (define-syntax (name arg)
       (parameterize ([current-syntax-context arg])
         body ...))]))
