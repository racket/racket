#lang racket/base
(require (for-syntax racket/base)
         syntax/parse/define)

(provide m)

(define-syntax-parse-rule (m e)
  #:declare e (expr/c #'string?)
  e.c)
