#lang racket/base

(provide (all-defined-out))

(require "literals.rkt"
         (for-template "honu-typed-scheme.rkt")
         syntax/parse)

;; syntax -> string
(define (to-honu-string stx)
  (syntax-parse stx
    #:literal-sets ([cruft #:at stx])
    #:literals (honu-unparsed-begin)
    [(f (#%parens x ...) rest ...)
     (format "~a(~a)~a"
             (to-honu-string #'f)
             (to-honu-string #'(x ...))
             (to-honu-string #'(rest ...)))]
    [(x1) (to-honu-string #'x1)]
    [(x1 x2 ...)
     (format "~a ~a" (to-honu-string #'x1)
             (to-honu-string #'(x2 ...)))]
    [() ""]
    [else (format "~a" (syntax->datum stx))]))
