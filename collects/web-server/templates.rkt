#lang racket/base
(require xml
         scribble/text
         racket/port)

(define-syntax include-template
  (syntax-rules ()
    [(_ p)
     (with-output-to-string
      (lambda ()
        (output (include/text p))))]))

(define-syntax include-template/xexpr
  (syntax-rules ()
    [(_ p)
     (string->xexpr (include-template p))]))

(define-syntax in
  (syntax-rules ()
    [(_ x xs e ...)
     (for/list ([x xs])
       (begin/text e ...))]))

(provide include-template
         in)
