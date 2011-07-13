#lang racket/base

(require "macro2.rkt"
         (for-syntax syntax/parse
                     "literals.rkt"
                     "parse2.rkt"
                     racket/base))


(provide honu-function)
(define-honu-syntax honu-function
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name:identifier (#%parens arg:identifier ...)
          (#%braces code ...)
          . rest)
       (values
         #'(define (name arg ...)
             (let-syntax ([do-parse (lambda (stx)
                                      (parse #'(code ...)))])
               (do-parse)))
         #'rest)])))
