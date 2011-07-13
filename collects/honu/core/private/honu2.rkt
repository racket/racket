#lang racket/base

(require "macro2.rkt"
         "operator.rkt"
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

(provide honu-+)
(define-honu-operator/syntax honu-+ 1
                             (lambda (left right)
                               (with-syntax ([left left]
                                             [right right])
                                 #'(+ left right))))
