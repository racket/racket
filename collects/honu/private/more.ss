#lang scheme

(require "honu-typed-scheme.ss"
         "literals.ss"
         (for-syntax syntax/parse
                     "literals.ss")
         (for-template "honu-typed-scheme.ss"
                       "literals.ss"
                       ))

(provide (all-defined-out))

(define-honu-syntax honu-syntax
  (lambda (stx ctx)
    (syntax-parse stx #:literals (semicolon #%parens)
      [(_ (#%parens expr ...) semicolon . rest)
       (values
         (lambda ()
           #'(honu-unparsed-begin expr ...))
         #'rest)])))

