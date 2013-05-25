#lang racket/base

(require syntax/parse
         "literals.rkt")

(provide (all-defined-out))

(define (strip-stops code)
  (define-syntax-class stopper #:literal-sets (cruft)
    #;
    [pattern semicolon]
    [pattern honu-comma]
    [pattern colon])
  #;
  (syntax-parse code
    [(x:stopper rest ...) (strip-stops #'(rest ...))]
    [else code])
  code
  )
