#lang racket/base

(require syntax/parse
         "literals.rkt")

(provide (all-defined-out))

(define (honu->racket forms)
  (define-literal-set literals (%racket))
  (syntax-parse forms #:literal-sets (literals)
    [(%racket x) (honu->racket #'x)]
    [(form ...)
     (datum->syntax forms
                    (map honu->racket (syntax->list #'(form ...)))
                    forms
                    forms)]
    [x #'x]
    [() forms]))

(define (strip-stops code)
  (define-syntax-class stopper #:literal-sets (cruft)
    #;
    [pattern semicolon]
    [pattern honu-comma]
    [pattern colon])
  (syntax-parse code
    [(x:stopper rest ...) (strip-stops #'(rest ...))]
    [else code]))
