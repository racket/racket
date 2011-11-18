#lang racket/base

(require syntax/parse
         "literals.rkt")

(provide honu->racket)
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

