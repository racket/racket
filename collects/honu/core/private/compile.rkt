#lang racket/base

(require syntax/parse
         "literals.rkt"
         (for-template racket/base))

(provide honu->racket)
(define (honu->racket forms)
  (define-literal-set literals (%racket))
  ;; (debug "honu to racket ~a\n" (pretty-format (syntax->datum forms)))
  (syntax-parse forms #:literal-sets (literals)
    [(%racket x) (honu->racket #'x)]
    [(form ...)
     (with-syntax ([(form* ...) (map honu->racket (syntax->list #'(form ...)))])
       #'(form* ...))]
    [x #'x]
    [() forms]))

