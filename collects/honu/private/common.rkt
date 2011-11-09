#lang racket/base

(require honu/core/private/macro2
         honu/core/private/literals
         (for-syntax syntax/parse
                     racket/base
                     honu/core/private/literals
                     honu/core/private/parse2))

(provide sqr)
(define (sqr x) (* x x))

(provide honu-cond)
(define-honu-syntax honu-cond
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (~seq clause:honu-expression colon body:honu-expression (~optional honu-comma)) ...
          . rest)
       (values
         (with-syntax ([(clause-parsed ...) (map (lambda (clause)
                                                   (parse-all clause))
                                                 (syntax->list #'(clause.result ...)))]
                       [(body-parsed ...) (map (lambda (body)
                                                 (parse-all body))
                                               (syntax->list #'(body.result ...)))])
           #'(%racket-expression (cond
                                   [clause-parsed body-parsed]
                                   ...)))
         #'rest
         #t)])))
