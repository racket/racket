#lang racket/base

(require honu/core/private/syntax
         honu/core/private/literals
         (for-syntax syntax/parse
                     racket/base
                     honu/core/private/literals
                     honu/core/private/compile
                     honu/core/private/parse2))

(provide sqr)
(define (sqr x) (* x x))

;; convert a float to an integer
(provide integer)
(define (integer x)
  (inexact->exact (round x)))

(provide honu-cond)
(define-honu-syntax honu-cond
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (~seq clause:honu-expression colon body:honu-expression (~optional honu-comma)) ...
          . rest)
       (values
         (with-syntax ([(clause.result ...) (map honu->racket (syntax->list #'(clause.result ...)))]
                       [(body.result ...) (map honu->racket (syntax->list #'(body.result ...)))])
           #'(%racket (cond
                        [clause.result body.result]
                        ...)))
         #'rest
         #t)])))
