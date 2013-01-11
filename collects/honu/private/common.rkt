#lang racket/base

(require honu/core/private/syntax
         honu/core/private/literals
         (for-syntax syntax/parse
                     honu/core/private/debug
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
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (~seq clause:honu-expression colon body:honu-expression (~optional honu-comma)) ...
          . rest)
       (values
         (racket-syntax (cond
                          [clause.result body.result]
                          ...))
         #'rest
         #t)])))

(provide honu-time)
(define-honu-syntax honu-time
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ e:honu-expression . rest)
       (values (racket-syntax (time e.result))
               #'rest
               #'t)])))
