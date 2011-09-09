#lang racket/base

(require "macro2.rkt"
         (for-syntax racket/base
                     "literals.rkt"
                     "parse2.rkt"
                     syntax/parse)
         racket/class)

(begin-for-syntax
  (define (replace-with-public method)
    (syntax-parse method #:literals (define)
      [(define (name args ...) body ...)
       #'(define/public (name args ...) body ...)]))
  (define-splicing-syntax-class honu-class-method
    [pattern method:honu-function
             #:with result (replace-with-public #'method.result)]))

(provide honu-class)
(define-honu-syntax honu-class
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens constructor-argument ...) (#%braces method:honu-class-method ...) . rest)
       (define class
         #'(define name (class* object% ()
                                (super-new)
                                (init-field constructor-argument ...)
                                method.result ...)))
       (values
         class
         #'rest
         #t)])))

(provide honu-new)
(define-honu-syntax honu-new
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens arg ...) . rest)
       (define new #'(make-object name arg ...))
       (values
         new
         #'rest
         #f)])))
