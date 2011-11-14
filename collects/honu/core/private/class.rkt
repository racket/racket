#lang racket/base

(require "macro2.rkt"
         "literals.rkt"
         (only-in "honu2.rkt" honu-declaration honu-equal)
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
  (define-splicing-syntax-class honu-class-thing
                                #:literals (honu-equal)
    [pattern method:honu-function
             #:with result (replace-with-public #'method.result)]
    [pattern var:honu-declaration
             #:with result #'(field [var.name var.expression])]))

(provide honu-class)
(define-honu-syntax honu-class
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens constructor-argument ...) (#%braces method:honu-class-thing ...) . rest)
       (define class
         #'(%racket (define name (class* object% ()
                                         (super-new)
                                         (init-field constructor-argument ...)
                                         method.result ...))))
       (values class #'rest #t)])))

(provide honu-new)
(define-honu-syntax honu-new
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens arg:honu-expression/comma) . rest)
       (define new #'(%racket (make-object name arg.result ...)))
       (values
         new
         #'rest
         #f)])))
