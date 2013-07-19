#lang racket/base
(require (for-template "parse.rkt"))
(provide id:define-syntax-class
         id:define-splicing-syntax-class
         id:define-integrable-syntax-class
         id:syntax-parse
         id:syntax-parser
         id:define/syntax-parse
         id:syntax-parser/template
         id:parser/rhs
         id:define-eh-alternative-set)

(define (id:define-syntax-class) #'define-syntax-class)
(define (id:define-splicing-syntax-class) #'define-splicing-syntax-class)
(define (id:define-integrable-syntax-class) #'define-integrable-syntax-class)
(define (id:syntax-parse) #'syntax-parse)
(define (id:syntax-parser) #'syntax-parser)
(define (id:define/syntax-parse) #'define/syntax-parse)
(define (id:syntax-parser/template) #'syntax-parser/template)
(define (id:parser/rhs) #'parser/rhs)
(define (id:define-eh-alternative-set) #'define-eh-alternative-set)
