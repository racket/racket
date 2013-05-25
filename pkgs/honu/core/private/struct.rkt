#lang racket/base

(require "syntax.rkt"
         "literals.rkt"
         (for-syntax racket/base
                     "parse2.rkt"
                     "literals.rkt"
                     "compile.rkt"
                     syntax/parse
                     racket/syntax
                     unstable/syntax))

(provide honu-struct honu-struct? honu-struct-get)
(define-values (honu-struct honu-struct? honu-struct-get)
               (make-struct-type-property 'honu-struct))

(define-values (honu-struct-mutable honu-struct-mutable? honu-struct-mutate)
               (make-struct-type-property 'honu-struct-mutable))

(define-for-syntax (make-accessors name fields)
  (for/list ([field fields])
    (format-id name "~a-~a" name field)))

(define-for-syntax (make-mutators name fields)
  (for/list ([field fields])
    (format-id name "set-~a-~a!" name field)))

(provide honu-struct-set!)
(define (honu-struct-set! instance name value)
  ((honu-struct-mutate instance) instance name value))

(provide honu-structure)
(define-honu-syntax honu-structure
  (lambda (code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name:id (#%braces fields:identifier-comma-list) . rest)
       (define out
         (with-syntax ([(fields.name/accessor ...)
                        (make-accessors #'name (syntax->list #'(fields.name ...)))]
                       [(fields.name/mutator ...)
                        (make-mutators #'name (syntax->list #'(fields.name ...)))])
           (racket-syntax (struct name (fields.name ...)
                              #:transparent
                              #:mutable
                              #:property honu-struct-mutable
                                (lambda (instance name value)
                                  (case name
                                    [(fields.name) (fields.name/mutator instance value)]
                                    ...
                                    [else (error 'dot "no such field name ~a" name)]))
                              #:property honu-struct (lambda (instance name)
                                                       (case name
                                                         [(fields.name) (fields.name/accessor instance)]
                                                         ...
                                                         [else (error 'dot "no such field name ~a" name)]))))))
       (values out #'rest #t)])))

