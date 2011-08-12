#lang racket/base

(require "macro2.rkt"
         (for-syntax racket/base
                     "parse2.rkt"
                     "literals.rkt"
                     syntax/parse
                     unstable/syntax))

(provide honu-struct honu-struct? honu-struct-get)
(define-values (honu-struct honu-struct? honu-struct-get)
               (make-struct-type-property 'honu-struct))

(define-for-syntax (make-accessors name fields)
  (for/list ([field fields])
    (format-unique-id name "~a-~a" name field)))

(provide honu-structure)
(define-honu-syntax honu-structure
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name:id (#%braces fields:identifier-comma-list) . rest)
       (define out
         (with-syntax ([(fields.name/accessor ...)
                        (make-accessors #'name (syntax->list #'(fields.name ...)))])
           #'(struct name (fields.name ...)
                     #:property honu-struct (lambda (instance name)
                                              (case name
                                                [(fields.name) (fields.name/accessor instance)]
                                                ...
                                                [else (error 'dot "no such field name ~a" name)])))))
       (values out #'rest #t)])))

