#lang racket/base

(require (except-in syntax/parse id keyword)
         (for-syntax syntax/parse
                     racket/base
                     (only-in racket/syntax generate-temporary)))

(provide (except-out (all-defined-out) id keyword)
         (rename-out [id id*] [keyword keyword*]))

(define-syntax (parse/get stx)
  (syntax-parse stx
    [(_ arg:expr attr:id pat)
     (let* ([i (generate-temporary)]
            [get-i (datum->syntax
                    i
                    (string->symbol
                     (string-append (symbol->string (syntax-e i))
                                    "."
                                    (symbol->string (syntax-e #'attr)))))])
       (quasisyntax/loc stx
         (syntax-parse arg
           [#,i #:declare #,i pat #'#,get-i])))]))

(define (atom? v)
  (or (number? v) (string? v) (boolean? v) (symbol? v) (char? v) (bytes? v) (regexp? v)))

(define-syntax-class (3d pred)
  (pattern s
           #:attr datum (syntax-e #'s)
           #:fail-unless (pred (attribute datum)) #f))

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes (datum)
    (pattern x
             #:fail-unless (pred (syntax-e #'x)) #f
             #:attr datum (syntax-e #'x))))

(define-pred-stxclass atom atom?)
(define-pred-stxclass byte-pregexp byte-pregexp?)
(define-pred-stxclass byte-regexp byte-regexp?)
(define-pred-stxclass pregexp pregexp?)
(define-pred-stxclass regexp regexp?)
(define-pred-stxclass bytes bytes?)
(define-pred-stxclass id symbol?)
(define-pred-stxclass keyword keyword?)
