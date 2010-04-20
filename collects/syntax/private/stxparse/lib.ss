#lang racket/base

(require "sc.ss"
         "../util.ss"
         syntax/stx
         racket/struct-info
         unstable/srcloc
         (for-syntax racket/base
                     "rep.ss"
                     (only-in "rep-data.ss" make-literalset))
         (for-template racket/base
                       racket/contract/base))

(provide identifier
         boolean
         str
         character
         keyword
         number
         integer
         exact-integer
         exact-nonnegative-integer
         exact-positive-integer

         id
         nat
         char

         expr
         expr/c
         static
         atom-in-list

         kernel-literals)

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes () #:opaque
    (pattern x
             #:fail-unless (pred (syntax-e #'x)) #f)))

(define-pred-stxclass identifier symbol?)
(define-pred-stxclass boolean boolean?)
(define-pred-stxclass character char?)
(define-pred-stxclass keyword keyword?)

(define-syntax-class str #:attributes () #:opaque
  #:description "string"
  (pattern x
           #:fail-unless (string? (syntax-e #'x)) #f))

(define-pred-stxclass number number?)
(define-pred-stxclass integer integer?)
(define-pred-stxclass exact-integer exact-integer?)
(define-pred-stxclass exact-nonnegative-integer exact-nonnegative-integer?)
(define-pred-stxclass exact-positive-integer exact-positive-integer?)

;; Aliases
(define-syntax id (make-rename-transformer #'identifier))
(define-syntax nat (make-rename-transformer #'exact-nonnegative-integer))
(define-syntax char (make-rename-transformer #'character))

(define notfound (box 'notfound))

(define-syntax-class (static pred name)
  #:attributes (value)
  #:description name
  (pattern x:id
           #:fail-unless (syntax-transforming?)
                         "not within the extent of a macro transformer"
           #:attr value (syntax-local-value #'x (lambda () notfound))
           #:fail-when (eq? (attribute value) notfound) #f
           #:fail-unless (pred (attribute value)) #f))

(define-syntax-class (atom-in-list atoms name)
  #:attributes ()
  #:description name
  (pattern x
           #:fail-unless (memv (syntax-e #'x) atoms) #f))

(define-syntax-class struct-name
  #:description "struct name"
  #:attributes (descriptor
                constructor
                predicate
                [accessor 1]
                super
                complete?)
  (pattern s
           #:declare s (static struct-info? "struct name")
           #:with info (extract-struct-info (attribute s.value))
           #:with descriptor (list-ref (attribute info) 0)
           #:with constructor (list-ref (attribute info) 1)
           #:with predicate (list-ref (attribute info) 2)
           #:with r-accessors (reverse (list-ref (attribute info) 3))
           #:with (accessor ...)
                  (datum->syntax #f (let ([r-accessors (attribute r-accessors)])
                                      (if (and (pair? r-accessors) (eq? #f (car r-accessors)))
                                          (cdr r-accessors)
                                          r-accessors)))
           #:with super (list-ref (attribute info) 5)
           #:attr complete? (or (null? (attribute r-accessors))
                                (and (pair? (attribute r-accessors))
                                     (not (eq? #f (car (attribute r-accessors))))))))

(define-syntax-class expr
  #:attributes ()
  (pattern x
           #:fail-when (keyword? (syntax-e #'x)) #f))

(define-syntax-class (expr/c ctc)
  #:attributes (c)
  (pattern x:expr
           #:with
           c #`(contract #,ctc
                         x
                         (quote #,(source-location->string #'x "<<unknown>>"))
                         '<this-macro>
                         #f
                         (quote-syntax x))))

;; Literal sets

(define-literal-set kernel-literals
  (begin
   begin0
   define-values
   define-syntaxes
   define-values-for-syntax
   set!
   let-values
   letrec-values
   #%plain-lambda
   case-lambda
   if
   quote
   letrec-syntaxes+values
   with-continuation-mark
   #%expression
   #%plain-app
   #%top
   #%datum
   #%variable-reference
   module #%provide #%require
   #%plain-module-begin))
