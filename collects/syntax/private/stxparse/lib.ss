#lang scheme/base

(require "sc.ss"
         "../util.ss"
         syntax/stx
         syntax/kerncase
         scheme/struct-info
         scheme/private/contract-helpers
         (for-syntax scheme/base
                     syntax/kerncase
                     "rep.ss"
                     (only-in "rep-data.ss" make-literalset))
         (for-template scheme/base
                       scheme/contract))
(provide (all-defined-out))

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes ()
    (pattern x
             #:fail-unless (pred (syntax-e #'x)) #f)))

(define-pred-stxclass identifier symbol?)
(define-pred-stxclass boolean boolean?)
(define-pred-stxclass str string?)
(define-pred-stxclass character char?)
(define-pred-stxclass keyword keyword?)

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

(define-syntax-class (static-of pred name)
  #:attributes (value)
  (pattern x:id
           #:fail-unless (syntax-transforming?)
                         "not within the extent of a macro transformer"
           #:attr value (syntax-local-value #'x (lambda () notfound))
           #:fail-when (eq? (attribute value) notfound) #f))

(define-syntax-class static #:attributes (value)
  (pattern x
           #:declare x (static-of (lambda _ #t) "static")
           #:attr value (attribute x.value)))

(define-syntax-class struct-name
  #:description "struct name"
  #:attributes (descriptor
                constructor
                predicate
                [accessor 1]
                super
                complete?)
  (pattern s
           #:declare s (static-of "struct name" struct-info?)
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

(define-syntax kernel-literals
  (make-literalset
   (for/list ([id (kernel-form-identifier-list)])
     (list (syntax-e id) id))))
