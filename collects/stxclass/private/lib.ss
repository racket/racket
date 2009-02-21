#lang scheme/base

(require "sc.ss"
         "../util.ss"
         syntax/stx
         syntax/kerncase
         scheme/struct-info
         scheme/private/contract-helpers
         (for-syntax scheme/base
                     "rep.ss")
         (for-template scheme/base
                       scheme/contract))
(provide (all-defined-out))

(define-syntax-rule (define-pred-stxclass name pred)
  (define-syntax-class name #:attributes ([datum 0])
    (pattern x
             #:with datum (if (syntax? #'x) (syntax-e #'x) #'x)
             #:when (pred (attribute datum)))))

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

(define-syntax-class (static-of name pred)
  #:attributes (value)
  (pattern x:id
           #:with value-list (syntax-local-value* #'x)
           #:when (pair? (attribute value-list))
           #:with value (car (attribute value-list))
           #:when (pred (attribute value))))

(define (syntax-local-value* id)
  (let/ec escape
    (list (syntax-local-value id (lambda () (escape null))))))

(define-syntax-class static #:attributes (value)
  (pattern x
           #:declare x (static-of "static" (lambda _ #t))
           #:with value #'x.value))

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
           #:with complete? (or (null? (attribute r-accessors))
                                (and (pair? (attribute r-accessors))
                                     (not (eq? #f (car (attribute r-accessors))))))))

(define-syntax-class expr/local-expand
  #:attributes (expanded)
  (pattern x
           #:with expanded (local-expand #'x 'expression null)))

(define-syntax-class expr/head-local-expand
  #:attributes (expanded)
  (pattern x
           #:with expanded (local-expand #'x 'expression (kernel-form-identifier-list))))

(define-syntax-class block/head-local-expand
  #:attributes (expanded-block
                [expanded 1]
                [def 1]
                [vdef 1]
                [sdef 1]
                [expr 1])
  (pattern x
           #:with (expanded-block (expanded ...) (def ...) (vdef ...) (sdef ...) (expr ...))
                  (datum->syntax #f
                                 (let-values ([(ex1 ex2 defs vdefs sdefs exprs)
                                               (head-local-expand-and-categorize-syntaxes
                                                #'x #f #| #t |#)])
                                   (list ex1 ex2 defs vdefs sdefs exprs)))))

(define-syntax-class internal-definitions
  #:attributes (expanded-block
                [expanded 1]
                [def 1]
                [vdef 1]
                [sdef 1]
                [expr 1])
  (pattern x
           #:with (expanded-block (expanded ...) (def ...) (vdef ...) (sdef ...) (expr ...))
                  (datum->syntax #f
                                 (let-values ([(ex1 ex2 defs vdefs sdefs exprs)
                                               (head-local-expand-and-categorize-syntaxes
                                                #'x #t #| #f |#)])
                                   (list ex1 ex2 defs vdefs sdefs exprs)))))

(define-syntax-class expr
  #:attributes ()
  (pattern x
           #:when (and (syntax? #'x) (not (keyword? (syntax-e #'x))))))


;; FIXME: hack
(define expr/c-use-contracts? (make-parameter #t))

(define-syntax-class (expr/c ctc)
  #:attributes (c)
  (pattern x:expr
           #:with c #`(contract #,ctc
                                x
                                (quote #,(string->symbol (or (build-src-loc-string #'x) "")))
                                (quote #,(or (current-macro-name) '<this-macro>))
                                (quote-syntax #,(syntax/loc #'x (<there>))))))

;; Aliases

(define-syntax id (make-rename-transformer #'identifier))
(define-syntax nat (make-rename-transformer #'exact-nonnegative-integer))
(define-syntax char (make-rename-transformer #'character))
