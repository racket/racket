#lang racket/base

(require syntax/parse
         "unit-compiletime.rkt"
         "unit-keywords.rkt"
         (for-template "unit-keywords.rkt" racket/base racket/contract))

(provide import-clause/contract export-clause/contract body-clause/contract dep-clause
         import-clause/c export-clause/c body-clause/c)

(define-syntax-class sig-spec #:literals (prefix rename only except)
  #:attributes ((name 0))
  #:transparent
  (pattern name)
  (pattern (prefix i:identifier s:sig-spec)
           #:with name #'s.name)
  (pattern (rename s:sig-spec [int:identifier ext:identifier] ...)
           #:with name #'s.name)
  (pattern (only s:sig-spec i:identifier ...)
           #:with name #'s.name)
  (pattern (except s:sig-spec i:identifier ...)
           #:with name #'s.name))

(define-syntax-class tagged-sig-spec #:literals (tag)
  #:auto-nested-attributes
  #:transparent
  (pattern s:sig-spec
           #:with i #f)
  (pattern (tag i:identifier s:sig-spec)))

(define-syntax-class tagged-sig-id #:literals (tag)
  #:attributes ()
  #:transparent
  (pattern s)
  (pattern (tag i:identifier s)))

(define-syntax-class unit/c-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-id [x:identifier c:expr] ...))
  (pattern s:tagged-sig-id ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/c #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/c-clause ...)))
(define-syntax-class export-clause/c #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/c-clause ...)))
(define-splicing-syntax-class body-clause/c
  #:literals (values)
  #:auto-nested-attributes
  #:transparent
  (pattern (~seq)
           #:attr name #'()
           #:attr apply-invoke-ctcs
           (lambda (id blame) id))
  (pattern (values ctc:expr ...)
           #:attr name #'('(values ctc ...))
           #:attr apply-invoke-ctcs
           ;; blame here is really syntax representing a blame object
           (lambda (id blame)
             (define len (length (syntax->list #'(ctc ...))))
             #`(call-with-values (lambda () #,id)
                 (lambda args
                   (unless (= #,len (length args))
                     (raise-blame-error (blame-add-context #,blame "the body of")
                                        (blame-value #,blame)
                                        (format "expected ~a values, returned ~a"
                                                #,len (length args))))
                   (apply values
                          (map
                           (lambda (c arg)
                             (((contract-projection c)
                                (blame-add-context #,blame "the body of"))
                              arg))
                           (list ctc ...)
                           args))))))
  (pattern b:expr
           #:attr name #'('b)
           #:attr apply-invoke-ctcs
           (lambda (id blame)
             #`(((contract-projection b)
                  (blame-add-context #,blame "the body of"))
                #,id))))

(define-syntax-class unit/contract-clause
  #:auto-nested-attributes
  #:transparent
  (pattern (s:tagged-sig-spec [x:identifier c:expr] ...))
  (pattern s:tagged-sig-spec ;; allow a non-wrapped sig, which is the same as (sig)
           #:with (x ...) null
           #:with (c ...) null))
(define-syntax-class import-clause/contract #:literals (import)
  #:auto-nested-attributes
  #:transparent
  (pattern (import i:unit/contract-clause ...)))
(define-syntax-class export-clause/contract #:literals (export)
  #:auto-nested-attributes
  #:transparent
  (pattern (export e:unit/contract-clause ...)))
(define-syntax-class dep-clause #:literals (init-depend)
  #:auto-nested-attributes
  #:transparent
  (pattern (init-depend s:tagged-sig-id ...)))
(define-splicing-syntax-class body-clause/contract
  #:auto-nested-attributes
  #:transparent
  (pattern (~seq #:invoke/contract b:expr)))
