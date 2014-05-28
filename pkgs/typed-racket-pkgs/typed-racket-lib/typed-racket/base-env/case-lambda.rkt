#lang racket/base

;; This file is logically part of "prims.rkt" but is in a separate
;; file to avoid cyclic module dependencies
;;
;; In particular, "parse-type.rkt" needs the binding of the TR
;; case-lambda in order to match for case-lambda types.

(require (for-syntax "annotate-classes.rkt"
                     "../private/syntax-properties.rkt"
                     racket/base
                     syntax/parse))

(provide (rename-out [-case-lambda case-lambda]
                     [-case-lambda case-lambda:])
         pcase-lambda:)

(begin-for-syntax
  (define-syntax-class case-lambda-formals
    (pattern (~or (formal:optionally-annotated-formal ... . rst:rest-arg)
                  (~and (formal:optionally-annotated-formal ...)
                        (~bind [rst.form #'()])))
             #:with form
             (syntax/loc this-syntax
               (formal.ann-name ... . rst.form)))))

(define-syntax (-case-lambda stx)
  (syntax-parse stx
    [(_ vars:maybe-lambda-type-vars
        [formals:case-lambda-formals . body] ...)
     (quasisyntax/loc stx
       (#%expression
        #,(plambda-property
           (syntax/loc stx
             (case-lambda [formals.form . body] ...))
           (attribute vars.type-vars))))]))

(define-syntax (pcase-lambda: stx)
  (syntax-parse stx
    [(pcase-lambda: tvars:type-variables cl ...)
     (plambda-property
       (syntax/loc stx (-case-lambda cl ...))
       #'(tvars.vars ...))]))

