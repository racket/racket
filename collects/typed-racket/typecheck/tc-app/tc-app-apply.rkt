#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         syntax/parse/experimental/reflect
         (typecheck signatures tc-funapp check-below tc-subst)
         (types abbrev utils)
         (rep type-rep)

         ;; fixme - don't need to be bound in this phase - only to make tests work
         (only-in '#%kernel [apply k:apply])
         ;; end fixme
         (for-template
          racket/base
          (only-in '#%kernel [apply k:apply])))


(import tc-expr^ tc-apply^)
(export tc-app-apply^)

(define-tc/app-syntax-class (tc/app-apply expected)
  #:literals (k:apply apply values)
  (pattern ((~or apply k:apply) values e)
    (match (single-value #'e)
      [(tc-result1: (ListDots: dty dbound)) (values->tc-results (make-ValuesDots null dty dbound) #f)]
      [(tc-result1: (List: ts)) (ret ts)]
      [_ (tc/apply #'values #'(e))]))
  (pattern ((~or apply k:apply) f . args)
    (tc/apply #'f #'args)))
