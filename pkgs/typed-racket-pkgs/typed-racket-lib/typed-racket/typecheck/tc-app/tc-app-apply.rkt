#lang racket/unit

(require "../../utils/utils.rkt"
         "signatures.rkt"
         "utils.rkt"
         syntax/parse racket/match
         (typecheck signatures)
         (types abbrev utils)
         (rep type-rep)

         (for-label
          racket/base
          (only-in '#%kernel [apply k:apply])))


(import tc-expr^ tc-apply^)
(export tc-app-apply^)

(define-literal-set apply-literals
  #:for-label
  (k:apply apply values))

(define-tc/app-syntax-class (tc/app-apply expected)
  #:literal-sets (apply-literals)
  (pattern ((~or apply k:apply) (~and f values) e)
    (match (single-value #'e)
      [(tc-result1: (ListDots: dty dbound))
       (ret null null null dty dbound)]
      [(tc-result1: (List: ts)) (ret ts)]
      [_ (tc/apply #'f #'(e))]))
  (pattern ((~or apply k:apply) f . args)
    (tc/apply #'f #'args)))
