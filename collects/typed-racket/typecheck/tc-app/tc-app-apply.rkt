#lang racket/unit

(require "../../utils/utils.rkt"
         syntax/parse racket/match
         (typecheck signatures tc-app-helper tc-funapp check-below tc-subst)
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

(define (tc/app-apply form expected)
  (syntax-parse form
    #:literals (#%plain-app k:apply apply values)
    [(#%plain-app op:special-op args ...) #f]
    ;; rewrite this so that it takes advantages of all the special cases
    [(#%plain-app k:apply . args)
     (tc/app-apply (syntax/loc form (#%plain-app apply . args)) expected)]
    ;; (apply values l) gets special handling
    ;; Needs to be above the general apply checking
    [(#%plain-app apply values e)
     (match (single-value #'e)
       [(tc-result1: (ListDots: dty dbound)) (values->tc-results (make-ValuesDots null dty dbound) #f)]
       [(tc-result1: (List: ts)) (ret ts)]
       [_ (tc/apply #'values #'(e))])]
    ;; handle apply specially
    [(#%plain-app apply f . args) (tc/apply #'f #'args)]
    [_ #f]))

