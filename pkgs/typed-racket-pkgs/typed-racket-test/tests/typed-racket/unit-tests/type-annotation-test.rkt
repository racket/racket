#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/list
           (rep type-rep filter-rep object-rep)
           (private type-annotation)
           (types abbrev numeric-tower tc-result))
         (only-in typed-racket/typed-racket do-standard-inits)
         (base-env prims base-types base-types-extra colon)
         rackunit)

(provide tests)
(gen-test-main)

(begin-for-syntax
  (do-standard-inits))


(define-syntax-rule (tat ann-stx ty)
  (test-case (format "~a" (quote ann-stx))
    (unless
      (phase1-phase0-eval
        (define stx (local-expand (quote-syntax ann-stx) 'expression empty))
        (define ascrip (type-ascription stx))
        #`#,(equal? ascrip ty))
      (fail-check "Unequal types"))))

(define tests
  (test-suite
   "Type Annotation tests"
   ;; FIXME - ask Ryan
   (tat (ann foo : Number) (ret -Number -no-filter -no-obj))
   (tat foo #f)
   (tat (ann foo : 3) (ret (-val 3) -no-filter -no-obj))))
