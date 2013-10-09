#lang scheme/base

(require
 "test-utils.rkt"
 "typecheck-tests.rkt" ;;pass

 "subtype-tests.rkt" ;; pass
 "type-equal-tests.rkt" ;; pass
 "remove-intersect-tests.rkt" ;; pass
 "parse-type-tests.rkt" ;; pass
 "subst-tests.rkt" ;; pass
 "infer-tests.rkt" ;; pass
 "type-annotation-test.rkt" ;; pass
 "keyword-expansion-test.rkt" ;;pass
 "special-env-typecheck-tests.rkt" ;;pass"

 "contract-tests.rkt"
 "interactive-tests.rkt"

 rackunit)

(provide unit-tests)


(define unit-tests
  (make-test-suite
   "Unit Tests"
   (list 
    typecheck-special-tests
    typecheck-tests
    subtype-tests
    type-equal-tests
    restrict-tests
    remove-tests
    overlap-tests
    parse-type-tests
    type-annotation-tests
    fv-tests
    contract-tests
    keyword-tests
    interactive-tests)))
