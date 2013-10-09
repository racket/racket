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

 "contract-tests.rkt"
 "interactive-tests.rkt"

 racket/runtime-path
 rackunit)

(provide unit-tests)

(define-runtime-path special "special-env-typecheck-tests.rkt")

(define unit-tests
  (make-test-suite
   "Unit Tests"
   (list 
    ;; this uses dynamic require because the file fails to compile when there's a test failure
    (dynamic-require special 'typecheck-special-tests)
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
