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

 "module-tests.rkt" ;; pass
 "contract-tests.rkt"
 "interactive-tests.rkt"

 racket/runtime-path
 rackunit rackunit/text-ui)

(provide unit-tests)


(define-runtime-path special "special-env-typecheck-tests.rkt")

(define unit-tests
  (make-test-suite
   "Unit Tests"
   (for/list ([f (list
                  typecheck-tests
                  subtype-tests
                  type-equal-tests
                  restrict-tests
                  remove-tests
                  overlap-tests
                  parse-type-tests
                  type-annotation-tests
                  module-tests
                  fv-tests
                  contract-tests
                  keyword-tests
                  interactive-tests
                  ;; this uses dynamic require because the file fails to compile when there's a test failure
                  (λ () ((dynamic-require special 'typecheck-special-tests))))])
     (f))))



(define-go (lambda () unit-tests))

;(go/gui)


