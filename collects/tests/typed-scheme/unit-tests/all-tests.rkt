#lang scheme/base

(require
 "test-utils.rkt"
 "typecheck-tests.rkt" ;;fail

 "subtype-tests.rkt" ;; pass
 "type-equal-tests.rkt" ;; pass
 "remove-intersect-tests.rkt" ;; pass
 "parse-type-tests.rkt" ;; pass
 "subst-tests.rkt" ;; pass
 "infer-tests.rkt" ;; pass
 "type-annotation-test.rkt" ;; pass

 "module-tests.rkt" ;; pass
 "contract-tests.rkt"

 (r:infer infer infer-dummy)
 rackunit rackunit/text-ui)

(provide unit-tests)

(infer-param infer)

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
                  contract-tests)])
     (f))))



(define-go (lambda () unit-tests))

;(go/gui)


