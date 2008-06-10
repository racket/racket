#lang scheme/base

(require 
 "test-utils.ss"
 "typecheck-tests.ss"
 "subtype-tests.ss" ;; done
 "type-equal-tests.ss" ;; done
 "remove-intersect-tests.ss" ;; done
 "parse-type-tests.ss" ;; done
 "type-annotation-test.ss" ;; done
 "module-tests.ss"
 "subst-tests.ss"
 "infer-tests.ss")

(require (private planet-requires infer infer-dummy))

(require (schemeunit))

(provide unit-tests)

(infer-param infer)

(define unit-tests
  (apply
   test-suite 
   "Unit Tests"
   (for/list ([f (list
                  typecheck-tests
                  subtype-tests
                  type-equal-tests
                  restrict-tests
                  remove-tests
                  parse-type-tests
                  type-annotation-tests
                  module-tests
                  fv-tests)])
     (f))))



(define-go (lambda () unit-tests))

;(go/gui)


