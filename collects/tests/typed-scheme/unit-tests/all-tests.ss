#lang scheme/base

(require 
 "test-utils.ss"
 "subtype-tests.ss" ;; done
 "type-equal-tests.ss" ;; done
 "remove-intersect-tests.ss" ;; done
 "parse-type-tests.ss" ;; done
 "type-annotation-test.ss" ;; done
 "typecheck-tests.ss"
 "module-tests.ss"
 "infer-tests.ss")

(require (for-syntax scheme/base mzlib/etc))

(require (for-syntax (private utils)))

(require (private planet-requires))

(require (schemeunit))

(provide unit-tests)

(define unit-tests
  (apply
   test-suite 
   "Unit Tests"
   (map (lambda (f) (f))
        (list
         subtype-tests
         type-equal-tests
         restrict-tests
         remove-tests
         parse-type-tests
         type-annotation-tests
         typecheck-tests
         module-tests
         fv-tests))))



(define-go
  subtype-tests
  type-equal-tests
  restrict-tests
  remove-tests
  parse-type-tests
  type-annotation-tests
  typecheck-tests
  module-tests
  fv-tests) 

(define (fast)
  (run     
   subtype-tests
   type-equal-tests
   restrict-tests
   remove-tests
   parse-type-tests
   type-annotation-tests
   typecheck-tests
   module-tests
   fv-tests))

(define (faster)
  (run     
   subtype-tests
   type-equal-tests
   restrict-tests
   remove-tests
   parse-type-tests
   type-annotation-tests
   fv-tests))

;(go/gui)


