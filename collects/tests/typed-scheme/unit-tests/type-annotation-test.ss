#lang scheme/base
(require "test-utils.ss"
         (for-syntax scheme/base))
(require (private planet-requires type-annotation tc-utils type-rep type-effect-convenience type-environments
		 parse-type  init-envs type-name-env))

(require (schemeunit))

(provide type-annotation-tests)

(define-syntax (tat stx)
  (syntax-case stx ()
    [(_ ann-stx ty)
     #`(check-type-equal? #,(format "~a" (syntax->datum #'ann-stx))  
                          (type-annotation #'ann-stx)
			  ty)]))

#reader typed-scheme/typed-reader
(define (type-annotation-tests)
  (test-suite 
   "Type Annotation tests"
   
   (tat #{foo : Number} N)
   (tat foo #f)
   (tat #{foo : 3} (-val 3))))

(define-go
  type-annotation-tests)

