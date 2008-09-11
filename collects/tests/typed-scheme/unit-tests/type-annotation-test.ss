#lang scheme/base
(require "test-utils.ss"
         (for-syntax scheme/base))
(require (private type-annotation type-effect-convenience parse-type)
	 (env type-environments type-name-env init-envs)
	 (utils planet-requires tc-utils)
	 (rep type-rep))

(require (schemeunit))

(provide type-annotation-tests)

(define-syntax-rule (tat ann-stx ty)
  (check-type-equal? (format "~a" (quote ann-stx))  
                     (type-ascription (let ([ons (current-namespace)]
                                            [ns (make-empty-namespace)])
                                        (parameterize ([current-namespace ns])
                                          (namespace-attach-module ons 'scheme/base ns)
                                          (namespace-require 'scheme/base)
                                          (namespace-require 'typed-scheme/private/prims)
                                          (expand 'ann-stx))))
                     ty))

#reader typed-scheme/typed-reader
(define (type-annotation-tests)
  (test-suite 
   "Type Annotation tests"
   
   (tat (ann foo : Number) N)
   (tat foo #f)
   (tat (ann foo : 3) (-val 3))))

(define-go
  type-annotation-tests)

