#lang scheme/base
(require "test-utils.ss"
         (for-syntax scheme/base)
         typed-scheme/private/type-annotation
         typed-scheme/private/parse-type
         (types abbrev utils)
	 (env type-environments  init-envs)
	 (utils tc-utils)
	 (rep type-rep filter-rep object-rep)
         rackunit)

(provide type-annotation-tests)

(define-syntax-rule (tat ann-stx ty)
  (check-tc-result-equal? (format "~a" (quote ann-stx))  
                          (type-ascription (let ([ons (current-namespace)]
                                                 [ns (make-base-namespace)])
                                             (parameterize ([current-namespace ns])
                                               (namespace-require 'typed-scheme/private/prims)
                                               (namespace-require 'typed-scheme/private/base-types)
                                               (namespace-require 'typed-scheme/private/base-types-extra)
                                               (expand 'ann-stx))))
                          ty))

(define (type-annotation-tests)
  (test-suite 
   "Type Annotation tests"
   ;; FIXME - ask Ryan
   (tat (ann foo : Number) (ret -Number (make-NoFilter) (make-NoObject)))
   (tat foo #f)
   (tat (ann foo : 3) (ret (-val 3) (make-NoFilter) (make-NoObject)))))

(define-go
  type-annotation-tests)



