#lang scheme/base
(require "test-utils.rkt"
         (for-syntax scheme/base)
         typed-racket/private/type-annotation
         typed-racket/private/parse-type
         (types abbrev numeric-tower utils)
         (env type-env-structs init-envs)
         (utils tc-utils)
         (rep type-rep filter-rep object-rep)
         (submod typed-racket/base-env/base-types initialize)
         rackunit)

(initialize-type-names)

(provide type-annotation-tests)

(define-syntax-rule (tat ann-stx ty)
  (check-tc-result-equal? (format "~a" (quote ann-stx))
                          (type-ascription (let ([ons (current-namespace)]
                                                 [ns (make-base-namespace)])
                                             (parameterize ([current-namespace ns])
                                               (namespace-require 'typed-racket/base-env/prims)
                                               (namespace-require 'typed-racket/base-env/base-types)
                                               (namespace-require 'typed-racket/base-env/base-types-extra)
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



