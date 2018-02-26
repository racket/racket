#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt"
         "namespace-scope.rkt")

;; Compilation of top-level forms generates a link that has an
;; `eager-instance` argument to receive deserialization information: a
;; namspace, its phase, etc.

(provide eager-instance-imports
         make-eager-instance-instance
         empty-eager-instance-instance)

(define eager-instance-imports
  `(,ns-id
    ,dest-phase-id
    ,self-id
    ,bulk-binding-registry-id
    ,inspector-id
    swap-top-level-scopes))

(define (make-eager-instance-instance #:namespace ns
                                      #:dest-phase dest-phase
                                      #:self self 
                                      #:bulk-binding-registry bulk-binding-registry
                                      #:inspector inspector)
  (make-instance 'instance #f 'constant
                 ns-id ns
                 dest-phase-id dest-phase
                 self-id self
                 bulk-binding-registry-id bulk-binding-registry
                 inspector-id inspector
                 'swap-top-level-scopes swap-top-level-scopes))

(define empty-eager-instance-instance
  (make-eager-instance-instance #:namespace #f
                                #:dest-phase #f
                                #:self #f
                                #:bulk-binding-registry #f
                                #:inspector #f))
