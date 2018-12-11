#lang racket/base
(require "reserved-symbol.rkt"
         "../host/linklet.rkt")

;; Compilation generates a linklet that has an `instance` argument to
;; receive instantiation information: a namspace, its phase, etc.

(provide instance-imports
         make-instance-instance
         make-module-body-instance-instance
         empty-syntax-literals-instance
         empty-module-body-instance
         empty-syntax-literals-data-instance
         empty-top-syntax-literal-instance
         empty-instance-instance)

(define instance-imports
  `(,ns-id
    ,phase-shift-id
    ,self-id
    ,inspector-id               ; declaration-time inspector to grant to syntax objects
    ,bulk-binding-registry-id   ; declaration-time registry to connect to bulk bindings
    ,set-transformer!-id))

(define (make-instance-instance #:namespace ns
                                #:phase-shift phase-shift
                                #:self self 
                                #:inspector inspector
                                #:bulk-binding-registry bulk-binding-registry
                                #:set-transformer! set-transformer!)
  (make-instance 'instance #f 'constant
                 ns-id ns
                 phase-shift-id phase-shift
                 self-id self
                 inspector-id inspector
                 bulk-binding-registry-id bulk-binding-registry
                 set-transformer!-id set-transformer!))

(define (make-module-body-instance-instance #:set-transformer! set-transformer!)
  (make-instance 'body-instance #f 'constant
                 set-transformer!-id set-transformer!))

(define empty-syntax-literals-instance
  (make-instance 'empty-stx #f 'constant
                 get-syntax-literal!-id (lambda (pos) #f)
                 'get-encoded-root-expand-ctx #f))
(void (instance-describe-variable! empty-syntax-literals-instance
                                   get-syntax-literal!-id
                                   '(procedure/succeeds 2)))

(define empty-module-body-instance
  (make-module-body-instance-instance #:set-transformer! (lambda (name val) (void))))
(void (instance-describe-variable! empty-module-body-instance
                                   set-transformer!-id
                                   '(procedure/succeeds 4)))

(define empty-top-syntax-literal-instance
  (make-instance 'top-syntax-literal #f 'constant
                 mpi-vector-id #f
                 syntax-literals-id #f))

(define empty-syntax-literals-data-instance
  (make-instance 'empty-stx-data #f 'constant
                 deserialized-syntax-vector-id (vector)
                 deserialize-syntax-id void))

(define empty-instance-instance
  (make-instance-instance #:namespace #f
                          #:phase-shift #f
                          #:self #f
                          #:inspector #f
                          #:bulk-binding-registry #f
                          #:set-transformer! #f))
