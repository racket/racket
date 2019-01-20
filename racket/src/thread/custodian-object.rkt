#lang racket/base
(require "place-local.rkt")

(provide (struct-out custodian)
         create-custodian
         initial-place-root-custodian
         root-custodian)

(struct custodian (children     ; weakly maps maps object to callback
                   [shut-down? #:mutable]
                   [shutdown-sema #:mutable]
                   [need-shutdown #:mutable] ; queued asynchronous shutdown: #f, 'needed, or 'needed/sent-wakeup
                   [parent-reference #:mutable]
                   [place #:mutable]      ; place containing the custodian
                   [memory-use #:mutable] ; set after a major GC
                   [gc-roots #:mutable]   ; weak references to charge to custodian; access without interrupts
                   [memory-limits #:mutable] ; list of (cons limit cust)
                   [immediate-limit #:mutable]) ; limit on immediate allocation
  #:authentic)

(define (create-custodian)
  (custodian (make-weak-hasheq)
             #f     ; shut-down?
             #f     ; shutdown semaphore
             #f     ; need shutdown?
             #f     ; parent reference
             #f     ; place
             0      ; memory use
             #f     ; GC roots
             null   ; memory limits
             #f))   ; immediate limit

(define initial-place-root-custodian (create-custodian))

(define-place-local root-custodian initial-place-root-custodian)
