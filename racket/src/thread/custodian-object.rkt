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
                   [self-reference #:mutable]
                   [place #:mutable]      ; place containing the custodian
                   [memory-use #:mutable] ; set after a major GC
                   [gc-roots #:mutable]   ; weak references to charge to custodian; access without interrupts
                   [memory-limits #:mutable]   ; list of (cons limit cust)
                   [immediate-limit #:mutable] ; limit on immediate allocation
                   [sync-futures? #:mutable])  ; whether a sync witht future threads is needed on shutdown
  #:authentic)

(define (create-custodian parent)
  (custodian (make-weak-hasheq)
             #f     ; shut-down?
             #f     ; shutdown semaphore
             #f     ; need shutdown?
             #f     ; parent reference
             #f     ; self reference
             #f     ; place
             0      ; memory use
             #f     ; GC roots
             null   ; memory limits
             #f     ; immediate limit
             #f))   ; sync-futures?

(define initial-place-root-custodian (create-custodian #f))

(define-place-local root-custodian initial-place-root-custodian)
