#lang racket/base
(require "place-local.rkt")

(provide (struct-out custodian)
         create-custodian

         custodian-shut-down?
         custodian-shut-down?/other-pthread
         set-custodian-shut-down!

         initial-place-root-custodian
         root-custodian)

(struct custodian (children     ; weakly maps maps object to callback
                   shut-down?-box ; box of boolean
                   [shutdown-sema #:mutable]
                   [need-shutdown #:mutable] ; queued asynchronous shutdown: #f, 'needed, or 'needed/sent-wakeup
                   [parent-reference #:mutable]
                   [self-reference #:mutable]
                   [place #:mutable]      ; place containing the custodian
                   [memory-use #:mutable] ; set after a major GC
                   [gc-roots #:mutable]   ; weak references to charge to custodian; access without interrupts
                   [memory-limits #:mutable]   ; list of (cons limit #f-or-cust) where #f means "self"
                   [immediate-limit #:mutable] ; limit on immediate allocation
                   [sync-futures? #:mutable]   ; whether a sync with future threads is needed on shutdown
                   [post-shutdown #:mutable])  ; callbacks to run in atomic mode after shutdown
  #:authentic)

(define (create-custodian parent)
  (custodian (make-weak-hasheq)
             (box #f) ; shut-down?-box
             #f     ; shutdown semaphore
             #f     ; need shutdown?
             #f     ; parent reference
             #f     ; self reference
             #f     ; place
             0      ; memory use
             #f     ; GC roots
             null   ; memory limits
             #f     ; immediate limit
             #f     ; sync-futures?
             null)) ; post-shutdown

;; Call only from a place's main pthread, and only a
;; place's main thread should change the box value.
(define (custodian-shut-down? c)
  (unbox* (custodian-shut-down?-box c)))

;; Call only from a place's main pthread
(define (set-custodian-shut-down! c)
  (unless (box-cas! (custodian-shut-down?-box c) #f #t)
    (set-custodian-shut-down! c)))

;; Call from other pthreads to make sure they synchronize
;; enough with the place's main pthread:
(define (custodian-shut-down?/other-pthread c)
  (cond
    [(box-cas! (custodian-shut-down?-box c) #f #f)
     #f]
    [(box-cas! (custodian-shut-down?-box c) #t #t)
     #t]
    [else (custodian-shut-down?/other-pthread c)]))

(define initial-place-root-custodian (create-custodian #f))

(define-place-local root-custodian initial-place-root-custodian)
