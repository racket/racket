#lang racket/base
(require "place-local.rkt"
         "atomic.rkt"
         (only-in "host.rkt"
                  host:unsafe-make-weak-hasheq
                  host:make-mutex
                  host:mutex-acquire
                  host:mutex-release
                  assert-push-lock-level!
                  assert-pop-lock-level!))

(provide (struct-out custodian)
         create-custodian

         custodian-shut-down?
         custodian-shut-down?/other-pthread
         set-custodian-shut-down!

         initial-place-root-custodian
         root-custodian

         lock-custodians
         unlock-custodians
         init-custodian-lock!
         assert-custodians-locked)

;; The custodian lock cannot be taken inside a region where gc-interrupts are disabled.
;; Other locks cannot be taken inside the custodian lock.
;; The custodian lock implies uninterrupible mode.
(define-place-local custodian-lock (host:make-mutex))

(define (lock-custodians)
  (start-uninterruptible)
  (assert-push-lock-level! 'custodian)
  (host:mutex-acquire custodian-lock))
(define (unlock-custodians)  
  (host:mutex-release custodian-lock)
  (assert-pop-lock-level! 'custodian)
  (end-uninterruptible))

(define (init-custodian-lock!)
  (set! custodian-lock (host:make-mutex)))

;; No way to check this, currently:
(define-syntax-rule (assert-custodians-locked)
  (begin))

(struct custodian (children     ; weakly maps maps object to callback
                   shut-down?-box ; box of boolean
                   [shutdown-sema #:mutable]
                   [need-shutdown #:mutable] ; queued asynchronous shutdown: #f, 'needed, or 'needed/sent-wakeup; lock with atomic/no-gc
                   [parent-reference #:mutable]
                   [self-reference #:mutable]
                   [place #:mutable]      ; place containing the custodian
                   [memory-use #:mutable] ; set after a major GC; lock with atomic/no-gc
                   [gc-roots #:mutable]   ; weak references to charge to custodian; access without interrupts; lock with atomic/npo-gc
                   [memory-limits #:mutable]   ; list of (cons limit #f-or-cust) where #f means "self"; lock with atomic/no-gc
                   [immediate-limit #:mutable] ; limit on immediate allocation
                   [sync-futures? #:mutable]   ; whether a sync with future threads is needed on shutdown
                   [post-shutdown #:mutable])  ; callbacks to run in atomic mode after shutdown
  #:authentic)

(define (create-custodian parent)
  (custodian (host:unsafe-make-weak-hasheq)
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

;; Might be called with the custodian lock already held or in atomic mode, so that
;; a shut-down check can guard work that sholdn't be done if
;; it can't be registered with the custodian. (A custodian can only
;; be shut down with the lock and in atomic mode.)
(define (custodian-shut-down? c)
  (lock-custodians)
  (define shut-down? (unbox* (custodian-shut-down?-box c)))
  (unlock-custodians)
  shut-down?)

;; Called with the custodian lock
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
