#lang racket/base
(require (only-in racket/unsafe/ops
                  unsafe-struct*-cas!)
         "../host/thread.rkt"
         "../host/pthread.rkt"
         "../common/internal-error.rkt"
         "port.rkt")

(provide port-lock
         port-unlock
         with-lock
         with-no-lock
         merely-atomically
         also-atomically
         port-lock-require-atomic!
         port-lock-just-became-atomic?
         port-lock-init-atomic-mode)

;; A port lock implies uninterruptable mode, but not necessarily
;; atomic mode --- and an upgrade to atomic mode is not allowed. Also,
;; beware that a port lock is not reentrant. Since port locks have no
;; order, only one port lock can be taken at a time. It's fine to take
;; a port lock in atomic mode, though.
;;
;; A port can be in a state where a port lock implies atomic mode.
;; That state is needed for progress evts in an input port, since
;; progress synchronization involves the thread scheduler.
;;
;; Possble `(core-port-lock p)` values for a port `p`:
;;  - #f: lock is available, atomic mode not required [fast path]
;;  - #t: lock is taken, atomic mode not required, uncontended [fast path]
;;  - 'atomic: lock is available, but must be take in atomic mode
;;  - 'to-atomic: lock is taken, atomic mode required in future, uncontended
;;  - 'in-atomic: lock is taken, atomic mode was and remains required, uncontended
;;  - `lock` record: general case for contended, uses host-supplied synchronization
;; A port lock can go from atomic mode to non-atomic mode while being held (as
;; decided by the thread holding the lock), but it can go from contended to
;; uncontended without the lock holder's decision, obviously.
;; When a lock goes from uncontended to contended, it stays contentded.
;; So, these are the possible transitions:
;;
;;   *uncontended, non-atomic*               *uncontended, atomic*
;;         {#f, #t}             <---->   {'atomic, 'in-atomic, 'to-atomic}
;;
;;            |                                  |
;;            v                                  V
;;
;;     (lock #f ....)           <---->       (lock #t ....)
;;    *contended, non-atomic*              *contended,atomic*
;;
;; Port locks are unordered. If you need to take multiple of then (as
;; `subprocess` does), then set them to atomic mode, and take them only
;; after entering atomic mode (at which point the order won't matter).
;; Port locks are order *before* the rktio lock.

(struct lock ([atomic? #:mutable]
              [was-atomic? #:mutable]
              [locked? #:mutable]
              mutex
              condition)
  #:authentic)

(define-syntax-rule (core-port-lock-cas! p old-v new-v)
  (unsafe-struct*-cas! p 2 old-v new-v))

(define-syntax-rule (port-lock p-expr)
  (let ([p p-expr])
    (start-uninterruptible)
    (unless (core-port-lock-cas! p #f #t)
      (port-lock-slow p))
    (memory-order-acquire)))

(define-syntax-rule (port-unlock p-expr)
  (let ([p p-expr])
    (memory-order-release)
    (unless (core-port-lock-cas! p #t #f)
      (port-unlock-slow p))
    ;; The intent of ending uninterruptible mode with
    ;; `end-atomic` is to include a future barrier exit, in
    ;; case `also-aotmically` was used
    (end-atomic)))

(define-syntax-rule (with-lock p-expr e ...)
  (let ([p p-expr])
    (port-lock p)
    (begin0
      (let () e ...)
      (port-unlock p))))

(define-syntax-rule (with-no-lock p-expr e ...)
  (let ([p p-expr])
    (port-unlock p)
    (begin0
      (let () e ...)
      (port-lock p))))

;; Releases the lock during `e ...`, but keeps atomic mode
;; (i.e., the lock is expected to be held in atomic mode, not
;; merely uninterruptible mode)
(define-syntax-rule (merely-atomically p-expr e ...)
  (let ([p p-expr])
    (port-unlock-slow p)
    (begin0
      (let () e ...)
      (port-lock-slow p))))

;; Releases the lock at the beginning, but retains it after starting `e ...`
(define-syntax-rule (also-atomically p-expr e ...)
  (let ([p p-expr])
    (port-unlock p) ; can't escalate to atomic with port lock held
    (start-atomic)
    (port-lock p) ; implies uninterrupted
    (begin0
      (let () e ...)
      ;; this is really a "demote from atomic to uninterrupted"
      (end-atomic))))

;; in uninterrutable mode, might be in atomic mode on exit
(define (port-lock-slow p)
  (define lock (core-port-lock p))
  (cond
    [(not lock)
     ;; try fast path again
     (unless (core-port-lock-cas! p #f #t)
       (port-lock-slow p))]
    [(eq? lock 'atomic)
     (end-uninterruptible)
     (start-atomic)
     (unless (core-port-lock-cas! p 'atomic 'in-atomic)
       (end-atomic)
       (start-uninterruptible)
       (port-lock-slow p))]
    [(or (eq? lock #t)
         (eq? lock 'in-atomic)
         (eq? lock 'to-atomic))
     ;; lock is taken, try switching to general mode
     (define new-lock (make-lock (eq? lock 'in-atomic)))
     (core-port-lock-cas! p #t new-lock) ; ok for CAS to fail
     (port-lock-slow p)]
    [(lock-atomic? lock)
     (end-uninterruptible)
     (start-atomic)
     (lock-acquire lock)
     (set-lock-was-atomic?! lock #t)]
    [(lock? lock)
     (lock-acquire lock)
     (cond
       [(lock-atomic? lock)
        ;; switched to requiring atomic while we waited
        (lock-release lock)
        (port-lock-slow p)]
       [else
        (set-lock-was-atomic?! lock #f)])]
    [else
     (internal-error "tried to take port lock reentrantly")]))

;; in uninterrutable mode, possibly atomic; returns as uninterrutable
(define (port-unlock-slow p)
  (define lock (core-port-lock p))
  (cond
    [(eq? lock #t)
     ;; try fast path again
     (unless (core-port-lock-cas! p #t #f)
       (port-unlock-slow p))]
    [(eq? lock 'to-atomic)
     (unless (core-port-lock-cas! p 'to-atomic 'atomic)
       (port-unlock-slow p))]
    [(eq? lock 'in-atomic)
     (cond
       [(core-port-lock-cas! p 'in-atomic 'atomic)
        (end-atomic)
        (start-uninterruptible)]
       [else
        (port-unlock-slow p)])]
    [(lock-was-atomic? lock)
     (lock-release lock)
     (end-atomic)
     (start-uninterruptible)]
    [(lock? lock)
     (lock-release lock)]
    [else
     (internal-error "tried to release port lock not held")]))

;; change lock requirement for future takers
(define (port-lock-require-atomic! p atomic?)
  (define lock (core-port-lock p))
  (cond
    [(eq? lock #t)
     (when atomic?
       (let loop ()
         (or (core-port-lock-cas! p #t 'to-atomic)
             (loop))))]
    [(eq? lock 'to-atomic)
     (unless atomic?
       (let loop ()
         (or (core-port-lock-cas! p 'to-atomic #t)
             (loop))))]
    [(eq? lock 'in-atomic)
     (unless atomic?
       (let loop ()
         (unless (core-port-lock-cas! p 'in-atomic #t)
           (loop))))]
    [(lock? lock)
     (set-lock-atomic?! lock atomic?)]
    [else
     (internal-error "tried to set port lock atomicity without holding it")]))

(define (port-lock-just-became-atomic? p)
  (eq? (core-port-lock p) 'to-atomic))

(define (port-lock-init-atomic-mode p)
  (set-core-port-lock! p 'atomic)
  p)

(define (make-lock init-atomic?)
  (lock init-atomic? init-atomic? #t (make-mutex) (make-condition)))

(define (lock-acquire lock)
  (mutex-acquire (lock-mutex lock))
  (let loop ()
    (cond
      [(lock-locked? lock)
       (condition-wait (lock-condition lock) (lock-mutex lock))
       (loop)]
      [else
       (set-lock-locked?! lock #t)]))
  (mutex-release (lock-mutex lock)))

(define (lock-release lock)
  (mutex-acquire (lock-mutex lock))
  (set-lock-locked?! lock #f)
  (condition-signal (lock-condition lock))
  (mutex-release (lock-mutex lock)))
