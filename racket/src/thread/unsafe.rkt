#lang racket/base
(require racket/fixnum
         "atomic.rkt"
         "thread.rkt"
         "schedule.rkt"
         "custodian-object.rkt"
         "evt.rkt"
         (only-in "host.rkt"
                  threaded?
                  host:make-mutex
                  host:mutex-acquire
                  host:mutex-release))

(provide unsafe-start-atomic
         unsafe-end-atomic
         unsafe-start-breakable-atomic
         unsafe-end-breakable-atomic
         unsafe-in-atomic?
         unsafe-set-on-atomic-timeout!

         unsafe-start-uninterruptible
         unsafe-end-uninterruptible

         unsafe-make-uninterruptible-lock
         unsafe-uninterruptible-lock-acquire
         unsafe-uninterruptible-lock-release

         unsafe-uninterruptible-custodian-lock-release
         unsafe-uninterruptible-custodian-lock-acquire)

(define (unsafe-start-breakable-atomic)
  (start-atomic)
  (current-breakable-atomic (fx+ (current-breakable-atomic) 1)))

(define (unsafe-end-breakable-atomic)
  (current-breakable-atomic (fx- (current-breakable-atomic) 1))
  (end-atomic))

(define (unsafe-start-atomic)
  (start-atomic))

(define (unsafe-end-atomic)
  (end-atomic))

(define (unsafe-in-atomic?)
  (positive? (current-atomic)))

(define (unsafe-set-on-atomic-timeout! proc)
  (set-atomic-timeout-callback! proc))

(define (unsafe-start-uninterruptible)
  (start-uninterruptible))
(define (unsafe-end-uninterruptible)
  (end-uninterruptible))
  
(define (unsafe-make-uninterruptible-lock)
  (if (threaded?)
      (host:make-mutex)
      'dummy-lock))
(define (unsafe-uninterruptible-lock-acquire m)
  (start-uninterruptible)
  (when (threaded?)
    (host:mutex-acquire m)))
(define (unsafe-uninterruptible-lock-release m)
  (when (threaded?)
    (host:mutex-release m))
  (end-uninterruptible))

(define (unsafe-uninterruptible-custodian-lock-acquire)
  (lock-custodians))
(define (unsafe-uninterruptible-custodian-lock-release)
  (unlock-custodians))
