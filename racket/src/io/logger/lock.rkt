#lang racket/base
(require "../host/thread.rkt"
         "../host/place-local.rkt")

(provide uninterruptibly/with-logger-lock/no-gc-interrupts/no-wind
         atomically/with-logger-lock/no-gc-interrupts/no-wind
         logger-init-lock!)

;; Using a logger requires the logger lock (implies GC interrupts are
;; disabled as well as uninterruptible mode for Racket schedulers) or
;; being in a GC interrupt via and host Scheme thread with all other
;; Scheme threads paused; the latter effectively holds the lock.

;; A log receiver is protected by atomic mode (not mere uninterrupted
;; mode), and not by the logger lock.

;; Take this lock only with GC disabled:
(define-place-local logger-lock (unsafe-make-uninterruptible-lock))

;; Disables host interrupts, but the "no wind" part is
;; an unforced constraint: don't use anything related
;; to `dynamic-wind`, continuations, or continuation marks.
;; Cannot be exited with `non-atomically`.
(define-syntax-rule (atomically/with-logger-lock/no-gc-interrupts/no-wind e ...)
  (begin
    (start-atomic/no-gc-interrupts)
    (assert-push-lock-level! 'logger)
    (unsafe-uninterruptible-lock-acquire logger-lock)
    (begin0
      (let () e ...)
      (unsafe-uninterruptible-lock-release logger-lock)
      (assert-pop-lock-level! 'logger)
      (end-atomic/no-gc-interrupts))))


(define-syntax-rule (uninterruptibly/with-logger-lock/no-gc-interrupts/no-wind e ...)
  (begin
    (start-uninterruptible/no-gc-interrupts)
    (assert-push-lock-level! 'logger)
    (unsafe-uninterruptible-lock-acquire logger-lock)
    (begin0
      (let () e ...)
      (unsafe-uninterruptible-lock-release logger-lock)
      (assert-pop-lock-level! 'logger)
      (end-uninterruptible/no-gc-interrupts))))

(define (logger-init-lock!)
  (set! logger-lock (unsafe-make-uninterruptible-lock)))
