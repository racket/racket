#lang racket/base
(require racket/fixnum
         "internal-error.rkt"
         "host.rkt"
         "parameter.rkt"
         "atomic.rkt")

;; This module implements a lightweight spinlock for futures and
;; fsemaphores.

;; The overall locking regime depends on this lock order (i.e.,
;; when multiple locks are held at once, they must be acquired
;; in this order):
;;
;;    - engine atomicity
;;    - fsemaphore [one at a time]
;;    - futures, lower ID before higher ID (implies engine atomicity)
;;    - schedule queue
;;    - place lock
;;
;; A future's lock must be held to change the future's fields, except
;; that the fields to implement the schedule queue should be modified
;; only with the schedule-queue lock held.
;;
;; A future with state #f is available to run, but it must be either
;; would-be (never in the queue, only run by a Racket thread) or
;; currently in a queue for a future pthread to take ownership of the
;; future.

(provide with-lock
         make-lock
         lock-acquire
         lock-release)

(define-syntax-rule (with-lock lock-expr expr ...)
  (let ([lock lock-expr])
    (lock-acquire lock)
    (begin0
      (let () expr ...)
      (lock-release lock))))

(define (make-lock) (box 0))

(define (lock-acquire lock)
  (start-uninterruptible)
  (let loop ()
    (if (box-cas! lock 0 1)
        (memory-order-acquire)
        (loop))))

(define (lock-release lock)
  (memory-order-release)
  (cond
    [(box-cas! lock 1 0)
     (end-uninterruptible)]
    [(eq? (unbox lock) 0)
     ;; not just a spurious failure...
     (internal-error "lock release failed!")]
    [else (lock-release lock)]))
