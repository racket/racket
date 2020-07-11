#lang racket/base
(require "internal-error.rkt"
         "host.rkt"
         "parameter.rkt"
         "atomic.rkt")

;; This module implements a lightweight spinlock for futures and
;; fsemaphores.

;; The overall locking regime depends on this lock order (i.e.,
;; when multiple locks are held at once, they must be acquired
;; in this order):
;;
;;    - fsemaphore [one at a time]
;;    - schedule queue
;;    - futures, lower ID before higher ID
;;
;; A future's lock must be held to change the future's fields, except
;; that the fields to implement the schedule queue should be modified
;; only with the schedule-queue lock held.
;;
;; A future with state #f is available to run, but it must be either
;; `would-be?` (never in the queue, only run by a Racket thread) or
;; currently in a queue for a future pthread to take ownership of the
;; future.

(provide with-lock
         make-lock
         lock-acquire
         lock-release
         start-future-uninterrupted
         end-future-uninterrupted)

(define-syntax-rule (with-lock lock-expr expr ...)
  (let ([lock lock-expr])
    (lock-acquire lock)
    (begin0
      (let () expr ...)
      (lock-release lock))))

(define (make-lock) (box 0))

(define (start-future-uninterrupted)
  (if (current-future)
      (current-atomic (add1 (current-atomic))) ; see `run-future-in-worker`
      (start-atomic)))

(define (end-future-uninterrupted)
  (if (current-future)
      (current-atomic (sub1 (current-atomic))) ; see `run-future-in-worker`
      (end-atomic)))

(define (lock-acquire lock)
  (start-future-uninterrupted)
  (let loop ()
    (if (box-cas! lock 0 1)
        (memory-order-acquire)
        (loop))))

(define (lock-release lock)
  (cond
    [(box-cas! lock 1 0)
     (memory-order-release)
     (end-future-uninterrupted)]
    [(eq? (unbox lock) 0)
     ;; not just a spurious failure...
     (internal-error "lock release failed!")]
    [else (lock-release lock)]))
