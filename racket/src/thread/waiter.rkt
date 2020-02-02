#lang racket/base
(require "internal-error.rkt")

(provide prop:waiter
         make-waiter-methods
         waiter-resume!
         waiter-suspend!
         
         select-waiter)

;; A waiter can be in the queue for a semaphore or
;; channel
(define-values (prop:waiter waiter? waiter-ref)
  (make-struct-type-property 'waiter))

(struct waiter-methods (suspend resume)
  #:authentic)

(define (make-waiter-methods #:suspend! suspend
                             #:resume! resume)
  (waiter-methods suspend resume))
                             

(define (waiter-resume! w s)
  ((waiter-methods-resume (waiter-ref w)) w s))

;; `interrupt-cb` is run in atomic mode if the suspend is interrupted
;; by either a break or kill; the result is a `retry-cb`, which is run
;; out of atomic mode is the suspend-triggering action should be
;; retried
(define (waiter-suspend! w interrupt-cb)
  ((waiter-methods-suspend (waiter-ref w)) w interrupt-cb))

;; Used for semaphores and channels to run a "just selected" callback
;; when synchronized:
(struct select-waiter (proc)
  #:property prop:waiter
  (make-waiter-methods #:suspend! (lambda args (internal-error "should not suspend a select-waiter"))
                       #:resume! (lambda (w s)
                                   ((select-waiter-proc w)))))
