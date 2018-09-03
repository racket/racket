#lang racket/base
(require "check.rkt"
         "host.rkt"
         "atomic.rkt")

(provide unsafe-os-thread-enabled?
         unsafe-call-in-os-thread
         unsafe-make-os-semaphore
         unsafe-os-semaphore-post
         unsafe-os-semaphore-wait)

(define (unsafe-os-thread-enabled?)
  (threaded?))

(define/who (unsafe-call-in-os-thread proc)
  (check who (procedure-arity-includes/c 0) proc)
  (unless threaded? (raise-unsupported who))
  (fork-pthread (lambda ()
                  (start-atomic) ; just in case
                  (proc)))
  (void))

(struct os-semaphore ([count #:mutable] mutex condition))

(define/who (unsafe-make-os-semaphore)
  (unless threaded? (raise-unsupported who))
  (os-semaphore 0 (host:make-mutex) (host:make-condition)))

(define/who (unsafe-os-semaphore-post s)
  (check who os-semaphore? s)
  (host:mutex-acquire (os-semaphore-mutex s))
  (when (zero? (os-semaphore-count s))
    (host:condition-signal (os-semaphore-condition s)))
  (set-os-semaphore-count! s (add1 (os-semaphore-count s)))
  (host:mutex-release (os-semaphore-mutex s)))

;; interrupts must be enabled when waiting on a semaphore; otherwise,
;; the wait will block GCs, likely deadlocking this thread and another
;; thread that is working toward posting the semaphore
(define/who (unsafe-os-semaphore-wait s)
  (check who os-semaphore? s)
  (host:mutex-acquire (os-semaphore-mutex s))
  (let loop ()
    (cond
      [(zero? (os-semaphore-count s))
       (host:condition-wait (os-semaphore-condition s) (os-semaphore-mutex s))
       (loop)]
      [else
       (set-os-semaphore-count! s (sub1 (os-semaphore-count s)))]))
  (host:mutex-release (os-semaphore-mutex s)))

(define (raise-unsupported who)
  (raise
   (exn:fail:unsupported
    (string-append (symbol->string who) ": unsupported on this platform")
    (current-continuation-marks))))
