#lang racket/base
(require ffi/unsafe/schedule)

;; make sure that a poll that never completes doesn't spin,
;; even when waiting on it from a parallel thread

(define ready 0)

(struct e ()
  #:property prop:evt (unsafe-poller
                       (lambda (self wakeups)
                         #;(log-error "~s ~s" ready (and wakeups))
                         (set! ready (add1 ready))
                         (values #f self))))
(define t1
 (thread (lambda ()
           (sync (e)))))
(define t2
 (thread #:pool 'own
         (lambda ()
           (sync (e)))))

(when (sync/timeout 1 t1 t2)
  (error "should not sync"))

(unless (ready . < . 2000)
  (error "too much polling" ready))

ready
