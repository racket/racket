#lang racket/base
(require "evt.rkt"
         "semaphore.rkt"
         "internal-error.rkt")

(provide (rename-out [get-system-idle-evt system-idle-evt])

         any-idle-waiters?
         post-idle)

(define idle-sema (make-semaphore))
(define wrapped-idle-sema (wrap-evt idle-sema void))
(struct system-idle-evt ()
  #:property prop:evt (lambda (i) wrapped-idle-sema))

(define the-idle-evt (system-idle-evt))

(define get-system-idle-evt
  (let ([system-idle-evt
         (lambda () the-idle-evt)])
    system-idle-evt))

;; Called by the scheduler in atomic mode:
(define (any-idle-waiters?)
  (semaphore-any-waiters? idle-sema))

;; Called by the scheduler in atomic mode:
(define (post-idle)
  (and (semaphore-any-waiters? idle-sema)
       (begin
         (semaphore-post/atomic idle-sema)
         #t)))
