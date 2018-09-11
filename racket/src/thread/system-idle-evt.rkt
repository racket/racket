#lang racket/base
(require "place-local.rkt"
         "evt.rkt"
         "semaphore.rkt")

(provide (rename-out [get-system-idle-evt system-idle-evt])

         any-idle-waiters?
         post-idle
         init-system-idle-evt!)

(define-place-local idle-sema (make-semaphore))
(define-place-local wrapped-idle-sema (wrap-evt idle-sema void))
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

(define (init-system-idle-evt!)
  (set! idle-sema (make-semaphore))
  (set! wrapped-idle-sema (wrap-evt idle-sema void)))
