#lang racket/base
(require racket/fixnum
         "atomic.rkt"
         "thread.rkt"
         "schedule.rkt"
         "evt.rkt")

(provide unsafe-start-atomic
         unsafe-end-atomic
         unsafe-start-breakable-atomic
         unsafe-end-breakable-atomic
         unsafe-in-atomic?
         unsafe-set-on-atomic-timeout!)

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
