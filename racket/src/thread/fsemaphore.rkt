#lang racket/base
(require "check.rkt"
         "semaphore.rkt")

(provide fsemaphore?
         make-fsemaphore
         fsemaphore-post
         fsemaphore-wait
         fsemaphore-try-wait?
         fsemaphore-count)

(struct fsemaphore (sema))

(define/who (make-fsemaphore init)
  (check who exact-nonnegative-integer? init)
  (fsemaphore (make-semaphore init)))

(define/who (fsemaphore-post fsema)
  (check who fsemaphore? fsema)
  (semaphore-post (fsemaphore-sema fsema)))

(define/who (fsemaphore-wait fsema)
  (check who fsemaphore? fsema)
  (semaphore-wait (fsemaphore-sema fsema)))

(define/who (fsemaphore-try-wait? fsema)
  (check who fsemaphore? fsema)
  (semaphore-try-wait? (fsemaphore-sema fsema)))

(define/who (fsemaphore-count fsema)
  (check who fsemaphore? fsema)
  0)
