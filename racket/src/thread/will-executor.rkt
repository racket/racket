#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "host.rkt"
         "evt.rkt"
         "sync.rkt"
         "semaphore.rkt")

;; The core must provide `will-try-execute`. We implement
;; `will-execute` here, because it has to block when no will is ready.

(provide make-will-executor
         make-stubborn-will-executor
         will-executor?
         will-register
         will-try-execute
         will-execute)

(struct will-executor (host-we sema)
  #:authentic
  #:property prop:evt (lambda (we)
                        (wrap-evt (semaphore-peek-evt (will-executor-sema we))
                                  (lambda (v) we))))

(define (do-make-will-executor host:make-will-executor)
  (define sema (make-semaphore))
  (will-executor (host:make-will-executor
                  ;; called in scheduler:
                  (lambda () (semaphore-post/atomic sema)))
                 sema))

(define (make-will-executor)
  (do-make-will-executor host:make-will-executor))

(define (make-stubborn-will-executor)
  (do-make-will-executor host:make-stubborn-will-executor))

(define/who (will-register we v proc)
  (check who will-executor? we)
  (check who (procedure-arity-includes/c 1) proc)
  (host:will-register (will-executor-host-we we) v proc))

(define (do-will-execute who we fail-k)
  (check who will-executor? we)
  ((atomically
    (cond
      [(host:will-try-execute (will-executor-host-we we))
       => (lambda (p)
            (semaphore-wait/atomic (will-executor-sema we))
            (lambda ()
              ((car p) (cdr p))))]
      [else fail-k]))))

(define/who (will-execute we)
  (do-will-execute who we (lambda ()
                            (sync we)
                            (will-execute we))))

(define/who (will-try-execute we [default #f])
  (do-will-execute who we (lambda () default)))
