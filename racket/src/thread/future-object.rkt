#lang racket/base
(require "host.rkt"
         "parameter.rkt")

(provide (struct-out future*)
         (struct-out parallel-thread-pool)

         (struct-out parallel*)
         future-stop?

         currently-running-future-key
         currently-running-future)

;; ----------------------------------------

;; See "future-lock.rkt" for information on locking rules
(struct future* (id
                 lock
                 custodian          ; don't run in future pthread if custodian is shut down
                 [parallel #:mutable] ; #f for a normal future, a `parallel` record for a future implementing a parallel thread
                 [kind #:mutable]   ; #f, 'would-be, or 'was
                 [thunk #:mutable]  ; thunk or continuation
                 [prev #:mutable]   ; queue previous
                 [next #:mutable]   ; queue next
                 [results #:mutable] ; may have (cons <mutex> <condition>) to go with a stop request
                 [state #:mutable]  ; #f (could run), 'running, 'blocked, 'done, 'aborted, 'fsema or box, or future waiting on
                 [dependents #:mutable] ; futures that are blocked on this one
                 [suspend-pthread-id #:mutable] ; for delayed logging
                 [suspend-timestamp #:mutable]) ; got delayed logging
  #:authentic
  #:reflection-name 'future)

(struct parallel-thread-pool (scheduler
                              [capacity #:mutable]
                              [swimmers #:mutable]
                              [custodian-reference #:mutable])
  #:authentic
  #:reflection-name 'parallel-thread-pool)

(struct parallel* (pool               ; futures scheduler that manages the future, #f implies `(current-scheduler)`
                   [thread #:mutable] ; a Racket thread or or 'stop termination request
                   [stop? #:mutable]))

(define (future-stop? f)
  (define p (future*-parallel f))
  (and p (parallel*-stop? p)))

;; ----------------------------------------

(define currently-running-future-key (gensym 'future))

(define (currently-running-future)
  (define f (current-future))
  (cond
    [f (and (not (future*-parallel f))
            f)]
    [else
     (continuation-mark-set-first
      #f
      currently-running-future-key
      #f
      (unsafe-root-continuation-prompt-tag))]))
