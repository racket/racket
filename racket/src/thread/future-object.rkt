#lang racket/base
(require "host.rkt")

(provide (struct-out future*)

         currently-running-future-key
         currently-running-future)

;; ----------------------------------------

;; See "future-lock.rkt" for information on locking rules
(struct future* (id
                 lock
                 custodian          ; don't run in future pthread if custodian is shut down
                 [would-be? #:mutable] ; transitions from #t to 'blocked after blocked
                 [thunk #:mutable]  ; thunk or continuation
                 [prev #:mutable]   ; queue previous
                 [next #:mutable]   ; queue next
                 [results #:mutable]
                 [state #:mutable]  ; #f (could run), 'running, 'blocked, 'done, 'aborted, 'fsema or box, or future waiting on
                 [dependents #:mutable]) ; futures that are blocked on this one
  #:authentic
  #:reflection-name 'future)

;; ----------------------------------------

(define currently-running-future-key (gensym 'future))

;; Only called in a Racket thread:
(define (currently-running-future)
  (continuation-mark-set-first
   #f
   currently-running-future-key
   #f
   (unsafe-root-continuation-prompt-tag)))
