#lang racket/base
(require "host.rkt")

(provide current-atomic
         current-thread/in-racket
         current-future ; not the one exported to Racket; see "api.rkt"
         in-racket-thread?
         in-future-thread?)

;; These definitions are specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register.

;; Mre precisely, "current uninterruptible level":
(define current-atomic (make-pthread-parameter 0))

;; The `current-thread/in-racket` register is `#f` in
;; a future pthread, including one for a Racket parallel thread.
;; The `current-thread` wrapper blocks on access to this
;; pthread-local value in a future pthread:
(define current-thread/in-racket (make-pthread-parameter #f))

;; Normally #f for a place's main pthread (running a Racket thread)
;; and non-#f for a future pthread, but can be a would-be future
;; in the main pthread, and can be a future for an automatically generated
;; unblock thread:
(define current-future (make-pthread-parameter #f))

;; Calling `(current-thread/in-racket)` is faster than
;; `(current-thread)`, but it's only valid in a place's main pthread
;; --- not in a future pthread. Sometimes, we call it in a future pthread
;; to check for `#f` to me that we're in a future pthread.

(define (in-racket-thread?)
  (and (current-thread/in-racket) #t))
(define (in-future-thread?)
  (not (current-thread/in-racket)))
