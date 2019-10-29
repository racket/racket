#lang racket/base
(require "host.rkt")

(provide current-atomic
         current-thread/in-atomic
         current-future) ; not the one exported to Racket; see "api.rkt"

;; These definitions are specially recognized for Racket on
;; Chez Scheme and converted to use a virtual register.

(define current-atomic (make-pthread-parameter 0))

;; The `current-thread` wrapper disallows access to this
;; pthread-local value in a future pthread:
(define current-thread/in-atomic (make-pthread-parameter #f))

;; Normally #f for a place's main pthread (running a Racket thread)
;; and non-#f for a future pthread, but can be a would-be future
;; in the main pthread
(define current-future (make-pthread-parameter #f))

;; Calling `(current-thread/in-atomic)` is faster than
;; `(current-thread)`, but it's only valid in a place's main pthread
;; --- not in a future thread.
