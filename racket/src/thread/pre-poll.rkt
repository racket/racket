#lang racket/base
(require "atomic.rkt"
         "host.rkt")

(provide unsafe-add-pre-poll-callback!
         call-pre-poll-external-callbacks)

(define pre-poll-callbacks null)

;; called in atomic mode in an arbitrary host thread, but
;; with all other host threads paused; the given procedure
;; will be called in atomic mode, possibly in the schduler
(define (unsafe-add-pre-poll-callback! proc)
  (set! pre-poll-callbacks (cons proc pre-poll-callbacks)))

;; in atomic mode
(define (call-pre-poll-external-callbacks)
  (unless (null? pre-poll-callbacks)
    ;; disable interrupts to avoid a case with `unsafe-add-pre-poll-callback!`
    (host:disable-interrupts)
    (define l pre-poll-callbacks)
    (set! pre-poll-callbacks null)
    (host:enable-interrupts)
    ;; Call the callbacks
    (for ([cb (in-list (reverse l))])
      (cb))))
