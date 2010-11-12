#lang racket/base

(provide current-run-status-box set-run-status
         current-messenger message)

;; current-run-status-box is used to let the client know where we are in the
;; submission process.
(define current-run-status-box (make-parameter #f))

;; current-messenger is a function that will send a message to the client.
(define current-messenger (make-parameter #f))
(define (message . args)
  (let ([messenger (current-messenger)])
    (and messenger (apply messenger args))))

;; Set the current-run-status-box and send a message.
(define (set-run-status s)
  (let ([b (current-run-status-box)])
    (when b (set-box! b s) (message s))))
