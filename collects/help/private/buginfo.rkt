#lang racket/base

(provide set-bug-report-info!
         get-bug-report-infos
         bri-label
         bri-value)

(struct bri (label get-value) #:transparent)
(define (bri-value bri) ((bri-get-value bri)))

(define bug-report-infos null)

(define (set-bug-report-info! str thunk)
  (set! bug-report-infos (cons (bri str thunk) bug-report-infos)))

(define (get-bug-report-infos) bug-report-infos)
