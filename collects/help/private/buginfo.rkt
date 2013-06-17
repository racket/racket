#lang racket/base

(provide set-bug-report-info!
         get-bug-report-infos
         bri-label
         bri-value
         bri-min-height)

(struct bri (label get-value min-height) #:transparent)
(define (bri-value bri) ((bri-get-value bri)))

(define bug-report-infos null)

(define (set-bug-report-info! str thunk [min-height #f])
  (set! bug-report-infos (cons (bri str thunk min-height) bug-report-infos)))

(define (get-bug-report-infos) bug-report-infos)
