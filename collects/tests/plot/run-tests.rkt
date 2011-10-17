#!/bin/sh
#| -*- racket -*-
exec gracket "$0" "$@"
|#
#lang racket

(require rackunit racket/date
         plot plot/utils plot/common/date-time)

(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #t) '(0 1))
(check-equal? (linear-seq 0 1 2 #:start? #t #:end? #f) '(0 2/3))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #t) '(1/3 1))
(check-equal? (linear-seq 0 1 2 #:start? #f #:end? #f) '(1/4 3/4))

(check-exn exn:fail:contract?
           (λ () (vector-field (λ (v [z 0]) v) -4 4 -4 4))
           "Exception should be 'two of the clauses in the or/c might both match' or similar")

;; ===================================================================================================
;; Date rounding

(check-equal? (utc-seconds-round-year (find-seconds 0 0 12 2 7 1970 #f))
              (find-seconds 0 0 0 1 1 1970 #f))
(check-equal? (utc-seconds-round-year (find-seconds 0 0 13 2 7 1970 #f))
              (find-seconds 0 0 0 1 1 1971 #f))
;; A leap year's middle is a half day earlier on the calendar:
(check-equal? (utc-seconds-round-year (find-seconds 0 0 0 2 7 1976 #f))
              (find-seconds 0 0 0 1 1 1976 #f))
(check-equal? (utc-seconds-round-year (find-seconds 0 0 1 2 7 1976 #f))
              (find-seconds 0 0 0 1 1 1977 #f))

(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 16 1 2010 #f))
              (find-seconds 0 0 0 1 1 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 17 1 2010 #f))
              (find-seconds 0 0 0 1 2 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 12 16 1 2010 #f))
              (find-seconds 0 0 0 1 1 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 13 16 1 2010 #f))
              (find-seconds 0 0 0 1 2 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 16 12 2010 #f))
              (find-seconds 0 0 0 1 12 2010 #f))
(check-equal? (utc-seconds-round-month (find-seconds 0 0 0 17 12 2010 #f))
              (find-seconds 0 0 0 1 1 2011 #f))

;; ===================================================================================================
;; Time conversion

(check-equal? (seconds->plot-time 0)                (plot-time 0 0 0 0))
(check-equal? (seconds->plot-time #e59.999999)      (plot-time #e59.999999 0 0 0))
(check-equal? (seconds->plot-time 60)               (plot-time 0 1 0 0))
(check-equal? (seconds->plot-time #e60.000001)      (plot-time #e0.000001 1 0 0))
(check-equal? (seconds->plot-time #e119.999999)     (plot-time #e59.999999 1 0 0))
(check-equal? (seconds->plot-time 120)              (plot-time 0 2 0 0))
(check-equal? (seconds->plot-time #e120.000001)     (plot-time #e0.000001 2 0 0))
(check-equal? (seconds->plot-time 3599)             (plot-time 59 59 0 0))
(check-equal? (seconds->plot-time 3600)             (plot-time 0 0 1 0))
(check-equal? (seconds->plot-time 3601)             (plot-time 1 0 1 0))
(check-equal? (seconds->plot-time (- seconds-per-day 1))         (plot-time 59 59 23 0))
(check-equal? (seconds->plot-time seconds-per-day)               (plot-time 0 0 0 1))
(check-equal? (seconds->plot-time (- seconds-per-day))           (plot-time 0 0 0 -1))
(check-equal? (seconds->plot-time (- (- seconds-per-day) 1))     (plot-time 59 59 23 -2))

(define sec-secs (sequence->list (in-range -60 61 #e0.571123)))
(define min-secs (sequence->list (in-range (- seconds-per-hour) (+ seconds-per-hour 1)
                                           (* #e0.571123 seconds-per-minute))))
(define hour-secs (sequence->list (in-range (- seconds-per-day) (+ seconds-per-day 1)
                                            (* #e0.571123 seconds-per-hour))))
(define day-secs (sequence->list (in-range (- seconds-per-week) (+ seconds-per-week 1)
                                           (* #e0.571123 seconds-per-day))))
(check-equal? (map (compose plot-time->seconds seconds->plot-time) sec-secs) sec-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) min-secs) min-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) hour-secs) hour-secs)
(check-equal? (map (compose plot-time->seconds seconds->plot-time) day-secs) day-secs)

