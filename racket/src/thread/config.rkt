#lang racket/base

(provide (all-defined-out))

(define TICKS 100000)

(define (set-schedule-quantum! n)
  (set! TICKS n))
