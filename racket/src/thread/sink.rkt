#lang racket/base

(provide reference-sink)

(define (reference-sink v)
  (ephemeron-value (make-ephemeron #f (void)) (void) v))
