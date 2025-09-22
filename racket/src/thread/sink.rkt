#lang racket/base

(provide reference-sink)

(define (reference-sink v)
  (black-box v)
  (void))
