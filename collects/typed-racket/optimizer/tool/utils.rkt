#lang racket/base

(provide (all-defined-out))

(define (pos-inside-us? pos our-pos our-span)
  (and pos our-pos our-span (<= our-pos pos (+ our-pos our-span))))
