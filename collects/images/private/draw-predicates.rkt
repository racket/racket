#lang racket/base

(require racket/draw racket/class)

(provide bitmap? dc?)

(define (bitmap? bm)
  (bm . is-a? . bitmap%))

(define (dc? dc)
  (dc . is-a? . dc<%>))
