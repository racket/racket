#lang racket/base

(provide current-debug)

;; Insert debugging checks?
(define current-debug (make-parameter #f))

