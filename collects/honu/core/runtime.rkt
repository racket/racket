#lang racket/base

(require "read.rkt")

(provide configure)
(define (configure . args)
  (current-read-interaction honu-read-syntax))
