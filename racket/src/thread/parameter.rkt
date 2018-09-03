#lang racket/base
(require "host.rkt")

(provide current-thread)

(define current-thread (make-pthread-parameter #f))
