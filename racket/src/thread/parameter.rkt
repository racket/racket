#lang racket/base
(require "engine.rkt")

(provide current-thread)

(define current-thread (make-pthread-parameter #f))
