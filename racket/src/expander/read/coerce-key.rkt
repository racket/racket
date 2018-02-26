#lang racket/base
(require "config.rkt")

(provide coerce-key)

(define (coerce-key key config)
  (define for-syntax? (read-config-for-syntax? config))
  ((read-config-coerce-key config)
   for-syntax?
   key))
