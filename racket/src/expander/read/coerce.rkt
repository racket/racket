#lang racket/base
(require "config.rkt")

(provide coerce)

(define (coerce val in config)
  (define for-syntax? (read-config-for-syntax? config))
  ((read-config-coerce config)
   for-syntax?
   val
   (and for-syntax? (port+config->srcloc in config))))
