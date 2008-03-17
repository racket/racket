#lang scheme/base

(provide s:read s:write)

(define (s:write . args)
  (parameterize ([print-graph #t]) (apply write args)))
(define (s:read . args)
  (parameterize ([read-accept-graph #t]) (apply read args)))
