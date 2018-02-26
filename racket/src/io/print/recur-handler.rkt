#lang racket/base
(require "../port/output-port.rkt"
         "mode.rkt")

(provide set-port-handlers-to-recur!)

(define (set-port-handlers-to-recur! port handle)
  (set-core-output-port-print-handler! port
                                       (lambda (e p [mode 0])
                                         (handle e p mode)))
  (set-core-output-port-write-handler! port
                                       (lambda (e p)
                                         (handle e p WRITE-MODE)))
  (set-core-output-port-display-handler! port
                                         (lambda (e p)
                                           (handle e p DISPLAY-MODE))))
