#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../file/identity.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "file-stream.rkt"
         "check.rkt")

(provide port-file-identity)

(define/who (port-file-identity p)
  (check who file-stream-port? p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (start-atomic)
  (check-not-closed who cp)
  (define fd ((file-stream-ref cp) cp))
  (path-or-fd-identity who #:fd fd #:port p))
