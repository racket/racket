#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../file/stat.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "lock.rkt"
         "file-stream.rkt"
         "check.rkt")

(provide port-file-stat)

(define/who (port-file-stat p)
  (check who file-stream-port? p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (port-lock cp)
  (check-not-closed who cp)
  (define fd ((file-stream-ref cp) cp))
  (path-or-fd-stat who #:fd fd #:port p #:unlock (lambda () (port-unlock cp))))
