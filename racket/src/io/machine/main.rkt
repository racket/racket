#lang racket/base
(require "../host/rktio.rkt"
         "../host/thread.rkt"
         "../string/convert.rkt")

(provide get-machine-info)

(define (get-machine-info)
  (bytes->string/utf-8
   (atomically
    (define v (rktio_uname rktio))
    (begin0
      (rktio_to_bytes v)
      (rktio_free v)))))
