#lang racket/base
(require "../host/rktio.rkt"
         "../host/thread.rkt"
         "../locale/string.rkt")

(provide get-machine-info)

(define (get-machine-info)
  (bytes->string/locale
   (atomically
    (define v (rktio_uname rktio))
    (begin0
      (rktio_to_bytes v)
      (rktio_free v)))))
