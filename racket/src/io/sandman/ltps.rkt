#lang racket/base
(require "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/place-local.rkt")

(provide shared-ltps
         shared-ltps-place-init!)

(define (make-ltps)
  (define ltps (rktio_ltps_open rktio))
  (unless (rktio-error? ltps)
    (unsafe-custodian-register (current-custodian)
                               ltps
                               ;; in atomic mode
                               (lambda (ltps)
                                 (rktio_ltps_close rktio ltps))
                               #f
                               #f))
  (if (rktio-error? ltps)
      rktio_NULL
      ltps))

(define-place-local shared-ltps (make-ltps))

(define (shared-ltps-place-init!)
  (make-ltps))
