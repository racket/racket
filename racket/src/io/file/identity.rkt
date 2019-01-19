#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt"
         "host.rkt"
         "error.rkt")

(provide path-or-fd-identity)

;; In atomic mode; returns out of atomic mode
(define (path-or-fd-identity who
                             #:host-path [host-path #f]
                             #:as-link? [as-link? #f] ; used only if `host-path`
                             #:fd [fd #f]
                             #:port [port #f] ; for errors, and non-#f if `fd` provided
                             #:no-error? [no-error? #f])
  (define r0 (if host-path
                 (rktio_path_identity rktio host-path (not as-link?))
                 (rktio_fd_identity rktio fd)))
  (define r (if (rktio-error? r0)
                r0
                (begin0
                  (rktio_identity_to_vector r0)
                  (rktio_free r0))))
  (end-atomic)
  (cond
    [(rktio-error? r0)
     (and (not no-error?)
          (raise-filesystem-error who
                                  r
                                  (if host-path
                                      (format (string-append
                                               "error obtaining identity for path\n"
                                               "  path: ~a")
                                              (host-> host-path))
                                      (format (string-append
                                               "error obtaining identity for port\n"
                                               "  port: ~v")
                                              port))))]
    [else
     (+ (vector-ref r 0)
        (arithmetic-shift (vector-ref r 1)
                          (vector-ref r 3))
        (arithmetic-shift (vector-ref r 2)
                          (+ (vector-ref r 3) (vector-ref r 4))))]))
