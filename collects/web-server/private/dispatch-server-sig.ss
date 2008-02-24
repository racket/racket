#lang scheme/base
(require mzlib/unit)

(define-signature dispatch-server^
  (serve
   serve-ports))

(define-signature dispatch-server-config^
  (port listen-ip max-waiting initial-connection-timeout
        read-request dispatch))

(provide
 dispatch-server^ dispatch-server-config^)
