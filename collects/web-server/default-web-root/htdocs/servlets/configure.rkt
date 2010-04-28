; DO NOT DELETE THIS SERVLET,
; unless you never want to reconfigure the Web server again.
; The servlet accepts requests only from the *same machine* as the Web server
; for security purposes.
#lang racket/base
(require web-server/private/configure)
(provide (all-from-out web-server/private/configure))
