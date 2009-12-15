#lang scheme/base

(require r6rs/private/ports)
(provide (except-out (all-from-out r6rs/private/ports)
                     r6rs-port->port))
