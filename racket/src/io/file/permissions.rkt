#lang racket/base

(provide permissions?
         permissions-desc)

(define (permissions? perms)
  (and (exact-integer? perms) (<= 0 perms 65535)))
(define permissions-desc "(integer-in 0 65535)")

