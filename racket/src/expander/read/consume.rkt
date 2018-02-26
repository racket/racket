#lang racket/base
(require "special.rkt"
         "config.rkt")

(provide consume-char
         consume-char/special)

;; Consume a previously peek character. We could
;; double-check that the read character matches `c`
(define (consume-char in c)
  (read-char in)
  (void))

(define (consume-char/special in config c)
  (read-char-or-special in special (read-config-source config))
  (void))
