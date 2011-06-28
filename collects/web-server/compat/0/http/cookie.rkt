#lang racket/base
(require web-server/http/xexpr)

(define (xexpr-response/cookies cs xe)
  (response/xexpr xe #:cookies cs))

(provide (all-from-out))
