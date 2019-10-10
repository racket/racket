#lang racket/base

;; Probably locale-sensitive string comparisons are not justed just by
;; starting up, so the first `string-locale<?` will trigger the
;; creation of a byte converter on most Unix platforms. Make sure
;; that a cached converted is not connected to custodian.

(define c (make-custodian))
(parameterize ([current-custodian c])
  (void (string-locale<? "a" "b")))
(custodian-shutdown-all c)
(void (string-locale<? "a" "b"))
