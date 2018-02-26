#lang racket/base
(require "../common/check.rkt")

(provide check-bstr)

(define (check-bstr who bstr start end)
  (check who bytes? bstr)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (define len (bytes-length bstr))
  (unless (<= 0 start len)
    (raise-range-error who "byte string" "starting " start bstr 0 len #f))
  (unless (<= start end len)
    (raise-range-error who "byte string" "ending " end bstr 0 len start)))
