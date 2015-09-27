#lang racket/base

(require "private/unix-rand.rkt" "private/windows-rand.rkt" racket/contract/base)
(provide (contract-out [crypto-random-bytes (-> exact-nonnegative-integer? bytes?)]))

; (: crypto-random-bytes (-> Positive-Integer Bytes))
; returns n random bytes from the os.
(define (crypto-random-bytes n)
  (case (system-type 'os)
    [(unix macosx) (crypto-random-unix-bytes n)]
    [(windows) (crypto-random-windows-bytes n)]
    [else (raise (make-exn:fail:unsupported
                  "not supported on the current platform"
                  (current-continuation-marks)))]))
