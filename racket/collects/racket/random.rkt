#lang racket/base

(require "private/unix-rand.rkt" "private/windows-rand.rkt" racket/contract/base)
(provide (contract-out [crypto-random-bytes (-> exact-positive-integer? bytes?)]))

; (: crypto-random-bytes (-> Positive-Integer Bytes))
; returns n random bytes from the os.
(define (crypto-random-bytes n)
  (case (system-type 'os)
    [(unix macosx) (crypto-random-unix-bytes n)]
    [(windows) (crypto-random-windows-bytes n)]
    [else (raise (make-exn:fail:unsupported
                  "Only UNIX, OS X, and Windows XP or greater are currently supported"
                  (current-continuation-marks)))]))