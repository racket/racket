#lang racket/base
(provide crypto-random-windows-bytes)

(require ffi/unsafe
         ffi/unsafe/define)

(define-ffi-definer define-advapi (and (eq? (system-type) 'windows) (ffi-lib "Advapi32.dll"))
  #:default-make-fail make-not-available)

; supposed to be the same csprng as CryptGenRand, but with less overhead
; see Microsoft security dev Michael Howard: http://blogs.msdn.com/b/michael_howard/archive/2005/01/14/353379.aspx
; this is for Windows XP and later only, but I doubt that's a problem
(define-advapi SystemFunction036 (_fun _pointer _ulong -> _bool))

; (: crypto-random-windows-bytes (-> Positive-Integer Bytes))
(define (crypto-random-windows-bytes n)
  (define rand-bytes-buf (make-bytes n))
  (if (SystemFunction036  rand-bytes-buf n)
      rand-bytes-buf
      (raise (make-exn:fail
              "crypto-random-windows: SystemFunction036 failed to generate bytes"
              (current-continuation-marks)))))
