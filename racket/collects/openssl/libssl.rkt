#lang racket/base
(require ffi/unsafe
         racket/runtime-path
         (for-syntax racket/base)
         "libcrypto.rkt")

(provide libssl
         libssl-load-fail-reason)

(define libssl-load-fail-reason #f)

;; We need to declare because they might be distributed with PLT Scheme
;; in which case they should get bundled with stand-alone executables:
(define-runtime-path libssl-so
  (case (system-type)
    [(windows) '(so "ssleay32")]
    [else '(so "libssl")]))

(define libssl
  (and libcrypto
       (with-handlers ([exn:fail?
                        (lambda (x)
                          (set! libssl-load-fail-reason (exn-message x))
                          #f)])
         (ffi-lib libssl-so
                  '(""
                    "1.0.1e"
                    "1.0" "1.0.0" "1.0.0e" "1.0.0d" "1.0.0c" "1.0.0b" "1.0.0a"
                    "0.9.8e" "0.9.8b" "0.9.8" "0.9.7")))))
